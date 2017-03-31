#### Matt Hill

## Still to do:
## 1. Variable selection pre-modelling
## 2. Variable response curves
## 3. Add in Australian collection data (ALA and APPD added - add our data or use to test...)
## 4. Niche overlap between Australian and global datasets (if of interest)

library (rgeos)
library (dismo)
library (maptools)

#### GBIF data
library (rgbif)
##First, lookup the species on GBIF using the rgbif package
head(name_lookup(query = 'Forficula auricularia', rank="species", return = 'data'))
hold.dat <- occ_search(scientificName = "Forficula auricularia", limit = 20)

#7862 records on the 27/3/2017

key <- name_backbone(name='Forficula auricularia')$speciesKey
# download all the records and plot (based on a ggplot function)
dat <- occ_search(taxonKey=key, return='data', limit=hold.dat$meta$count)
gbifmap(dat)

dist <- cbind (dat[,c("species",  "decimalLatitude", "decimalLongitude")])

### ALA data
library (ALA4R)
y <- occurrences(taxon="forficula auricularia",fields=c("latitude","longitude"),download_reason_id=10)

ala_dist <- data.frame(cbind("species" = "Forficula auricularia", y$data[, c("latitude", "longitude")]))
ala_dist <- ala_dist[complete.cases(ala_dist),]
colnames (ala_dist) <- c("species",  "decimalLatitude", "decimalLongitude")

#### add both together
dist <- rbind (dist, ala_dist)

### APPD data
#### Note: this comes from a closed database and I had to gain permission to use this data - not for redistribution.
#### Skip this if you don't have these data.

appd <- read.csv("./data/Forficula auricularia_290317.csv", header=T)
appd_dist <- appd[,c("Scientific.Name", "Latitude...original", "Longitude...original")]
colnames (appd_dist) <- c("species",  "decimalLatitude", "decimalLongitude")
dist <- rbind (dist, appd_dist)

## Other data
#### NOT for redistribution at this point

projdata <- read.csv("./data/european earwig_Sarina.csv", header=T)
proj_dist <- data.frame(cbind("species" = "Forficula auricularia", projdata[, c("lat", "long")]))
colnames (proj_dist) <- c("species",  "decimalLatitude", "decimalLongitude")
dist <- rbind (dist, proj_dist)

### loading raster data (example here is the worldclim data)
### about a 10 mb download at this resolution for the world.
all_biovar <- getData("worldclim", var = "bio", res = 10) 

## subset to most important variables. will expand on this section eventually with methodology.
biovar <- subset(all_biovar, c("bio2","bio3","bio5","bio6","bio7","bio13","bio14","bio15"))

##  create a reference raster from this, rescale the points to 1 per grid cell 
##  and create a new data.frame with this new distribution dataset.
refrast <- biovar[[1]]
sppR <- rasterize (dist[,3:2], refrast)
sppP <- data.frame (rasterToPoints (sppR))

### To show now we only have 1 unique point per grid cell:
length (dist[,1])
length (sppP[,1])

## load the worldmap from the maptools package
data("wrld_simpl")

# Plot the shapefile and points to see how it looks...
plot (wrld_simpl)
points (sppP[,1:2], pch=20, col="red")

######## Create the species data frame
sppP$pa <- 1
clim.df <- as.data.frame(extract (biovar, sppP[,1:2]))
sppDF <- cbind (sppP, clim.df)
sppDF$layer <- NULL

######## Create the background data (using Koppen-Geiger data from online)
temp <- tempfile()
download.file("http://koeppen-geiger.vu-wien.ac.at/data/Koeppen-Geiger-ASCII.zip",temp)
dF <- read.table(unz(temp, "Koeppen-Geiger-ASCII.txt"), header=TRUE,as.is=TRUE)
unlink(temp)

#convert to sp SpatialPointsDataFrame
coordinates(dF) = c("Lon", "Lat")
# promote to SpatialPixelsDataFrame
gridded(dF) <- TRUE
# promote to SpatialGridDataFrame
sGDF = as(dF, "SpatialGridDataFrame")

#convert to spatial polygon
koppen_poly <-  as(sGDF, "SpatialPolygonsDataFrame") 

#determine unique koppen zones
globdis <- sppP
coordinates(globdis) <- ~x+y

koppen <- na.exclude (over (globdis, koppen_poly))
kopp_unique <- unique(koppen$Cls)
globdis_poly <- koppen_poly[koppen_poly$Cls %in% kopp_unique, ]

#break down Koppen polygons
Kpoly <- gUnaryUnion(globdis_poly)
projection(Kpoly) <- CRS('+proj=longlat')

#sample 10000 random cells across backgrounds 
background <- data.frame (spsample (Kpoly, 10000, type="random"))
background$pa <- 0
background.df <- as.data.frame(extract (biovar, background[,1:2]))
bgDF <- cbind (background, background.df)

## Make modelling data.frame
forficula <- rbind (sppDF, bgDF)
forficula <- forficula[complete.cases(forficula),]
forficula <- na.omit (forficula)

##### Seeting up prediction data.
# Predict the model to the climate values for Australia (data.frame)
e <- extent (112, 155, -45, -10)
Australia <- crop (biovar, e)
#convert to a stack
Australia <- stack(unstack(Australia))

###################################################################################################
## Using BIOMOD for now - might update to the Maxnet package at some point...

library (biomod2)
###################################################################################################
##GLOBAL MODELLING OPTIONS
###################################################################################################
myBiomodOption <- BIOMOD_ModelingOptions(
  MAXENT.Phillips = list( path_to_maxent.jar = "./maxent/maxent.jar",
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = FALSE,
                 quadratic = FALSE,
                 product = FALSE,
                 threshold = FALSE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = 5,#-1, #numeric (default -1.0), regularization parameter to be applied 
                 beta_categorical = -1,  #to all linear, quadratic and product features; negative value 
                 beta_lqp = -1,          #enables automatic setting
                 beta_hinge = 2,
                 defaultprevalence = 0.5))


###################################################################################################
## GLOBAL DISTRIBUTION MODELLING
###################################################################################################
myRespName <-"Forficula auricularia"

myResp <- forficula$pa
myExpl <- forficula[,4:ncol(forficula)]
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = forficula[,c("x", "y")],
                                     resp.name = myRespName)


myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('MAXENT.Phillips'), #just Maxent at this stage
  models.options = myBiomodOption,
  NbRunEval=1, # how many runs to do = normally 10, but here is 1
  DataSplit=100, # data split =100. normally 70 for training, 30 for testing
  Yweights=NULL,
  Prevalence=0.5,
  VarImport=10,
  models.eval.meth = c('ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE)


# examine evaluation metrics
eval<- get_evaluations(myBiomodModelOut)


#projections

myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                  new.env = Australia,
                                  proj.name = 'curr_nat',
                                  #xy.new.env = cruclim,
                                  selected.models = 'all',
                                  #binary.meth = c('TSS', 'ROC'),
                                  filtered.meth = NULL,
                                  compress = 'gzip',
                                  clamping.mask = T,
                                  do.stack=T)



####### Plot up the map quickly....
library (rasterVis)
map <- myBiomodProj@proj@val[[1]]

gplot(map) + geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = 'white', high = 'dark blue') +
  geom_path (data=wrld_simpl, aes(x=long, y=lat, group=group))+
  geom_point(data=dist, aes(decimalLongitude, decimalLatitude), colour="red", alpha=0.6)+
  coord_equal()+
    scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
    scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  ggtitle ("Forficula auricularia Maxent SDM v1")

all_vals <- extract (map, dist[,2:3])

LTE10 <- quantile (all_vals, .1, na.rm=T)[[1]]

map2 <- reclassify (map, c(-Inf, LTE10, 0, LTE10, Inf, 1))

gplot(map2) + geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = 'white', high = 'dark blue') +
  geom_path (data=wrld_simpl, aes(x=long, y=lat, group=group))+
  geom_point(data=dist, aes(decimalLongitude, decimalLatitude), colour="red", alpha=0.6)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  ggtitle ("Forficula auricularia Maxent SDM v1")

### Ensemble modelling (if multiple algorithms)

myEnsemble <- BIOMOD_EnsembleModeling (modeling.output =myBiomodModelOut, 
                                       chosen.models='all', 
                                       em.by = 'all',
                                       eval.metric=c('TSS','ROC'),
                                       eval.metric.quality.threshold=c(0.5, 0.7), ##this TSS score is low, but sometimes shit is broke
                                       models.eval.meth = c('TSS', 'ROC'), 
                                       prob.mean = FALSE,
                                       prob.ci.alpha = 0.05,
                                       committee.averaging = FALSE,
                                       prob.mean.weight = TRUE,
                                       prob.mean.weight.decay = 'proportional')

ensembleBiomodProj <- BIOMOD_EnsembleForecasting(EM.output=myEnsemble,
                                                 projection.output=myBiomodProj)

  






