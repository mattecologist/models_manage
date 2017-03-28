#### GBIF data

library (rgbif)
library (rgeos)
library (dismo)
library (maptools)
library (maxnet)
##First, lookup the species on GBIF using the rgbif package

head(name_lookup(query = 'Forficula auricularia', rank="species", return = 'data'))

hold.dat <- occ_search(scientificName = "Forficula auricularia", limit = 20)

#7862 records on the 27/3/2017

key <- name_backbone(name='Forficula auricularia')$speciesKey
# download all the records and plot (based on a ggplot function)
dat <- occ_search(taxonKey=key, return='data', limit=hold.dat$meta$count)
gbifmap(dat)

dist <- cbind (dat[,c("species",  "decimalLatitude", "decimalLongitude")])

### loading raster data (example here is the worldclim data)
### about a 10 mb download at this resolution for the world.
biovar <- getData("worldclim", var = "bio", res = 10) 

##  create a reference raster from this, rescale the points to 1 per grid cell 
##  and create a new data.frame with this new distribution dataset.
refrast <- biovar$bio1
sppR <- rasterize (dist[,3:2], refrast)
sppP <- data.frame (rasterToPoints (sppR))

### To show now we only have 1 unique point per grid cell:
length (dist[,1])
length (sppP[,1])

## load the worldmap from the maptools package
data("wrld_simpl")

## for the native range we can basically say that region "142" contains the points we are after
plot (wrld_simpl[wrld_simpl$REGION==142,])

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

#### Maxent
p = a vector of presence (1) and absence (0) data 



#plotting map (from rworldmap)
mapDevice() #create world map shaped window
mapParams <- mapGriddedData(sGDF ,catMethod='categorical',addLegend=FALSE)
#adding formatted legend
do.call(addMapLegendBoxes,c(mapParams,cex=0.8,ncol=10,x='bottom',title='Koeppen-Geiger Climate Zones'))




