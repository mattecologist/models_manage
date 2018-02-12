#### Matt Hill

## Still to do:
## 1. Variable selection pre-modelling
## 2. Variable response curves
## 3. Add in Australian collection data (ALA and APPD added - add our data or use to test...)
## 4. Niche overlap between Australian and global datasets (if of interest)

library (rgeos)
library (dismo)
library (maptools)
library (ggplot2)
#### GBIF data
library (rgbif)
library (rJava)

#######################################################################
update_dist=FALSE

if (update_dist == FALSE){
  dist <- read.csv("complete_dist.csv", header=T)
} else{


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
y <- occurrences(taxon="forficula auricularia",fields=c("latitude","longitude", "basis_of_record"),download_reason_id=10)

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

#appd2 <- appd[,c("Scientific.Name", "Latitude...original", "Longitude...original", "Data.Resource.Name")] 

## Other data
#### NOT for redistribution at this point

projdata <- read.csv("./data/european earwig_Sarina.csv", header=T)
proj_dist <- data.frame(cbind("species" = "Forficula auricularia", projdata[, c("lat", "long")]))
colnames (proj_dist) <- c("species",  "decimalLatitude", "decimalLongitude")
dist <- rbind (dist, proj_dist)



write.csv (dist, file="complete_dist.csv", row.names = FALSE)
}

####### Add in project data
## note: for Vic and SA, we haven't got the field recorded lat-longs yet.

this.dat <- read.csv("./data/european_earwig_grdc.csv", header=T)

this.dat.pres <- data.frame(cbind("Species" = "Forficula auricularia", this.dat[,c("LATITUDE", "LONGITUDE")][this.dat$Present == 1,]))
colnames (this.dat.pres) <- c("species",  "decimalLatitude", "decimalLongitude") 

this.dat.abs <- data.frame(cbind("Species" = "Forficula auricularia", this.dat[,c("LATITUDE", "LONGITUDE")][this.dat$Present == 0,]))
colnames (this.dat.abs) <- c("species",  "decimalLatitude", "decimalLongitude") 

dist <- rbind (dist, this.dat.pres)


## From the 2017 Quarrell et al paper
quarrell_data <- read.csv ("./data/european_earwig_Quarrell.csv", header=T)
quarrell_dist <- data.frame(cbind("species" = "Forficula auricularia", quarrell_data[, c("Latitude", "Longitude")]))
colnames (quarrell_dist) <- c("species",  "decimalLatitude", "decimalLongitude")


### Begin analysis

### loading raster data (example here is the worldclim data)
### about a 10 mb download at this resolution for the world.
#all_biovar <- getData("worldclim", var = "bio", res = 10) 

bioclimall <- stack("./worldclim/bioclimall_clipped.grd")

## subset to most important variables. will expand on this section eventually with methodology.
biovar <- subset(bioclimall, c("bio02",
                               "bio03",
                               "bio05",
                               "bio06",
                               "bio07",
                               "bio13",
                               "bio14",
                               "bio15"))
## human influence index
hii <- raster ("hii_10min.tif")
hii <- resample (hii, biovar[[1]])

#biovar <- stack(biovar, hii)

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

##### Seeting up prediction data############
## Need the extent of Australia for defining the ranges
# Predict the model to the climate values for Australia (data.frame)
e <- extent (112, 155, -45, -10)
Australia <- crop (biovar, e)
#convert to a stack
Australia <- stack(unstack(Australia))
#############################################


######## Perform a PCA on the presence data
locs.dat <- (sppP[,1:2])
coordinates (locs.dat) <- ~x+y
wrld_simpl@proj4string <- CRS("+proj=longlat +datum=WGS84")
locs.dat@proj4string <- CRS("+proj=longlat +datum=WGS84")

code <- extract (Australia[[1]], sppP[,1:2])
code[is.na(code)] <- 0
code[code!=0] <- 1

pca_dat <- extract (biovar, sppP[,1:2])
pca_dat <- cbind(pca_dat, code)
pca_dat <- pca_dat[complete.cases(pca_dat),]
pca_out <- princomp(pca_dat[,1:8])

pca.dat <- as.data.frame(pca_out$scores)

pca_dat <- as.data.frame(pca_dat)


#relevel(pca_dat[,9]) 2 = Africa, 9 = Australia, 19 = Americas, 142 = Asia, 150 = Europe, 

ggplot (pca.dat, aes(Comp.1, Comp.2))+
  geom_point(aes(colour=as.factor(pca_dat$code)))+
  theme_minimal()


ggplot (pca.dat, aes(Comp.1, Comp.3))+
  geom_point(aes(colour=as.factor(pca_dat$code)))+
  theme_minimal()

ggplot (pca.dat, aes(Comp.2, Comp.3))+
  geom_point(aes(colour=as.factor(pca_dat$code)))+
  theme_minimal()


###################################################################################################
## Niche analysis

## Coding
sppP$code <- code

## check if the background files exist, if not, will go ahead and make them using the "background_builder" function

if (file.exists("earwigs_Australia.grd")){
  inv_ras <- raster("earwigs_Australia.grd")
} else{
  terres <- readShapePoly("./Shapes/terrestrial/wwf_terr_ecos_2.shp")
  invasive <- data.frame (sppP[sppP$code==1,])
  colnames(invasive)[2] <- "Latitude"
  colnames(invasive)[1] <- "Longitude"
  inv_ras <- background_builder(invasive, terres, wrld_simpl, ref_rast = biovar[[1]])
  inv_ras <- crop (inv_ras, e)
  writeRaster(inv_ras, filename = "earwigs_Australia.grd", overwrite=TRUE)
}

if (file.exists("earwigs_global.grd")){
  nat_ras <- raster("earwigs_global.grd")
} else{
  terres <- readShapePoly("./Shapes/terrestrial/wwf_terr_ecos_2.shp")
  native <- data.frame (sppP[sppP$code==0,])
  colnames(native)[2] <- "Latitude"
  colnames(native)[1] <- "Longitude"
  nat_ras <- background_builder(native, terres, wrld_simpl, ref_rast = biovar[[1]])
  temp <- resample (Australia[[1]], nat_ras)
  nat_ras <- mask (nat_ras, temp, inverse=TRUE)
  writeRaster(nat_ras, filename = "earwigs_global.grd", overwrite=TRUE)
}

if (file.exists("earwigs_global_alldata.grd")){
  alldata <- raster ("earwigs_global_alldata.grd")
} else{
  temp <- resample (inv_ras, nat_ras)
  alldata <-sum(stack(list(nat_ras, temp)), na.rm=T)
  alldata <- reclassify(alldata, c(-Inf, 0, NA))
  writeRaster(alldata, filename="earwigs_global_alldata.grd", overwrite=TRUE)
}

background.ras <- alldata

background <- as.data.frame(randomPoints(background.ras, 40000))

background$pa <- 0
background.df <- as.data.frame(extract (biovar, background[,1:2]))
bgDF <- cbind (background, background.df)


## Make modelling data.frame
#forficula <- rbind (sppDF, bgDF)
#forficula <- forficula[complete.cases(forficula),]
#forficula <- na.omit (forficula)

locs <- sppDF[,1:2]

#varsstack <- crop(biovar, nat_ras)
#varsstack <- mask(varsstack, nat_ras)

###################################################################################################
library (dismo)

jar <- paste("./maxent/maxent.jar")



file.exists(jar) ## must be true!

wd <- getwd()

## This vway of implementing maxent from the Renner et al. paper seems too restrictive.
# max1 <- maxent(x=varsstack, p=locs, path=paste(wd,'/max1',sep=""),
#               args=c("-P", "noautofeature", "nothreshold", "noproduct", 
#               "maximumbackground=400000","noaddsamplestobackground","noremoveduplicates"))
# 
# max1.pred <- predict(max1, Australia, args="outputformat=raw",
#               filename=paste(wd, '/max1/pred.asc', sep=""), format="ascii",
#               progress="text", overwrite=TRUE)
# 
# max2.pred <- predict(max1, biovar, args="outputformat=raw",
#                      filename=paste(wd, '/max1/pred2.asc', sep=""), format="ascii",
#                      progress="text")

# #Start with a default map
# max2 <- maxent(x=varsstack, p=locs, path=paste(wd,'/max2',sep=""),
#                args=c("beta_threshold=-1", 
#                       "-P",  # response curves
#                       "noautofeature", 
#                       "nothreshold", 
#                       "noproduct",
#                       "maximumbackground=40000",
#                       "noaddsamplestobackground",  
#                       "noremoveduplicates"))
# 
# max2.pred <- predict(max2, Australia, args="outputformat=raw",
#                      filename=paste(wd, '/max2/pred.asc', sep=""), format="ascii",
#                      progress="text", overwrite=TRUE)


## use only the presence points relevant to the model background

in_or_out <- extract(background.ras, sppDF[,1:2])
pres.env <- sppDF[,4:11][which (in_or_out==1),]

bg.env <- background.df
pbg.env <- rbind(pres.env, bg.env)
pbg.which <- c(rep(1, nrow(pres.env)), rep(0, nrow(bg.env)))

#default settings

max1SWD <- maxent(x=pbg.env, p=pbg.which, path=paste(wd,'/max1SWD',sep=""),
                  args=c("beta_threshold=-1", 
                         "-P"))

## point-process settings 
max2SWD <- maxent(x=pbg.env, p=pbg.which, path=paste(wd,'/max2SWD',sep=""),
                  args=c("beta_threshold=-1", 
                        "-P", #response curves
                        "noautofeature", 
                        "nothreshold", 
                        "noproduct",
                        "maximumbackground=40000",
                        "noaddsamplestobackground",  ## this is not working!
                         "noremoveduplicates"))

max1.pred <- predict(max1SWD, Australia, args="outputformat=logistic",
                    filename=paste(wd, '/max1SWD/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")

max2.pred <- predict(max2SWD, Australia, args="outputformat=logistic",
                     filename=paste(wd, '/max2SWD/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")

####### Plot up the map quickly....
library (rasterVis)
library (viridis)
#map <- myBiomodProj@proj@val[[1]]

sites <- data.frame("site"=NA, lat="NA", long="NA")
sites[1:3] <- c("Thoona", -36.3305, 146.0853)
sites[2, 1:3] <- c("Elmore", -36.4968, 144.6088)
sites$lat <- as.numeric(sites$lat)
sites$long <- as.numeric(sites$long)

gplot(max2.pred) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient(low = 'white', high = 'dark blue') +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  geom_path (data=wrld_simpl, aes(x=long, y=lat, group=group))+
  geom_point(data=dist, aes(decimalLongitude, decimalLatitude), colour="black", alpha=0.6, size=2)+
  coord_equal()+
    scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
    scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  geom_point (data=sites, aes (long, lat), colour="green", size=2)+
  geom_point (data=quarrell_dist , aes (decimalLongitude, decimalLatitude), colour="blue", size=2)+
  #geom_text(aes(label=sites$site),hjust=0, vjust=0)+
  ggtitle (paste0("Forficula auricularia Maxent (point process) SDM AUC = ", max2SWD@results["Training.AUC",]))

gplot(max1.pred) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient(low = 'white', high = 'dark blue') +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  geom_path (data=wrld_simpl, aes(x=long, y=lat, group=group))+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  geom_point (data=this.dat.abs , aes (decimalLongitude, decimalLatitude), colour="blue", size=2)+
  geom_point (data=this.dat.pres , aes (decimalLongitude, decimalLatitude), colour="green", size=2)+
  #geom_text(aes(label=sites$site),hjust=0, vjust=0)+
  ggtitle (paste0("Forficula auricularia Maxent (default settings) SDM AUC = ", max1SWD@results["Training.AUC",]))

################################################################################################################################
## Niche shift analysis

library (ecospat)

occ.sp1 <- sppDF[sppP$code==0,]
occ.sp2 <- sppDF[sppP$code==1,]

occ.sp1 <- occ.sp1[complete.cases(occ.sp1),]
occ.sp2 <- occ.sp2[complete.cases(occ.sp2),]

x1 <- randomPoints(nat_ras, 10000)
clim1 <- as.data.frame(cbind(x1,(extract(biovar, x1))))
clim1 <- clim1[complete.cases(clim1),]

x2 <- randomPoints(inv_ras, 10000)
clim2 <- as.data.frame(cbind(x2,(extract(biovar, x2))))
clim2 <- clim2[complete.cases(clim2),]

clim1$pa <- 0
clim2$pa <- 0

clim1 <- clim1[,c(1,2,11,3,4,5,6,7,8, 9, 10)]
clim2 <- clim2[,c(1,2,11,3,4,5,6,7,8, 9, 10)]

inv <- rbind (occ.sp2, clim2)
nat <- rbind (occ.sp1, clim1)


pca.env <- dudi.pca(rbind(nat,inv)[,4:11],scannf=F,nf=2)

ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim <- pca.env$li

scores.sp.nat <- suprow(pca.env,nat[which(nat[,3]==1),4:11])$li
scores.sp.inv <- suprow(pca.env,inv[which(inv[,3]==1),4:11])$li

scores.clim.nat <-suprow(pca.env,nat[,4:11])$li
scores.clim.inv <-suprow(pca.env,inv[,4:11])$li

grid.clim.nat <- ecospat.grid.clim.dyn (glob=scores.globclim,
  glob1=scores.clim.nat,
  sp=scores.sp.nat, R=100,
  th.sp=0)

grid.clim.inv <- ecospat.grid.clim.dyn(glob=scores.globclim,
  glob1=scores.clim.inv,
  sp=scores.sp.inv, R=100,
  th.sp=0)

D.overlap <- ecospat.niche.overlap(grid.clim.nat, grid.clim.inv, cor=T)$D
D.overlap


## boost the reps up to 100 for final
eq.test <- ecospat.niche.equivalency.test(grid.clim.nat, grid.clim.inv,rep=10, alternative = "greater")
sim.test <- ecospat.niche.similarity.test (grid.clim.nat, grid.clim.inv, rep=10, alternative = "greater",rand.type=2)
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")


niche.dyn <- ecospat.niche.dyn.index(grid.clim.nat, grid.clim.inv, intersection = 0.1)

ecospat.plot.niche.dyn (grid.clim.nat, grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1", name.axis2="PC2")
ecospat.shift.centroids(scores.sp.nat, scores.sp.inv, scores.clim.nat, scores.clim.inv)




for (i in names(nat[,4:11])){
# gridding the native niche
grid.clim.t.nat <-ecospat.grid.clim.dyn(glob=as.data.frame(rbind(nat,inv)[,i]),
                                        glob1=as.data.frame(nat[,i]),sp=as.data.frame(nat[which(nat[,3]==1),i]), R=1000, th.sp=0)
# gridding the invaded niche
grid.clim.t.inv <-ecospat.grid.clim.dyn(glob=as.data.frame(rbind(nat,inv)[,i]),
                                        glob1=as.data.frame(inv[,i]),sp=as.data.frame(inv[which(inv[,3]==1),i]), R=1000, th.sp=0)

t.dyn<-ecospat.niche.dyn.index(grid.clim.t.nat, grid.clim.t.inv,intersection=0.1)
ecospat.plot.niche.dyn(grid.clim.t.nat, grid.clim.t.inv, quant=0, interest=2, title= "Niche Overlap",
                       name.axis1=paste(names(nat[i])))
}



