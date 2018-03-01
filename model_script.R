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
library (reshape2)
library (rasterVis)
library (viridis)
library (corrplot)

#######################################################################

## Read in the distribution data files
dist <- read.csv("complete_dist.csv", header=T)
grains_dist <- read.csv("grains_dist.csv", header=T)

## Predictor varibles
## stacked worldclim 2.0 data in 10' resolution
bioclimall <- stack("./worldclim/bioclimall_clipped.grd")
## human influence index
hii <- raster ("hii_10min.tif")
hii <- resample (hii, bioclimall[[1]])
names(hii) <- "hii"

## Aridity layer is from CGIAR
arid <- raster("./arid.grd")
names(arid) <- "arid"
bioclimall <- stack(bioclimall, arid, hii)

##### Seeting up prediction data############
## Need the extent of Australia for defining the ranges
# Predict the model to the climate values for Australia (data.frame)
e <- extent (112, 155, -45, -10)
Australia <- crop (bioclimall, e)
#convert to a stack
Australia <- stack(unstack(Australia))

### Data cleaning
## There are number of points that seem a bit odd..
#A bounding box of x = -130, 40 and y = 20, 75 should clear them up...

austdist <- dist[dist$decimalLongitude > 112 & dist$decimalLongitude < 155, ]
austdist <- austdist[austdist$decimalLatitude > -45 & austdist$decimalLatitude < -10, ]

globdist <- dist[dist$decimalLongitude > -130 & dist$decimalLongitude < 40, ]
globdist <- globdist[globdist$decimalLatitude > 20 & globdist$decimalLatitude < 75, ]

NZdist <- dist[dist$decimalLongitude > 164 & dist$decimalLongitude < 180, ]
NZdist <- NZdist[NZdist$decimalLatitude > -55 & NZdist$decimalLatitude < -33, ]

dist <- rbind (globdist, NZdist, austdist)

#############################################
###LOAD backgroundS

inv_ras <- raster("earwigs_Australia.grd")
inv_ras <- crop (inv_ras,e)

nat_ras <- raster("earwigs_global.grd")
#nat_ras <- crop(nat_ras, c(-180, 75, 0, 100))



if (file.exists("earwigs_global_alldata.grd")){
  alldata <- raster ("earwigs_global_alldata.grd")
} else{
  temp <- resample (inv_ras, bioclimall[[1]])
  temp2 <- resample (nat_ras, bioclimall[[1]])
  alldata <-sum(stack(list(temp, temp2)), na.rm=T)
  alldata <- reclassify(alldata, c(-Inf, 0, NA))
  alldata <- trim (alldata)
  writeRaster(alldata, filename="earwigs_global_alldata.grd", overwrite=TRUE)
}

######## Perform a PCA on the presence data
code <- extract (Australia[[1]], dist[,3:2])
code[is.na(code)] <- "Global"
code[code!="Global"] <- "Australia"

grains_code <- rep("grains", nrow(grains_dist))
code <- c(code, grains_code)




pca_dat <- as.data.frame(extract (bioclimall, rbind(dist[,3:2], grains_dist[,3:2])))
pca_dat <- cbind(pca_dat, code)
pca_dat <- pca_dat[complete.cases(pca_dat),]

pca_out <- princomp(pca_dat[,1:20]) ## just climatic data

pca.dat <- as.data.frame(pca_out$scores)

pca_dat <- as.data.frame(pca_dat)


ggplot (pca.dat, aes(Comp.1, Comp.2))+
  ggConvexHull::geom_convexhull(alpha=0.3, aes(fill=pca_dat$code))+
  geom_point(aes(colour=as.factor(pca_dat$code)), alpha=1, size=2)+
  theme_minimal()

ggplot (pca.dat[pca_dat$code == "Australia" | pca_dat$code == "grains",], aes(Comp.1, Comp.2, fill=pca_dat$code[pca_dat$code == "Australia" | pca_dat$code == "grains"]))+
  ggConvexHull::geom_convexhull(alpha=0.3)+
  geom_point(aes(colour=as.factor(pca_dat$code[pca_dat$code == "Australia" | pca_dat$code == "grains"])), alpha=0.6, size=2)+
  theme_minimal()

components <-summary (pca_out)
components

# with aridity, this accounts for 99.8% variance on comp1 
#withou aridity ## Comp1 = 84.4% variance, Comp2 = 13% deviance

components$loadings
#Comp 1 = #bio04, bio12, bio13, bio16, bio17, bio18, bio19
#Comp 2 = #bio04, bio12, bio18, bio19


keep_vars <-  c("arid",
                "bio04",
                "bio12",
                "bio13",
                "bio16",
                "bio17",
                "bio18",
                "bio19",
                "hii")

### examine correlations (across training background rather than presence points)
background <- as.data.frame(randomPoints(alldata, 40000))

background.df <- as.data.frame(extract (bioclimall[[keep_vars]], background[,1:2]))
background.df <- background.df[complete.cases(background.df),]

pca_cor <- cor(background.df, method="kendall") ## kendall method takes a while compared to Pearson and Spearman...
corrplot (pca_cor, method="number")

#bio12 & bio13 correlated at 0.81
#bio12 & bio16 correlated at 0.84
#bio12 & bio17 correlated at 0.69
#bio12 & bio18 correlated at 0.69
#bio12 & bio19 correlated at 0.65

#bio13 & bio18 correlated at 0.78
#bio13 $ bio19 correlated at 0.5

#bio16 & bio18 correlated at 0.79
#bio16 & bio19 correlated at 0.52

#bio17 & bio19 correlated at 0.77

### Keeping these correlations in mind, running the models first and revising after seems best way to go...

biovar <- subset(bioclimall, keep_vars)
Australia <- subset(Australia, keep_vars)


##  create a reference raster from this, rescale the points to 1 per grid cell 
##  and create a new data.frame with this new distribution dataset.
refrast <- biovar[[1]]
sppR <- rasterize (dist[,3:2], refrast)
sppG <- rasterize (grains_dist[,3:2], refrast)

## for intial models we want to use ALL DATA
sppP <- data.frame (rbind(rasterToPoints (sppR), rasterToPoints(sppG)))

### To show now we only have 1 unique point per grid cell:
length (rbind(dist[,1], grains_dist[,1]))
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








###################################################################################################
## Niche analysis

background$pa <- 0
background.df <- as.data.frame(extract (biovar, background[,1:2]))
bgDF <- cbind (background, background.df)

background.ras <- alldata
###################################################################################################
library (dismo)
library (rmaxent) ## remember to cite this...

jar <- paste("./maxent/maxent.jar")

file.exists(jar) ## must be true!

wd <- getwd()

## use only the presence points relevant to the model background
in_or_out <- extract(background.ras, sppDF[,1:2])
pres.env <- sppDF[,4:ncol(sppDF)][which (in_or_out==1),]

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
                        "noaddsamplestobackground",  
                         "noremoveduplicates"))

## point process - climate only 

max3SWD <- maxent(x=pbg.env[,1:8], p=pbg.which, path=paste(wd,'/max3SWD',sep=""),
                  args=c("beta_threshold=-1", 
                         "-P", #response curves
                         "noautofeature", 
                         "nothreshold", 
                         "noproduct",
                         "maximumbackground=40000",
                         "noaddsamplestobackground",  
                         "noremoveduplicates"))

## Look at evaluation on the training dataset
max1SWD@results["Training.AUC",]
max2SWD@results["Training.AUC",]
max3SWD@results["Training.AUC",]
# Point process model does the better job

me1 <- project (max1SWD, biovar)
me2 <- project (max2SWD, biovar)
me3 <- project (max3SWD, biovar)

occ <- sppDF[,1:2]
colnames (occ) <- c("lon", "lat")

ic(stack(me1$prediction_raw, me2$prediction_raw, me3$prediction_raw), 
   occ, list(max1SWD, max2SWD, max3SWD))

################## Limiting factor analysis
lim1 <- limiting(Australia, max2SWD)
levelplot(lim1, col.regions=rainbow) +
  layer(sp.points(SpatialPoints(occ), pch=20, col=1))

## Examine the limiting factor analysis, and then the percent contribution and perumtation importance as determined by maxent
## also examine the correlations examined before

#bio12, bio16, could be dropped based on permutation importance
#bio13 is correlated with 18, but is slightly more important - so 18 is dropped
#bio17 is correlated with 19, but lower imporance, 17 is dropped
corrplot (pca_cor, method="number")
#update keep_vars # these are best performing, uncorrelated variables 
final_vars <-  c("arid",
                 "bio04",
                 "bio19",
                 "bio13",
                 "hii")

## point-process settings 
max4SWD <- maxent(x=pbg.env[,c(final_vars)], p=pbg.which, path=paste(wd,'/max4SWD',sep=""),
                  args=c("beta_threshold=-1", 
                         "-P", #response curves
                         "noautofeature", 
                         "nothreshold", 
                         "noproduct",
                         "maximumbackground=40000",
                         "noaddsamplestobackground",  
                         "noremoveduplicates"))

max4SWD@results["Training.AUC",]

me4 <- project (max4SWD, biovar)

ic(stack(me2$prediction_raw, me4$prediction_raw), 
   occ, list(max2SWD, max4SWD))

### the new model gives a lower AICc/

lim2 <- limiting(Australia, max4SWD)
levelplot(lim2, col.regions=rainbow) +
  layer(sp.points(SpatialPoints(occ), pch=20, col=1))


####### Plot up the two map quickly....

max2.pred <- predict(max2SWD, Australia, args="outputformat=logistic",
                     filename=paste(wd, '/max2SWD/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")

max3.pred <- predict(max3SWD, Australia, args="outputformat=logistic",
                     filename=paste(wd, '/max3SWD/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")


max4.pred <- predict(max4SWD, Australia, args="outputformat=logistic",
                     filename=paste(wd, '/max4SWD/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")

gplot(max2.pred) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10)) +
  ggtitle (paste0("Forficula auricularia Maxent (point process) SDM AUC = ", max2SWD@results["Training.AUC",]))

gplot(max3.pred) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10)) +
  ggtitle (paste0("Forficula auricularia Maxent (point process) SDM AUC = ", max3SWD@results["Training.AUC",]))

gplot(max4.pred) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10)) +
  ggtitle (paste0("Forficula auricularia Maxent (point process) SDM AUC = ", max4SWD@results["Training.AUC",]))



## point-process settings 
max5SWD <- maxent(x=pbg.env[,c(final_vars[1:4])], p=pbg.which, path=paste(wd,'/max5SWD',sep=""),
                  args=c("beta_threshold=-1", 
                         "-P", #response curves
                         "noautofeature", 
                         "nothreshold", 
                         "noproduct",
                         "maximumbackground=40000",
                         "noaddsamplestobackground",  
                         "noremoveduplicates"))

max5.pred <- predict(max5SWD, Australia, args="outputformat=logistic",
                     filename=paste(wd, '/max5SWD/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")

me5 <- project (max5SWD, biovar)

gplot(max5.pred) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10)) +
  ggtitle (paste0("Forficula auricularia Maxent (point process) SDM AUC = ", max5SWD@results["Training.AUC",]))


results <- as.data.frame(ic(stack(me1$prediction_raw, me2$prediction_raw, me3$prediction_raw, me4$prediction_raw, me5$prediction_raw), 
   occ, list(max1SWD, max2SWD, max3SWD, max4SWD, max5SWD)))
results$AUC <- NA
results$vars <- NA
results$AUC[1] <- max1SWD@results["Training.AUC",]; results$vars[1] <- paste(keep_vars, collapse=" ")
results$AUC[2] <- max2SWD@results["Training.AUC",]; results$vars[2] <- paste(keep_vars, collapse=" ")
results$AUC[3] <- max3SWD@results["Training.AUC",]; results$vars[3] <- paste(keep_vars[1:8], collapse=" ")
results$AUC[4] <- max4SWD@results["Training.AUC",]; results$vars[4] <- paste(final_vars, collapse=" ")
results$AUC[5] <- max5SWD@results["Training.AUC",]; results$vars[5] <- paste(final_vars[1:4], collapse=" ")

#### Table for paper
write.csv(results, file="./model_summ.csv")

#########################################################################################################################################
## FIGURE PLOT

sites <- data.frame("site"=NA, lat="NA", long="NA")
sites[1:3] <- c("Thoona", -36.3305, 146.0853)
sites[2, 1:3] <- c("Elmore", -36.4968, 144.6088)
sites$lat <- as.numeric(sites$lat)
sites$long <- as.numeric(sites$long)

gplot(max4.pred) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient(low = 'white', high = 'dark blue') +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1)+
  geom_path (data=wrld_simpl, aes(x=long, y=lat, group=group))+
  geom_point(data=austdist, aes(decimalLongitude, decimalLatitude), size=3, alpha=.75, shape = 21, colour = "black", fill = "light blue")+
  geom_point(data=grains_dist, aes(decimalLongitude, decimalLatitude), size=3, alpha=.75, shape = 21, colour = "black", fill = "green")+
  geom_point(data=sites, aes(long, lat), size=4, shape=21, colour="yellow", fill="red")+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  geom_path (data=worldshp, aes(x=long, y=lat, group=group))+
  #geom_text(aes(label=sites$site),hjust=0, vjust=0)+
  ggtitle (expression(paste(italic("Forficula auricularia"))))



###############################################################################################################################
## Reciprocal distribution modelling


# same as before, except now we only use the background thats everywhere but Australia
# selection of variables and model conditions is all done prior
# this is see how a subset of the distribution data can reciprocally model across
# we expect that Australia will underpredict the global distribution

background.ras <- nat_ras
background <- as.data.frame(randomPoints(background.ras, 40000))
background$pa <- 0
background.df <- as.data.frame(extract (biovar[[final_vars]], background[,1:2]))
bgDF <- cbind (background, background.df)

in_or_out <- extract(background.ras, sppDF[,1:2])
pres.env <- sppDF[,c(final_vars)][which (in_or_out==1),]

bg.env <- background.df
pbg.env <- rbind(pres.env, bg.env)
pbg.which <- c(rep(1, nrow(pres.env)), rep(0, nrow(bg.env)))

Glob2Aus <- maxent(x=pbg.env, p=pbg.which, path=paste(wd,'/Glob2Aus',sep=""),
                  args=c("beta_threshold=-1", 
                         "-P", #response curves
                         "noautofeature", 
                         "nothreshold", 
                         "noproduct",
                         "maximumbackground=40000",
                         "noaddsamplestobackground",  
                         "noremoveduplicates"))

Glob2Aus.pred <- predict(Glob2Aus, biovar, args="outputformat=logistic",
                     filename=paste(wd, '/Glob2Aus/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                     progress="text")

### Aus to Global

background.ras <- inv_ras
background <- as.data.frame(randomPoints(background.ras, 40000))
background$pa <- 0
background.df <- as.data.frame(extract (biovar[[final_vars]], background[,1:2]))
bgDF <- cbind (background, background.df)

in_or_out <- extract(background.ras, sppDF[,1:2])
pres.env <- sppDF[,c(final_vars)][which (in_or_out==1),]

bg.env <- background.df
pbg.env <- rbind(pres.env, bg.env)
pbg.which <- c(rep(1, nrow(pres.env)), rep(0, nrow(bg.env)))

Aus2Glob <- maxent(x=pbg.env, p=pbg.which, path=paste(wd,'/Aus2Glob',sep=""),
                   args=c("beta_threshold=-1", 
                          "-P", #response curves
                          "noautofeature", 
                          "nothreshold", 
                          "noproduct",
                          "maximumbackground=40000",
                          "noaddsamplestobackground",  
                          "noremoveduplicates"))

Aus2Glob.pred <- predict(Aus2Glob, biovar, args="outputformat=logistic",
                         filename=paste(wd, '/Aus2Glob/pred.asc', sep=""), format="ascii", overwrite=TRUE,
                         progress="text")

## AUC and Schoener's D testing.
## raster preparation
inv_ras <- crop (inv_ras, Australia)
inv_ras <- resample(inv_ras, biovar[[1]])
nat_ras <- resample(nat_ras, biovar[[1]])

Glob2Aus.pred1 <- trim(mask(Glob2Aus.pred, inv_ras))
Glob2Aus.pred2 <- trim(mask(Glob2Aus.pred, nat_ras))



Aus2Glob.pred1 <- trim(mask(Aus2Glob.pred, inv_ras))
Aus2Glob.pred2 <- trim(mask(Aus2Glob.pred, nat_ras))

### Global projecting to Australia:
## AUC
p <- rasterize (sppP[,1:2],Glob2Aus.pred1)
p <- mask(Glob2Aus.pred1, p)
a <- mask(Glob2Aus.pred1, p, inverse=TRUE)
p <- rasterToPoints(p)[,3]
a <- rasterToPoints(a)[,3]

evaluate (p, a)

## Schoener's D
nicheOverlap(Glob2Aus.pred1, Aus2Glob.pred1)

## ArcGIS base file
worldshp <- readShapePoly ("./worldadmin/WorldAdmin.shp") 
## Clip just to Australia
Aust <- worldshp[worldshp@data$CNTRY_NAME=="Australia",]

RDM.B <- gplot(Glob2Aus.pred1) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1, na.value = NA)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10)) +
  geom_path (data=worldshp, aes(x=long, y=lat, group=group))+
  ggtitle("Global model projected to Australia")

RDM.C <- gplot(Aus2Glob.pred1) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1, na.value = NA)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10)) +
  geom_path (data=worldshp, aes(x=long, y=lat, group=group))+
  ggtitle("Australia model projected to Australia")


##Australia projecting globally
## AUC
p <- rasterize (sppP[,1:2],Aus2Glob.pred2)
p <- mask(Aus2Glob.pred2, p)
a <- mask(Aus2Glob.pred2, p, inverse=TRUE)
p <- rasterToPoints(p)[,3]
a <- rasterToPoints(a)[,3]

evaluate (p, a)

## Schoener's D
nicheOverlap(Aus2Glob.pred2, Glob2Aus.pred2)

RDM.D <- gplot(Aus2Glob.pred2) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1, na.value = NA)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(-180, 75)) +
  scale_y_continuous(expand = c(0,0), limits= c(3.833333, 80)) +
  geom_path (data=worldshp, aes(x=long, y=lat, group=group), alpha=0.5, size=0.1)+
  ggtitle("Australia model projected to Global model")

RDM.A <- gplot(Glob2Aus.pred2) + geom_tile(aes(fill = value)) +
  scale_fill_viridis(option="magma", begin=0.0, direction=-1, na.value = NA)+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(-180, 75)) +
  scale_y_continuous(expand = c(0,0), limits= c(3.833333, 80)) +
  geom_path (data=worldshp, aes(x=long, y=lat, group=group), alpha=0.5, size=0.1)+
  ggtitle("Global model projected to Global model")



cowplot::plot_grid(RDM.A, RDM.B, RDM.D, RDM.C,
          rel_widths = c(1.75, 1))

################################################################################################################################
## Niche shift analysis

## Coding - Hypothesis 1 - Australia vs Global
locs.dat <- (sppP[,1:2])
coordinates (locs.dat) <- ~x+y
wrld_simpl@proj4string <- CRS("+proj=longlat +datum=WGS84")
locs.dat@proj4string <- CRS("+proj=longlat +datum=WGS84")

code <- extract (Australia[[1]], locs.dat)
code[is.na(code)] <- 0
code[code!=0] <- 1

sppP$code <- code

library (ecospat)

occ.sp1 <- sppDF[sppP$code==0,c("x", "y", "pa", final_vars)]
occ.sp2 <- sppDF[sppP$code==1,c("x", "y", "pa", final_vars)]

occ.sp1 <- occ.sp1[complete.cases(occ.sp1),]
occ.sp2 <- occ.sp2[complete.cases(occ.sp2),]

x1 <- randomPoints(nat_ras, 10000)
clim1 <- as.data.frame(cbind(x1,(extract(biovar[[final_vars]], x1))))
clim1 <- clim1[complete.cases(clim1),]

x2 <- randomPoints(inv_ras, 10000)
clim2 <- as.data.frame(cbind(x2,(extract(biovar[[final_vars]], x2))))
clim2 <- clim2[complete.cases(clim2),]

clim1$pa <- 0
clim2$pa <- 0

occ.sp1$code <- NULL
occ.sp2$code <- NULL

inv <- rbind (occ.sp2, clim2)
nat <- rbind (occ.sp1, clim1)

## just on climate.... hii is the last column
inv <- inv[,1:ncol(inv)-1]
nat <- nat[,1:ncol(nat)-1]



pca.env <- dudi.pca(rbind(nat,inv)[,4:ncol(nat)],scannf=F,nf=2)

ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim <- pca.env$li

scores.sp.nat <- suprow(pca.env,nat[which(nat[,3]==1),4:ncol(nat)])$li
scores.sp.inv <- suprow(pca.env,inv[which(inv[,3]==1),4:ncol(nat)])$li

scores.clim.nat <-suprow(pca.env,nat[,4:ncol(nat)])$li
scores.clim.inv <-suprow(pca.env,inv[,4:ncol(nat)])$li

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





niche.dyn <- ecospat.niche.dyn.index(grid.clim.nat, grid.clim.inv, intersection = 0.1)
niche.dyn 
ecospat.plot.niche.dyn (grid.clim.nat, grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1", name.axis2="PC2")
ecospat.shift.centroids(scores.sp.nat, scores.sp.inv, scores.clim.nat, scores.clim.inv)

## boost the reps up to 100 for final
eq.test <- ecospat.niche.equivalency.test(grid.clim.nat, grid.clim.inv,rep=10, alternative = "greater")
sim.test <- ecospat.niche.similarity.test (grid.clim.nat, grid.clim.inv, rep=10, alternative = "greater",rand.type=2)
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")


for (i in names(nat[,4:ncol(nat)])){
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

### This next part is a bit messy, due to the way the previous analyses were setup..

### Hypthoesis 2. Australia broad and Australia grains are different


sppAus <- as.data.frame(rasterToPoints(sppR))

locs.dat <- as.data.frame(sppAus[,1:2])
coordinates (locs.dat) <- ~x+y
wrld_simpl@proj4string <- CRS("+proj=longlat +datum=WGS84")
locs.dat@proj4string <- CRS("+proj=longlat +datum=WGS84")

code <- extract (Australia[[1]], locs.dat)
code[is.na(code)] <- 1
code[code!=1] <- 0

sppAus$code <- code

sppAus <- sppAus[sppAus$code == 0,]

sppGrains <- as.data.frame(rasterToPoints(sppG))
sppGrains$code <- 1

sppAus <- rbind (sppAus, sppGrains)

AusDF <- merge(sppAus, sppDF, by=c("x", "y"))

AusDF$code.y <- NULL
colnames(AusDF)[which(names(AusDF) == "code.x")] <- "code"

occ.sp1 <- AusDF[sppAus$code==0,] ## Other Australia
occ.sp2 <- AusDF[sppAus$code==1,] ## Grains

occ.sp1 <- occ.sp1[complete.cases(occ.sp1),]
occ.sp2 <- occ.sp2[complete.cases(occ.sp2),]

occ.sp1$code <- NULL
occ.sp2$code <- NULL
occ.sp1$layer <- NULL
occ.sp2$layer <- NULL




x1 <- randomPoints(Australia, 10000)
clim1 <- as.data.frame(cbind(x1,(extract(biovar, x1))))
clim1 <- clim1[complete.cases(clim1),]

x2 <- randomPoints(Australia, 10000)
clim2 <- as.data.frame(cbind(x2,(extract(biovar, x2))))
clim2 <- clim2[complete.cases(clim2),]

clim1$pa <- 0
clim2$pa <- 0

#clim1 <- clim1[,c(1,2,11,3,4,5,6,7,8, 9, 10, 12)]
#clim2 <- clim2[,c(1,2,11,3,4,5,6,7,8, 9, 10, 12)]

#clim1 <- clim1[,c(1,2,11,5,6,7,9,12)]

inv <- rbind (occ.sp2, clim2)
nat <- rbind (occ.sp1, clim1)

## just on climate....
inv <- inv[,1:ncol(inv)-1]
nat <- nat[,1:ncol(nat)-1]

#inv <- inv[,c(1, 2, 3, 6, 7, 8, 10, 12)]
#nat <- nat[,c(1, 2, 3, 6, 7, 8, 10, 12)]

pca.env <- dudi.pca(rbind(nat,inv)[,4:ncol(nat)],scannf=F,nf=2)

ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim <- pca.env$li

scores.sp.nat <- suprow(pca.env,nat[which(nat[,3]==1),4:ncol(nat)])$li
scores.sp.inv <- suprow(pca.env,inv[which(inv[,3]==1),4:ncol(nat)])$li

scores.clim.nat <-suprow(pca.env,nat[,4:ncol(nat)])$li
scores.clim.inv <-suprow(pca.env,inv[,4:ncol(nat)])$li

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
niche.dyn

ecospat.plot.niche.dyn (grid.clim.nat, grid.clim.inv, quant=0.25, interest=2, title= "Niche Overlap", name.axis1="PC1", name.axis2="PC2")
ecospat.shift.centroids(scores.sp.nat, scores.sp.inv, scores.clim.nat, scores.clim.inv)



##### look at null model and associations with HII and rainfall

null_spp <- randomPoints(background.ras, nrow(sppDF))
null_sppDF <- as.data.frame(extract(biovar, null_spp))
null_sppDF <- cbind (null_spp, null_sppDF)
null_sppDF$species <- "Null"

sppDF2 <- sppDF
sppDF2$pa <-NULL
sppDF2$species <- "Actual"

dens_data <- rbind (sppDF2, null_sppDF)

dens_data <- melt(dens_data, id.vars = c("x", "y", "species"))

ggplot (dens_data)+
  geom_density(aes(x=value, fill=species), alpha=0.5)+
  facet_wrap(~variable, scales="free")

ggplot (dens_data[dens_data$variable=="hii_10min",])+
  geom_density(aes(x=value, fill=species), alpha=0.5)

wetMap <- bioclimall[[12]]
wetMap <- crop (wetMap, Australia[[1]])
wetMap <- reclassify (wetMap, c(-Inf, 400, NA, 400, Inf, 1))

gplot(wetMap) + geom_tile(aes(fill = value), alpha=0.8) +
  scale_fill_gradient(low = 'white', high = 'dark blue') +
  #scale_fill_viridis(option="magma", begin=0.0, direction=1)+
  geom_path (data=wrld_simpl, aes(x=long, y=lat, group=group))+
  coord_equal()+
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  geom_point (data=this.dat.abs , aes (decimalLongitude, decimalLatitude), colour="blue", size=2)+
  geom_point (data=this.dat.pres , aes (decimalLongitude, decimalLatitude), colour="red", size=2)+
  # geom_point (data=sites, aes (long, lat), colour="green", size=2)+
  #geom_text(aes(label=sites$site),hjust=0, vjust=0)+
  ggtitle (paste0("Association with rainfaill (400 mm)"))
