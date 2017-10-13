##Mapping

## 1. Map current distributions of the invasive fish species in the MDB
## 2. Predictor selection methods
## 3. Environmental overlap / niche similarity between species
## 4. JSDM methods to model co-occurence interactions 
## Pollock et al. 2014 Methods Ecology and Evolution 5,397–406
## Leach et al. 2017  Journal of Mammalogy 98, 1434–1442


## typical packages I load for GIS in R
library (raster)
library (maptools)
library (rgeos)
library (dismo)
library (rgdal)
library (ggplot2)

library (ALA4R)

## ArcGIS base file
worldshp <- readShapePoly ("./worldadmin/WorldAdmin.shp") 


## Clip just to Australia
Aust <- worldshp[worldshp@data$CNTRY_NAME=="Australia",]

### Fish species
### Preliminary list from Linterman's "Fishes of the Murray-Darling Basin"
spp <- "cyprinus carpio" # European carp
spp <- "salmo trutta" # Brown trout
spp <- "salmo salar" # Atlantic salmon
spp <- "salvelinus fontinalis" # Brook char
spp <- "oncorhynchus mykiss" # Rainbow trout
spp <- "carassius auratus" # Goldfish
spp <- "misgurnus anguillicaudatus" # Oriental weatherloach
spp <- "perca fluviatilis" # Redfin perch 
spp <- "tinca tinca" # Tench
spp <- "rutilus rutilus" # Roach
spp <- "gambusia holbrooki" #Mosquitofish / Gambusia

#Other fish
spp <- "amniataba percoides" #Banded grunter - only in southern QLD would it come into contact with carp

### ALA data

y <- occurrences(taxon=paste(spp),fields=c("latitude","longitude"),download_reason_id=10)

ala_dist <- data.frame(cbind("species" = paste(spp), y$data[, c("latitude", "longitude")]))
ala_dist <- ala_dist[complete.cases(ala_dist),]
#colnames (ala_dist) <- c("species",  "decimalLatitude", "decimalLongitude")




## Species distribution points (XY)

points <-ala_dist[,c("latitude", "longitude")]
names (points) <- c("dd_lat", "dd_long")

### Plotting part
map1 <- ggplot()+
 # geom_polygon(data=Aust_koppen, aes(x=long, y=lat, group=group, fill=code)) +
  scale_x_continuous(expand = c(0,0), limits= c(112, 155)) +
  scale_y_continuous(expand = c(0,0), limits= c(-45, -10))+
  scale_fill_manual(values=cols.X)+
  labs(x = "Longitude", y="Latitude") +
  geom_path (data=Aust, aes(x=long, y=lat, group=group))+
  #geom_point (data=points, aes(x=dd_long, y=dd_lat),size=2, colour="black", alpha =1)+
  geom_point (data=points, aes(x=dd_long, y=dd_lat),size=1.5, colour="red", alpha =.06)+
  coord_equal()+
  ggtitle (paste(spp))
map1

