##Mapping

## typical packages I load for GIS in R
library (raster)
library (maptools)
library (rgeos)
library (dismo)
library (rgdal)
library (ggplot2)

library (ALA4R)

### Fish species

spp <- "cyprinus carpio" # European carp
spp <- "salmo trutta" # Brown trout
spp <- "salmo salar" # Atlantic salmon
spp <- "salvelinus fontinalis"
spp <- "oncorhynchus mykiss"
spp <- "carassius auratus"
spp <- "misgurnus anguillicaudatus"
spp <- "perca fluviatilis"
spp <- "tinca tinca"
spp <- "rutilus rutilus"
spp <- "gambusia holbrooki"

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

