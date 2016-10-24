#### GBIF data

library (rgbif)
library (dismo)
library (maptools)

##First, lookup the species on GBIF using the rgbif package

head(name_lookup(query = 'Harmonia axyridis', rank="species", return = 'data'))

hold.dat <- occ_search(scientificName = "Harmonia axyridis", limit = 20)

#23166 records on the 18/10/2016

key <- name_backbone(name='Harmonia axyridis')$speciesKey
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
points (sppP[,1:2], pch=20, col="red")
