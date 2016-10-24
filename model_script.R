#### GBIF data

library (rgbif)

head(name_lookup(query = 'Diuraphis noxia', rank="species", return = 'data'))

## not really enough data at this source.....

head(name_lookup(query = 'Harmonia axyridis', rank="species", return = 'data'))

hold.dat <- occ_search(scientificName = "Harmonia axyridis", limit = 20)

#23166 records on the 18/10/2016

key <- name_backbone(name='Harmonia axyridis')$speciesKey
# download all the records and plot (based on a ggplot function)
dat <- occ_search(taxonKey=key, return='data', limit=hold.dat$meta$count)
gbifmap(dat)

dist <- cbind (dat[,c("species", "decimalLongitude", "decimalLatitude")])

### loading raster data
library (dismo)
tmin <- getData("worldclim", var = "tmin", res = 10) 



