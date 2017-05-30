library (ALA4R)

ala_record <- function(species){
  y <- occurrences(taxon=paste(species),fields=c("latitude","longitude"),download_reason_id=10)
  ala_dist <- data.frame(cbind("species" = paste(species), y$data[, c("latitude", "longitude")]))
  ala_dist <- ala_dist[complete.cases(ala_dist),]
  colnames (ala_dist) <- c("species",  "decimalLatitude", "decimalLongitude")
  return (ala_dist)
}

spp_list <- c("Milax gagates",
              "Deroceras reticulatum",
              "Deroceras invadens",
              "Steriphus diversipes",
              "Mandalotus", ##remove ".spp"
              "Labidura truncata",
              "Ommatoiulus moreletii")

all_records <- data.frame ("species" =NA, "decimalLatitude"=NA, "decimalLongitude"=NA)

for (spp in spp_list){
    X <- ala_record (paste(spp))
    all_records <- rbind (all_records, X)
    
}

table (all_records$species)
