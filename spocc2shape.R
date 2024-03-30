#export occurrence data as shape file.
library(rgeos)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(dplyr)
library(raster)
library(sf)
dir = 'C:/Users/hy324/Google_Drive/research/SDM modeling/gis/sp_shapefiles'
setwd(dir)
species = read.csv("sp_list.csv")
species = species$x
dir2 = 'C:/Users/hy324/Google_Drive/research/SDM modeling/spdata/per_sp'
#setwd('C:/Users/hy324/Google_Drive/research/SDM modeling/gis/brown_trout_occurence_data')
for(i in 1:length(species))
{
  filename= sprintf('%s/%s%s',dir2,species[i],'.csv')
  #filename = 'sp_occurrence.csv'
  gbif_pre <- fread(filename, sep = "\t", header = TRUE, na.strings = "\\N")
  if(i==1){
    gbif = gbif_pre
  } else{
    gbif = rbind(gbif,gbif_pre)
  }
}
gbif$occurrenceStatus[gbif$occurrenceStatus=="PRESENT"] = 1
gbif$occurrenceStatus[gbif$occurrenceStatus=="ABSENT"] = 0
gbif$occurrenceStatus = as.numeric(gbif$occurrenceStatus)
setwd('C:/Users/hy324/Google_Drive/research/SDM modeling/gis/nhd05 catchment/NHDPlusMS/NHDPlus05/NHDPlusCatchment')
shpfile <- "Catchment_Dissolve.shp"

rr <- occurrence.from.shapefile2(shpfile, gbif, 'decimalLatitude', 'decimalLongitude', 'gbifID', 'species','occurrenceStatus', mkplot = FALSE)
rr = st_as_sf(rr)
setwd('C:/Users/hy324/Google_Drive/research/SDM modeling/gis/sp_shapefiles')
#writefilename = sprintf('occurrence.shp',species[i])
st_write(rr, "species_occ.shp")
