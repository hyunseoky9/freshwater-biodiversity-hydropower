library(rgeos)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(dplyr)
library(raster)

#'Filter georeferenced records by shapefile
#'
#' @param shapefile A shapefile object (.shp)
#' @param occurrence_df A data frame of georeferenced records. For large csv use fread()
#' @param lat The column name for latitude in the data frame
#' @param lon The column name for longitude in the data frame
#' @param gbifid A unique record key
#' @param map_crs The datum assigned to the occurrence data frame
#' @param mkplot Whether to draw the map plots. Can be very expensive
occurrence.from.shapefile <- function(shapefile, occurrence_df, lat, lon, gbifid, map_crs = "+proj=longlat +datum=WGS84", mkplot = FALSE){
  
  shape <- readOGR(shapefile)
  print(proj4string(shape))
  
  #subset the GBIF data into a data frame
  occ.map <- data.frame(occurrence_df[[lon]], occurrence_df[[lat]], occurrence_df[[gbifid]])
  print(str(occ.map, 1))
  #simplify column names
  names(occ.map)[1:3] <- c('long', 'lat', 'gbifID')
  print(head(occ.map, 10))
  #turning the data frame into a "spatial points data frame"
  coordinates(occ.map) <- c("long", "lat")
  #defining the datum 
  proj4string(occ.map) <- CRS(map_crs)
  #reprojecting the 'gbif' data frame to the same as in the 'shape' object 
  occ.map <- spTransform(occ.map, proj4string(shape))
  
  #Identifying records from gbif that fall within the shape polygons
  inside <- occ.map[apply(gIntersects(occ.map, shape, byid = TRUE), 2, any),]
  if(mkplot){
    raster::plot(shape)
    points(inside, col = "olivedrab3")
  }  
  
  #Prepare data frame for joining with the occcurrence df so only records 
  #that fall inside the polygons get selected 
  res.gbif <- data.frame(inside@data)
  final.gbif <- gbif %>% semi_join(res.gbif, by = c(gbifID = "gbifID"))
  
  return(final.gbif)
}  


