#get all the flowline comids where the occurrence probability will be projected to for each species.

#1. get all the flowlines 
# option1: restricting projection areas to certain HUC units (05 for example)
# read in the nhd flowline to huc unit cross walk data.

hucunits = 1:18
for (i in hucunits)
{
  filename = 'G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/CrosswalkTable_NHDplus_HU12.csv'
  data = read.csv(filename)
  data = data[which(data$HUC_12>0),] # exclude sink IDs
  data = data[which(data$FEATUREID>0),] # exclude flowlines that doesn't have a wbd match up.
  data$huc2digit = floor(data$HUC_12/(10^10)) # has all 18 digits
  huc2constraint = i
  projareacomid = data$FEATUREID[which(data$huc2digit==huc2constraint)] #all the comids in the constrained area (ex. huc 05 unit)
  namecode = sprintf('%d',i)
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%s.csv',namecode)
  write.csv(projareacomid,ofilename)
}


## standalone code to figure out how big the file is for each huc unitF
hucunits = 1:18
numflowline = c()
for (i in hucunits)
{
  namecode = sprintf('%d',i)
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%s.csv',namecode)
  d = read.csv(ofilename)
  numflowline[i]=nrow(d)
}
numflowline/numflowline[5]*(24+19)/24



#2. get all the midpoints of the flowlines from 1
### get midpoints of all the flowlines in the projection area
# resource: https://rdrr.io/cran/maptools/man/SpatialLinesMidPoints.html
# get all the flowlines first.
midpoint <- function(start_comid)
{
  flowline <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid), 
                            mode = "DM", 
                            distance_km = 1)
  if(class(flowline$origin$geometry)=="NULL")
  {
    out <- c(NA,NA)
  } else {
    g <- st_geometry(st_as_sf(flowline$origin$geometry))
    
    g_mids <- lapply(g, function(x) {
      
      coords <- as.matrix(x)
      
      # this is just a copypaste of View(maptools:::getMidpoints):
      get_mids <- function (coords) {
        dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
        dist_mid <- sum(dist)/2
        dist_cum <- c(0, cumsum(dist))
        end_index <- which(dist_cum > dist_mid)[1]
        start_index <- end_index - 1
        start <- coords[start_index, ]
        end <- coords[end_index, ]
        dist_remaining <- dist_mid - dist_cum[start_index]
        mid <- start + (end - start) * (dist_remaining/dist[start_index])
        return(mid)
      }
      
      mids <- st_point(get_mids(coords))
    })
    
    out <- st_sfc(g_mids, crs = st_crs(st_as_sf(flowline$origin$geometry)))
    out <- st_sf(out)
  }
}

library(sf)
library(sp)
library(maptools)
library(nhdplusTools)
library(pbapply)
for( i in 7)
{
  huccode = i
  ifilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',huccode)
  d = read.csv(ifilename)[,2]
  # get the midpoints using apply.
  coords = pbapply(as.matrix(d),1,midpoint)
  coords = as.data.frame(t(matrix(unlist(coords),nrow=2)))
  colnames(coords) = c("decimalLongitude", "decimalLatitude")
  coords = cbind(d,coords)
  #points = sfheaders::sf_point(coords)
  
  ofilename = ifilename
  write.csv(coords,ofilename,row.names=FALSE)
}






# function where the midpoint function is derived from. soure link: https://gis.stackexchange.com/questions/277219/sf-equivalent-of-r-maptools-packages-spatiallinesmidpoints
st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    # this is just a copypaste of View(maptools:::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- st_point(get_mids(coords))
  })
  
  out <- st_sfc(g_mids, crs = st_crs(sf_lines))
  out <- st_sf(out)
}

#seine = flowline$origin$geometry
#seine_sf <- st_as_sf(seine)
#seine_sf_mids <- st_line_midpoints(seine_sf)

#plot(seine_sf)
#plot(seine_sf_mids, add = TRUE)






# just play codes. don't worry about em.
# confirm all the flowlines are in the constraint area
library(parallel)
library(MASS)
library(raster)
library(rgdal)
library(foreach)
library(pbapply)
filename = 'G:/My Drive/research/sdm_modeling/gis/nhd05 catchment/NHDPlusMS/NHDPlus05/05unit_boundary/Catchment_Dissolve.shp'
ohiobasin = readOGR(filename) #epsg=4269
ohiobasin = st_as_sf(ohiobasin)
ohiobasin = st_transform(ohiobasin,4326)
ofilename = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc05.csv'
d = read.csv(ofilename)[,2]
#start_comid = d[2,2]
#start_comid = d[1,2]
#flowline <- navigate_nldi(list(featureSource = "comid", 
#                               featureID = start_comid), 
#                          mode = "UT", 
#                          distance_km = 1)

#flowlines2 = flowline$origin$geometry
#ptm = proc.time()
#inside = 0
intersection <- function(start_comid)
{
  flowline <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid), 
                            mode = "DM", 
                            distance_km = 1)
  unlist(st_intersects(flowline$origin$geometry, ohiobasin))
}


result = pbapply(as.matrix(d),1,intersection) # which flowlines are inside the restricted area
#result = pbapply(as.matrix(c(d[1,2],start_comid)),1,intersection)
insidenum = sum(unlist(result))
insidenum/length(d)




for(i in 2:10000)
{
  start_comid = d[i,2]
  flowline <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid), 
                            mode = "DM", 
                            distance_km = 1)
  
  
  flowlines2[i] = flowline$origin$geometry
  
}

inside = inside + unlist(st_intersects(flowline$origin$geometry, ohiobasin))





proc.time() - ptm

plot(ohiobasin)
for(i in 1:length(flowlines2))
{
  plot(flowlines2[i],add=TRUE,lwd=5)
}


