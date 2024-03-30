# us map with all the occ pts and dam locations.
library(sf)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(rgeos)
library(maptools)
library(proj4)
library(dplyr)
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
coords = c()
randomsp = ceiling(runif(20,0,length(files)))
for ( i in randomsp)#length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  file = read.csv(filename)
  coords = rbind(coords,cbind(file$decimalLongitude,file$decimalLatitude))
}
# plot us map 
us = readOGR('G:/My Drive/research/sdm_modeling/gis/2018_us_outline.shp')
ohio = readOGR('G:/My Drive/research/sdm_modeling/gis/nhd05 catchment/NHDPlusMS/NHDPlus05/05unit_boundary/Catchment_Dissolve.shp')
us_crs = proj4string(us)
proj4string(ohio)

plot(us, col=rgb(0, 0, 255, max = 255, alpha = 0, names = "blue50"))

# plot all the sample points
coords = as.data.frame(coords)
names(coords) = c('longitude','latitude')
# get rid of NAs 
coords = coords[-which(is.na(coords$longitude) | is.na(coords$latitude)),]
coordinates(coords) <- c("longitude", "latitude")
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
coords <- spTransform(coords, us_crs)


#my.sf.point <- st_as_sf(x = coords, 
#                        coords = c("longitude", "latitude"),
#                        crs = crs(us_crs))

#point_crs = st_crs(my.sf.point)


#my.sf.point2 = st_transform(my.sf.point,
#            st_crs(us_crs))

plot(coords,add=TRUE)
plot(my.sf.point,add=TRUE)


