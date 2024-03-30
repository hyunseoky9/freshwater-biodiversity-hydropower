library(proj4)
library(nhdplusTools)
library(sf)
require(dams)
library(nhdplusTools)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(rgeos)
library(maptools)
library(proj4)
library(dplyr)

get_split_catchment
i=1
{
dam  = nid_subset[which(nid_subset$nidid==nidids[i]),]
dam$max_storage
coords = data.frame(dam$longitude,dam$latitude)
names(coords) = c('longitude','latitude')
# get rid of NAs 
point <- st_as_sf(x = coords, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

shape = get_split_catchment(point,upstream=TRUE)



plot(shape$geometry[1],add=TRUE,col='red')

plot(point,add=TRUE,col='blue')



hilarrifilename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(hilarrifilename)
comid = hilarri$comid[which(hilarri$nididfull==nidids[i])]

flowlines <- navigate_nldi(list(featureSource = "comid", 
                                featureID = as.numeric(comid)), 
                           mode = "UM", 
                           distance_km = 5000)
umcomids = as.numeric(flowlines$UM_flowlines$nhdplus_comid)
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = umcomids,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)


plot(subset$CatchmentSP$geometry,add=TRUE)
plot(point,add=TRUE,col='red')
plot(subset$NHDFlowline_Network$geometry,add=TRUE,col='red')


# snap dam point to the comid flowline
subset <- subset_nhdplus(comids = comid,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
flowline = subset$NHDFlowline_Network
flowline = sf::st_geometry(flowline)
flowline = as_Spatial(flowline)
flowline_crs = proj4string(flowline)
point2 = data.frame(longitude=dam$longitude,latitude=dam$latitude)
coordinates(point2) <- c("longitude", "latitude")
proj4string(point2) <- CRS("+proj=longlat +datum=WGS84")

point2 <- spTransform(point2, flowline_crs)
cut_dist = 200 # max distance
newpoint <- snapPointsToLines(point2, flowline, maxDist = cut_dist)
plot(subset$CatchmentSP$geometry,add=TRUE,border='red')
plot(subset$NHDFlowline_Network$geometry,add=TRUE,col='red')
plot(newpoint,add=TRUE,col='red')

shape = get_split_catchment(st_as_sf(point2),upstream=TRUE)
plot(shape$geometry[1],)


print('using get_split_catchment')
print(st_area(shape$geometry[1]))
print('catchment area of the coresponding comid using subset_nhdplus')
print(subset$CatchmentSP$areasqkm)
plot(subset$NHDFlowline_Network$geometry,add=TRUE,col='red')
}



nidids = c()
ehaids = c()
eiaids = c()
for( i in 1:18)
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  spproj = fread(path1)
  ehaids = c(ehaids,unique(spproj$ud_ehaID))
  nidids = c(nidids,unique(spproj$udd2hilarri_nidid))
}
nidids= unique(nidids)
nidids= nidids[-which(is.na(nidids))]
idx = apply(as.matrix(nidids),1,function(x) which(nid_subset$nidid==x))
for( i in 1:length(idx))
{
  if(length(idx[[i]])>=2)
  {
    idx[[i]] = idx[[i]][1]
  }
}
idx =unlist(idx)
damloc = nid_subset[idx,c('longitude','latitude')]
write.csv(damloc,'damlocations.csv',row.names=FALSE)
