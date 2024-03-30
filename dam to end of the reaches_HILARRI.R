# snap points (dam locations) to a line (reach segment) and get the distances from the snapped point to the ends of the line
# (upstream and downstream end of the reach from the dam location.)
library(nhdplusTools)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(proj4)
library(dplyr)
library(lwgeom)
# resource for snapping points to lines (used the maptool solution: Using maptools::snapPointsToLines): https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

# getting flowline and point data.

filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
comids = hilarri$comid
points = cbind(hilarri$longitude,hilarri$latitude)
nonacomids = which(!is.na(hilarri$comid))

distdata = as.data.frame(cbind(hilarri$comid,NA,NA))
names(distdata) = c('comid','u_length_km','d_length_km')
weird_partl = c() # record weird part lengths
nonhddata = c()
for( i in nonacomids)
{
  comid = comids[i]
  rawpoint = points[i,]
  UM <- try({
    navigate_nldi(list(featureSource = "comid", 
                                featureID = comid), 
                           mode = "UM", 
                           distance_km = 1)
  },silent = FALSE)
  if('try-error' %in% class(UM))
  {
    nonhddata = c(nonhddata,i)
    next
  }
    
  
  if(!is.null(UM$origin$geometry))
  {
    flowline = UM$origin$geometry
  } else if (!is.null(UM$geometry))
  {
    flowline = UM$geometry
  } else
  {
    nonhddata = c(nonhddata,i)
    next
  }
  
  flowline = sf::st_geometry(flowline)
  #flowline = as_Spatial(flowline)
  
  
  # spatial object type processing
  flowline_crs = proj4string(as_Spatial(flowline))
  point = data.frame(longitude=rawpoint[1],latitude=rawpoint[2])
  coordinates(point) <- c("longitude", "latitude")
  proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
  point <- spTransform(point, flowline_crs)
  point = st_as_sf(point)
  # snap points
  nrst = st_nearest_points(point, flowline)
  on_line = st_cast(nrst, "POINT")[2]
  
  # splitting the line with the snapped point using the buffer on a point WORKS
  buf <- st_buffer(on_line,0.1)
  parts = st_collection_extract(lwgeom::st_split(flowline, buf),"LINESTRING")
  part1 = parts[1]
  if(length(parts)==2)
  {
    part2 = parts[2]
  } else if (length(parts)==3) {
    part2 = parts[3]
  }
  if(length(parts)!=3)
  {
    print(sprintf('length of parts is %d',length(parts)))
    if(length(parts)>3)
    {
      part1 = parts[1]
      part2 = parts[length(parts)]
      weird_partl = c(weird_partl,i)
    }
  }
  
  # determine which of the split flowline is the upper stream.
  u_end = st_coordinates(flowline)[1,1:2] 
  d_end = st_coordinates(flowline)[nrow(st_coordinates(flowline)),1:2] # last row is downstream end.
  
  if(sum(apply(st_coordinates(part1)[,1:2],1, function(x) sum(x==u_end)))) # first part is upstream
  {
    u_length = as.numeric(st_length(part1))
  } else if (sum(apply(st_coordinates(part1)[,1:2],1, function(x) sum(x==d_end)))) # first part is downstream
  {
    d_length = as.numeric(st_length(part1))
  } else {
    print('part 1 is neither up nor downstream..')
  }
  
  if(sum(apply(st_coordinates(part2)[,1:2],1, function(x) sum(x==u_end)))) # first part is upstream
  {
    u_length = as.numeric(st_length(part2))
  } else if (sum(apply(st_coordinates(part2)[,1:2],1, function(x) sum(x==d_end)))) # first part is downstream
  {
    d_length = as.numeric(st_length(part2))
  } else {
    print('part 1 is neither up nor downstream..')
  }
  distdata[i,2] = u_length/1000
  distdata[i,3] = d_length/1000 # convert to km
  print(sprintf('%d/%d done',which(nonacomids==i),length(nonacomids)))  
}
newdata = cbind(hilarri,distdata$u_length_km,distdata$d_length_km)
names(newdata)[c((ncol(newdata)-1),ncol(newdata))] = c('u_length_km','d_length_km')

write.csv(newdata,filename,row.names=FALSE)
head(newdata)





















# turning the point and flowline data into sp data and consistent crs for plotting.
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = comid,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = TRUE,
                         return_data = TRUE, overwrite = TRUE)

flowline = subset$NHDFlowline_Network
flowline = sf::st_geometry(flowline)
flowline = as_Spatial(flowline)


#sp
flowline_crs = proj4string(as_Spatial(flowline))
point = data.frame(longitude=rawpoint[1],latitude=rawpoint[2])
coordinates(point) <- c("longitude", "latitude")
proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
point <- spTransform(point, flowline_crs)

#st
#point = st_as_sf(point)
#point = st_transform(point,st_crs(flowline_crs))

plot(flowline)
plot(point,add=TRUE)


# sapping point to the line (st version) DOESNT WORK
#site_snap = st_snap(st_transform(point, 3857), st_transform(flowline,3857), tol=200)
#site_snap = st_transform(site_snap, st_crs(flowline_crs))
#plot(site_snap,col='blue',add=TRUE,pch=2)

# snapping point to the line (st version2) WORKS
# res: https://stackoverflow.com/questions/55519152/split-line-by-multiple-points-using-sf-package
nrst = st_nearest_points(point, flowline)
on_line = st_cast(nrst, "POINT")[2]

# splitting the line with the snapped point using the buffer on a point WORKS
buf <- st_buffer(on_line,0.1)
parts = st_collection_extract(lwgeom::st_split(flowline, buf),"LINESTRING")
plot(on_line,add=TRUE)
plot(parts[1],col='red',add=TRUE)
plot(parts[3],col='blue',add=TRUE)
if(length(parts)!=3)
{
  print(sprintf('length of parts is %d',length(parts)))
}
st_length(parts[1])
st_length(parts[3])



# sp version DOESNT WORK
#cut_dist = 200 # max distance
#newpoint <- snapPointsToLines(point, flowline, maxDist = cut_dist)
#newpoint
#plot(newpoint,col='red',add=TRUE)

# splitting the flowline into two from the snapped point. DOESNT WORK
#sf.flowline = st_as_sf(flowline)  
#sf.newpoint = st_as_sf(newpoint)

#plot(st_geometry(sf.flowline))
#plot(st_geometry(sf.newpoint),add=TRUE,col='red')
#parts = st_collection_extract(lwgeom::st_split(st_geometry(sf.flowline), sf.newpoint),"LINESTRING")
#plot(parts)

# getting the distances from the point to the ends of the lines.
# X potential resource: https://gis.stackexchange.com/questions/292684/is-there-a-way-to-calculate-length-of-linestring-from-one-point-to-another
# X https://www.google.com/search?q=split+linestrings+by+point+r&rlz=1C1CHBF_enUS788KR789&ei=kXOXYo7xL5PFmAXUoLzQCw&ved=0ahUKEwjO_cy5t4z4AhWTIqYKHVQQD7oQ4dUDCA4&uact=5&oq=split+linestrings+by+point+r&gs_lcp=Cgdnd3Mtd2l6EAMyBggAEB4QFjoHCAAQRxCwAzoFCAAQkQI6CwguEIAEEMcBENEDOgUIABCABDoFCC4QgAQ6DgguEIAEEMcBENEDENQCOgQIABBDOggILhCABBDUAjoFCC4QkQI6CAguENQCEJECOgUIIRCgAToICAAQHhAPEBZKBAhBGABKBAhGGABQiQ9Yjj1g_D1oCHABeACAAa0BiAGuI5IBBDAuMzKYAQCgAQHIAQjAAQE&sclient=gws-wiz
# V https://stackoverflow.com/questions/55519152/split-line-by-multiple-points-using-sf-package


# find out which end of the reach is upstream
 
u_end = st_coordinates(flowline)[1,1:2] # last row is downstream end.
d_end = st_coordinates(flowline)[nrow(st_coordinates(flowline)),1:2]

if(sum(apply(st_coordinates(parts[1])[,1:2],1, function(x) sum(x==u_end)))) # first part is upstream
{
  u_length = as.numeric(st_length(parts[1]))
} else if (sum(apply(st_coordinates(parts[1])[,1:2],1, function(x) sum(x==d_end)))) # first part is downstream
{
  d_length = as.numeric(st_length(parts[1]))
} else {
  print('part 1 is neither up nor downstream..')
}

if(sum(apply(st_coordinates(parts[3])[,1:2],1, function(x) sum(x==u_end)))) # first part is upstream
{
  u_length = as.numeric(st_length(parts[3]))
} else if (sum(apply(st_coordinates(parts[3])[,1:2],1, function(x) sum(x==d_end)))) # first part is downstream
{
  d_length = as.numeric(st_length(parts[3]))
} else {
  print('part 1 is neither up nor downstream..')
}




sum(apply(st_coordinates(parts[3])[,1:2],1, function(x) sum(x==d_end)))

st_geometry(flowline)
st_coordinates()