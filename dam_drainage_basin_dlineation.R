# resource: https://rdrr.io/github/markwh/streamstats/f/README.md (from ryan mcmanamay's email)
# first section is downloading drainage basin with the point where the damlies
# second section looks at which dams had error and failed at downloading drainage basin data
# third section tries to snap the failed dams' points to a nhd flowline and tries to download the basin data again.
# fourth section tries to get the drainage basin using nhdplustools by including tributary flowline as well
# (the total catchment areas of all the tributaries and the delineation from the streamstats match pretty well based on trying an example dam.)
# section 1.
#install.packages("remotes")
#remotes::install_github("markwh/streamstats")
library(streamstats)
library(lwgeom)
require(dams)
library(data.table)
library(nhdplusTools)
library(sf)
library(maptools)

nid_subset$nidid
#get all the dams in my sdm dataset
nidids = c()
for( i in 1:18)
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  spproj = fread(path1)
  nidids = c(nidids,unique(spproj$udd2hilarri_nidid))
}
nidids= unique(nidids)
nidids= nidids[-which(is.na(nidids))]

idx = apply(as.matrix(nidids),1,function(x) which(nid_subset$nidid ==x))
nid_subset$nidid[idx[[286]]]

for( i in 1:length(idx))
{
  if(length(idx[[i]])>1)
  {
    #print(i)
    #print(nid_subset[idx[[i]],c(4,5)])
    #readline()
    idx[[i]] = idx[[i]][1] # only take the first dam.
  }
}

idx = unlist(idx)
idx = idx[which(!is.na(idx))]
featured_dams = nid_subset[idx,]

a = which(nidids %in% nid_subset$nidid)
b = which(!(nidids %in% nid_subset$nidid)) # dams in sdm not featured in NID.
length(a)
length(b)
nidids[b]
# add in the dam data for the sdm data dams that are un-featured in the NID manually.
unfeatured_dams = data.frame()
unfeatured_dams = rbind(unfeatured_dams,c('ME83016',955,44.5792,-69.5552))
unfeatured_dams = rbind(unfeatured_dams,c('ME00054',60,43.3946205,-70.4436353))
unfeatured_dams = rbind(unfeatured_dams,c('ME96105',126,44.285,-70.5371))
unfeatured_dams = rbind(unfeatured_dams,c('ME83030',711,43.497637, -70.451265))
unfeatured_dams = rbind(unfeatured_dams,c('GA00599',207,34.076389, -83.804167))
unfeatured_dams = rbind(unfeatured_dams,c('WI00726',2640,46.746699, -91.485091))
unfeatured_dams = rbind(unfeatured_dams,c('WI00136',140,43.687772, -89.047921))
unfeatured_dams = rbind(unfeatured_dams,c('WI00288',180,44.494025, -89.311200))
unfeatured_dams = rbind(unfeatured_dams,c('WI00284',120,43.472253, -87.991488))
unfeatured_dams = rbind(unfeatured_dams,c('NC05713',1400,35.4646, -82.5442))
unfeatured_dams = rbind(unfeatured_dams,c('WI00036',740,46.237210, -91.783966))
unfeatured_dams = rbind(unfeatured_dams,c('WI00056',710,45.761023, -91.220270))
unfeatured_dams = rbind(unfeatured_dams,c('WI00184',670,42.843677, -89.172776))
unfeatured_dams = rbind(unfeatured_dams,c('WI00219',200,43.586347, -90.644188))
unfeatured_dams = rbind(unfeatured_dams,c('WI00301',95,43.458664, -89.714883))
unfeatured_dams = rbind(unfeatured_dams,c('WI00181',500,43.223567, -90.466741))
unfeatured_dams = rbind(unfeatured_dams,c('WI00302',200,43.466794, -89.742323))
unfeatured_dams = rbind(unfeatured_dams,c('WI00149',1000,45.192620, -89.688284))
unfeatured_dams = rbind(unfeatured_dams,c('CA10411',165,36.9837,	-120.5))
names(unfeatured_dams)= c('nidids','max_storage_acreft','lat','long')
head(unfeatured_dams)


# get all the coordinates from featured and unfeatured.
all_coords  = data.frame(nidid=c(featured_dams$nidid,unfeatured_dams$nidids),long=c(featured_dams$longitude,unfeatured_dams$long),
                         lat=c(featured_dams$latitude,unfeatured_dams$lat))
dim(all_coords)

# download shapefile of the drainage basin delineation with the dam locations as pour points.
erroridx = c()
for(i in 1219:nrow(all_coords))
{
  delineation <- try({
    delineateWatershed(xlocation = as.numeric(all_coords$long[i]), ylocation = as.numeric(all_coords$lat[i]), crs = 4326,
                       includeparameters = "false")
  })
  
  if(!('try-error' %in% class(delineation)))
  {
    dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files'
    list.files(dir)
    if(!any(grepl(all_coords$nidid[i],list.files(dir)))) # if the file is not already written
    {
      writeShapefile(watershed = delineation, layer = sprintf('%s',all_coords$nidid[i]), dir = dir, what = "boundary")      
    } else
    {
      print('file already written')
    }
  } else
  {
    erroridx = c(erroridx,i)
    print(sprintf('error in %d',i))
  }
  print(i)    
}



# section 2.
# see which dams' drainage basin didn't get downloaded
dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files'
files = list.files(dir)
shpfiles = files[grepl('.shp',files)]
nrow(all_coords)-length(shpfiles)

good_dams = gsub('.shp','',shpfiles)
bad_dams_idx = which(!(all_coords$nidid %in% good_dams))



# section 3.
#trouble shooting
# result of the trouble shooting by trying the delineation function with snapped points instead (5/30/2023):
# did not work..lol

# find the nhdcomid for the points and get the flowlines
# snap the points to the flowline

filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
hilarri_baddam = hilarri[which(hilarri$nididfull %in% all_coords$nidid[bad_dams_idx]),]
hilarri_baddam$nididfull[1:4]
baddamcomid = hilarri_baddam$comid

for( i in 1:length(baddamcomid))
{
  subset_file <- tempfile(fileext = ".gpkg")
  subset <- subset_nhdplus(comids = baddamcomid[i],
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = TRUE,
                           return_data = TRUE, overwrite = TRUE)
  flowline = subset$NHDFlowline_Network
  flowline = sf::st_geometry(flowline)
  flowline = as_Spatial(flowline)
  flowline_crs = proj4string(flowline)
  point = data.frame(longitude=hilarri_baddam$longitude[i],latitude=hilarri_baddam$latitude[i])
  coordinates(point) <- c("longitude", "latitude")
  proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
  point <- spTransform(point, flowline_crs)
  cut_dist = 200 # max distance
  newpoint <- snapPointsToLines(point, flowline, maxDist = cut_dist)
  
  #plot(flowline)
  #plot(point,add=TRUE)
  #plot(newpoint,col='red',add=TRUE)
  coords <- st_coordinates(st_as_sf(newpoint))
  delineation <- try({
    delineateWatershed(xlocation = as.numeric(coords[1]), ylocation = as.numeric(coords[2]), crs = 4326,
                       includeparameters = "false")
  })
  
  if(!('try-error' %in% class(delineation)))
  {
    dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files'
    list.files(dir)
    if(!any(grepl(hilarri_baddam$nididfull[i],list.files(dir)))) # if the file is not already written
    {
      writeShapefile(watershed = delineation, layer = sprintf('%s',hilarri_baddam$nididfull[i]), dir = dir, what = "boundary")      
      print(sprintf('%d/%d done',i,length(baddamcomid)))
    } else
    {
      print('file already written')
    }
  } else
  {
    erroridx = c(erroridx,i)
    print(sprintf('error in %d',i))
  }
}



# section 4. try to get the drainage basin using nhdplustools by including tributary flowline as well
# * since 6/2/2023, I found out the method using the streamstat package to get drainage basin can be garbage (literally gives me traingle or rectange as a basin sometimes...)
# and getting it through nhd seems like the better method (gives me better looking drainage basin on dams that gave me triangles with streamstat (e.g.,ME00237).)
# going to get all the drainage basin using nhd.
filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
#hilarri_baddam = hilarri[which(hilarri$nididfull %in% all_coords$nidid[bad_dams_idx]),]
#baddamcomid = hilarri_baddam$comid
#comid = hilarri$comid[which(hilarri$nididfull=='ME00237')]
dir = dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files'
files = list.files(dir)
files = files[which(!grepl('nhd',files))]
files = files[which(grepl('.shp',files))]
ids = gsub('.shp','',files)
idx = apply(as.matrix(ids),1,function(x) which(hilarri$nididfull==x))
comids = hilarri$comid[idx]


for( i in 1386:1387)#1:length(baddamcomid))
{
  # find upstream tributaries
  #comid = baddamcomid[i]
  comid = comids[i]
  flowlines <- navigate_nldi(list(featureSource = "comid", 
                                  featureID = as.numeric(comid)), 
                             mode = "UM", 
                             distance_km = 5000)
  tflowlines<- navigate_nldi(list(featureSource = "comid", 
                                  featureID = as.numeric(comid)), 
                             mode = "UT", 
                             distance_km = 5000)
  
  umcomids = as.numeric(flowlines$UM_flowlines$nhdplus_comid)
  utcomids = as.numeric(tflowlines$UT_flowlines$nhdplus_comid)
  # get the catchment areas for the tributaries and merge them together
  subset_file <- tempfile(fileext = ".gpkg")
  if(length(utcomids)>500)
  {
    for( j in 375:ceiling(length(utcomids)/500))
    {
      interval = 1:500+500*(j-1)
      if(j==ceiling(length(utcomids)/500))
      {
        interval = (500*(j-1)+1):length(utcomids)
      }
      tsubset <- subset_nhdplus(comids = utcomids[interval],
                                  output_file = subset_file,
                                  nhdplus_data = "download", 
                                  flowline_only = FALSE,
                                  return_data = TRUE, overwrite = TRUE)
      geom = tsubset$CatchmentSP$geometry
      if(is.null(geom))
      {
        print('catchment download error!')
        tsubset <- subset_nhdplus(comids = utcomids[interval],
                                  output_file = subset_file,
                                  nhdplus_data = "download", 
                                  flowline_only = FALSE,
                                  return_data = TRUE, overwrite = TRUE)
        geom = tsubset$CatchmentSP$geometry
      }
      print('download complete')
      if(j==1)
      {
        total_catchment = st_union(geom)          
        if(any(st_is_valid(total_catchment)==FALSE))
        {
          total_catchment = st_make_valid(total_catchment)
        }

      } else 
      {
        total_catchment_temp <- try({
          st_union(total_catchment, st_union(geom))},silent=TRUE)
        if('try-error' %in% class(total_catchment_temp))
        {
          print('try-error activated!')
          sf_use_s2(FALSE)
          total_catchment = st_union(total_catchment, st_union(geom))
          sf_use_s2(TRUE)
          print('fixed!')
        } else {
          total_catchment = total_catchment_temp
        }
        if(any(st_is_valid(total_catchment)==FALSE))
        {
          total_catchment = st_make_valid(total_catchment)
        }
      }
      total_catchment = st_union(total_catchment)
      if(any(st_is_valid(total_catchment)==FALSE))
      {
        total_catchment = st_make_valid(total_catchment)
      }
      #tempofilename = sprintf('C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/%s_nhd_temp_loop%d.shp',hilarri_baddam$nididfull[i],j)
      tempofilename = sprintf('C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/%s_nhd_temp_loop%d.shp',ids[i],j)
      st_write(total_catchment,tempofilename)
      print(sprintf('%d/%d done',j,ceiling(length(utcomids)/500)))
    }
    dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files'
  } else {
    tsubset <- subset_nhdplus(comids = utcomids,
                              output_file = subset_file,
                              nhdplus_data = "download", 
                              flowline_only = FALSE,
                              return_data = TRUE, overwrite = TRUE)
    total_catchment = st_union(tsubset$CatchmentSP$geometry)
    if(any(st_is_valid(total_catchment)==FALSE))
    {
      st_make_valid(total_catchment)
    }
  }
  
  # save the polygon into shapefile
  #filename = sprintf('C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/%s_nhd.shp',hilarri_baddam$nididfull[i])
  filename = sprintf('C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/%s_nhd.shp',ids[i])
  st_write(total_catchment,filename)
  # erase all the temporary shapefiles
  if(length(utcomids)>500)
  {
    dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files'
    tempfiles = list.files(dir)
    #tempfiles = tempfiles[which(grepl(sprintf('%s_nhd_temp_loop',hilarri_baddam$nididfull[i]),tempfiles))]
    tempfiles = tempfiles[which(grepl(sprintf('%s_nhd_temp_loop',ids[i]),tempfiles))]
    tempfiles = paste(dir,'/',tempfiles,sep='')
    unlink(tempfiles)
  }
  print(i)
}




 # play codes
#install.packages('lwgeom')
ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326,
                          includeparameters = "true")

# leafletWatershed(ws1)

ws1$featurecollection[[1]]$feature$features


# TRY IT on my dams

ws2 <- delineateWatershed(xlocation = featured_dams[1,]$longitude, ylocation = featured_dams[1,]$latitude, crs = 4326,
                          includeparameters = "true")

dir = 'G:/My Drive/research/sdm_modeling/dam data/testdam_shp'

writeShapefile(watershed = ws2, layer = "layer_name", dir = dir, what = "boundary")

leafletWatershed(ws2)

us = st_read('G:/My Drive/research/sdm_modeling/dam data/testdam_shp/layer_name.shp')
plot(us$geometry)
st_area(st_as_sf(us$geometry))
which(is.na(featured_dams$latitude))


