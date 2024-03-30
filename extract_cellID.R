# extract cellIDs from the cellID raster from WBM for sp occ pts and other pts where sdm will project presence prob to.
# read in the target species data and get its coordinates
#install.packages('sf')
#install.packages('rgeos')
#install.packages('maptools')
#install.packages('proj4')
#install.packages('data.table')
#install.packages('rgdal')
#install.packages('dplyr')
#install.packages('raster')


library(sf)
library(rgeos)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(dplyr)
library(raster)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spnamedata = read.csv('./comprehensive_sp_info.csv')



ohio = 0 # area confinement to ohio
if(ohio)
{
  cellID = raster('G:/My Drive/research/sdm_modeling/gis/wbm/ohio_basin/nc/CONUS_Masks_HydroSTN30_30sec_Static_OhioBasin.tif')
} else {
  cellID = raster('G:/My Drive/research/sdm_modeling/gis/wbm/CONUS_Masks_HydroSTN30_30sec_Static.tif')
}
# below 2 lines are not in if(ohio) because we need the crs of the shapefile for 
# all occpts and projection areas in and outside of the ohio basin.
filename = 'G:/My Drive/research/sdm_modeling/gis/nhd05 catchment/NHDPlusMS/NHDPlus05/05unit_boundary/Catchment_Dissolve.shp'
ohiobasin = readOGR(filename) #epsg=4269

occ_or_proj= 0 #1= sp occurrene pts. 0= projection area
if(occ_or_proj)
{
  len = nrow(spnamedata)
} else {
  len = 18
}

for( i in 1:len)
{
  # **spdata could mean species occurrence points or projection area.
  
  if(occ_or_proj)
  {
    #i = which(spnamedata$name=='Aplodinotus grunniens') # for example species Aplodontis grunniens (freshwater darter). use the code with for loop later
    spname = spnamedata$name[i]
    spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
    spdata = read.csv(spfilename)  
  } else{
    hucnum = i #5
    projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",hucnum)
    spdata = read.csv(projfilename)
  }
  
  # resource: https://gis.stackexchange.com/questions/222978/lon-lat-to-simple-features-sfg-and-sfc-in-r
  spcoords_sf = st_as_sf(spdata, coords = c("decimalLongitude", "decimalLatitude"), 
                         crs = 4326, agr = "constant")
  # resource: https://campus.datacamp.com/courses/spatial-analysis-with-sf-and-raster-in-r/preparing-layers-for-spatial-analysis?ex=10
  spcoords_sp = as(spcoords_sf,Class='Spatial')
  
  #restrict the points' spatial extent (if you need to. e.g. restrict to ohio basin)
  # resource: https://www.r-bloggers.com/2014/07/clipping-spatial-data-in-r/
  
  spcoords_sp = spTransform(spcoords_sp, CRS(proj4string(ohiobasin))) # make projections and coordinate system consistent
  # check the plots to make sure the points and the extent map looks consistent with each other
  #plot(ohiobasin)
  #points(spcoords_sp)
  
  if(ohio)
  {
    spcoords_subset <- spcoords_sp[ohiobasin, ]  
  } else {
    spcoords_subset <- spcoords_sp
  }
  
  # check if the subsetting worked
  #plot(ohiobasin)
  #points(spcoords_subset)
  #### restrict the extent of the points for other areas as well.
  
  #get the cellids for each of those points.
  #resource: https://www.gisremotesensing.com/2012/10/extract-raster-values-from-points.html
  # read in the cellID raster
  
  spcoords_cellID = extract(cellID,spcoords_subset)
  
  if(ohio)
  {
    spdata$wbmIDOhio = rep(NA,nrow(spdata))
    spdata$wbmIDOhio[which(!is.na(match(spdata$comid,spcoords_subset$comid)))] = spcoords_cellID
  } else {
    spdata$wbmID = rep(NA,nrow(spdata))
    spdata$wbmID[which(!is.na(match(spdata$comid,spcoords_subset$comid)))] = spcoords_cellID
  }
  
  if(ohio)
  {
    if(occ_or_proj)
    {
      ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/%s_wcomid.csv',spname)
      write.csv(spdata,ofilename,row.names=FALSE)  
    } else{
      ofilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/huc%d.csv",hucnum)
      write.csv(spdata,ofilename,row.names = FALSE)
    }
  } else {
    if(occ_or_proj)
    {
      ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
      write.csv(spdata,ofilename,row.names=FALSE)  
    } else{
      ofilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",hucnum)
      write.csv(spdata,ofilename,row.names = FALSE)
    }
  }
}




setwd('G:/My Drive/research/sdm_modeling/spdata')
spnamedata = read.csv('./comprehensive_sp_info.csv')
#nalen = c()
for (i in 2)#1:nrow(spnamedata))
{
  i=20
  spname = spnamedata$name[i]
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
  spdata = read.csv(spfilename)
  idx = which(is.na(spdata$wbmID))
  for(j in idx)
  {
    print(as.numeric(spdata[j,c('decimalLatitude','decimalLongitude')]))
  }
  #spdata[idx,]
#  if(length(which(is.na(spdata$wbmID))))
#  {
#    nalen[i] = length(which(is.na(spdata$wbmID)))
#  } else {
#    nalen[i] = 0
#  }
}


nalen = c()
for ( i in 1:18)
{
  hucnum = i #5
  projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",hucnum)
  data = read.csv(projfilename)
  nalen[i] = length(which(is.na(data$wbmID)))
}
nalen
