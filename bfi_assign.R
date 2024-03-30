# get BFI values into the species occurrence and projection area. 
rm(list=ls())
library(sf)
library(sp)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(dplyr)
library(raster)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spnamedata = read.csv('./comprehensive_sp_info.csv')
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area
if(occ_or_proj)
{
  looplength = nrow(spnamedata)
} else {
  looplength = 18 # number of huc units
}
loopitems = 1:18
loopitems = loopitems[-c(1,5)]
for ( i in loopitems)#1:looplength)
{
  #i = 56 # for example species Aplodontis grunniens (freshwater darter). use the code with for loop later
  
  # **spdata could mean species occurrence points or projection area.
  
  if(occ_or_proj)
  {
    # for getting predictor values of fitting points.(sp occ pts)
    spname = spnamedata$name[i]
    spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
    spdata = read.csv(spfilename)
  } else{
    # for getting predictor values of projection points.
    hucnum = i
    projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",hucnum)
    spdata = read.csv(projfilename)
  }
  
  # resource: https://gis.stackexchange.com/questions/222978/lon-lat-to-simple-features-sfg-and-sfc-in-r
  spcoords_sf = st_as_sf(spdata, coords = c("decimalLongitude", "decimalLatitude"), 
                         crs = 4326, agr = "constant")
  # resource: https://campus.datacamp.com/courses/spatial-analysis-with-sf-and-raster-in-r/preparing-layers-for-spatial-analysis?ex=10
  spcoords_sp = as(spcoords_sf,Class='Spatial')
  
  
  # restrict the spatial extent of the operation
  restrict = 0 # 1=restrict spatial extent, 0=don't
  if (restrict)
  {
    filename = 'G:/My Drive/research/sdm_modeling/gis/nhd05 catchment/NHDPlusMS/NHDPlus05/05unit_boundary/Catchment_Dissolve.shp'
    ohiobasin = readOGR(filename) #epsg=4269
    spcoords_sp = spTransform(spcoords_sp, CRS(proj4string(ohiobasin))) # make projections and coordinate system consistent
    # check the plots to make sure the points and the extent map looks consistent with each other
    #plot(ohiobasin)
    #points(spcoords_sp)
    spcoords_subset <- spcoords_sp[ohiobasin,]
    # check if the subsetting worked
    #plot(ohiobasin)
    #points(spcoords_subset)
  } else {
    spcoords_subset <- spcoords_sp
  }
  
  
  
  #get the BFI values for each of those points.
  #resource: https://www.gisremotesensing.com/2012/10/extract-raster-values-from-points.html
  # read in the cellID raster
  bfi = raster('G:/My Drive/research/sdm_modeling/gis/BFI_groundwater/bfi48grd/bfi48grd/hdr.adf')
  spcoords_BFI = extract(bfi,spcoords_subset)
  spdata$BFI = rep(NA,nrow(spdata))
  spdata$BFI[spcoords_subset$X] = spcoords_BFI
  if(occ_or_proj)
  {
    write.csv(spdata,spfilename,row.names=FALSE)  
  } else{
    write.csv(spdata,projfilename,row.names = FALSE)
  }
  
}
  





