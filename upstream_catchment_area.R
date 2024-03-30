
#ref:
#https://cran.r-project.org/web/packages/nhdplusTools/vignettes/nhdplusTools.html
#https://ryanpeek.org/mapping-in-R-workshop/03_vig_snapping_points_to_line.html
#get species occurrence data.
# pull out species list
library(sf)
library(nhdplusTools)
# get upstream catchment area for each comid of an occurrence point.
# Was used for identifying occurrence points on WBM grid.
# Computation time too long for getting the catchment area so this code won't likely be used

library(data.table)
library(pbapply)
setwd('F:/sdm_modeling/spdata/per_sp') #where all the species occ data is in.
splist = read.csv("F:/sdm_modeling/spdata/sp_list.csv")
splist = splist$species_name

comidcalc <- function(longlat){
  occ_pt <- st_sfc(st_point(longlat), crs = 4269)
  return(discover_nhdplus_id(occ_pt))
}

get_discharge <- function(comid){
  ds_gages <- navigate_nldi(list(featureSource = "comid",featureID = comid),
                                 mode = "DM",
                                 distance_km = 100,
                                 data_source = "nwissite")
  
  us_gages <- navigate_nldi(list(featureSource = "comid",featureID = comid),
                                 mode = "UT",
                                 distance_km = 100,
                                 data_source = "nwissite")
}

upstream_catchment_area <- function(comid){
  #get upstream comids
  flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 9999)
  #download catchment area info for all upstream comids and the initial comid
  subset_file <- tempfile(fileext = ".gpkg")
  subset <- subset_nhdplus(comids = flowline$UT$nhdplus_comid,
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = FALSE,
                           return_data = TRUE, overwrite = TRUE)
  #plot the flowline of all the upstream comids
  #flowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
  #plot(sf::st_geometry(flowline), col = "blue")
  return(sum(subset$CatchmentSP$areasqkm)) # return total catchment area
}

s = "Acipenser transmontanus"
#s = 'Acipenser fulvescens'
for(s in splist)
{
  # pull out occ data for species s
  filename = sprintf("%s.csv",s)
  spdata <- fread(filename, header = TRUE)
  spdata$comid = NA # make a new column for comid

  #get comid
  spdata$comid <- pbapply(cbind(spdata$decimalLongitude,spdata$decimalLatitude),1,comidcalc)
  
  # change comid column from list to array
  idx <- !(sapply(spdata$comid, length))
  spdata$comid[idx] <- NA
  spdata$comid <- unlist(spdata$comid)
  
  # get rid of occurrences without comid
  spdata = spdata[!is.na(spdata$comid),]
  
  # get upstream catchment catchment area
  spdata$up_catch_area = pbsapply(spdata$comid,upstream_catchment_area)
  
  
  #foo = Reduce(c,spdata$comid)
  ofilename = sprintf("%s.csv",s) #output filename
  write.csv(spdata,filename)
}
