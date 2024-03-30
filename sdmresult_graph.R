# making sdm binary output into a graphic map

#libraries
library(sf)
library(nhdplusTools)


# CONUS map as a base
dir = 'G:/My Drive/research/sdm_modeling/gis/2018_us_outline.shp'
conus_shape <- read_sf(dir)
plot(sf::st_geometry(conus_shape),lwd=2)

#select a sp.
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[1])
spdata = read.csv(spdatafilename)
  

# study area huc 6 units outline (bold line)
relhuc6dir = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/huc6/%s_huc6.csv',spnamedata$name[1])
relhuc6 = read.csv(relhuc6dir)
huc6dir = 'G:/My Drive/research/sdm_modeling/gis/wbd/huc6_CONUS.gpkg'
huc6_shape <- read_sf(huc6dir)
plot(sf::st_geometry(huc6_shape[which(as.numeric(huc6_shape$huc6) %in% relhuc6$huc6),]),add=TRUE)

# plot all the comids with presence.
filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/sdmresults/%s_sdmoutput.csv',spnamedata$name[1])
sdmo = read.csv(filename)  #sdm output
pcomids = sdmo$comid[which(sdmo$maxsss==1)]
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = pcomids,
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = TRUE,
                         return_data = TRUE, overwrite = TRUE)

plot(subset$NHDFlowline_Network$geometry,add=TRUE,lwd=5)
