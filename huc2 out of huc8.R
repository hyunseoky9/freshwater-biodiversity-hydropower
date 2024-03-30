# make huc2 shapefile outof huc8
library(ggplot2)
theme_set(theme_bw())
library(pbapply)
library(data.table)
library(sf)
library(raster)
library(sp)
library(dplyr)

huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)

huc8conus$HUC2 = floor(as.numeric(huc8conus$HUC8)/10^6)

huc2conus = huc8conus %>% 
  group_by(HUC2) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
huc2conus
#huc2conus = st_union(huc8conus,by=HUC2)
plot(huc2conus)
ofilename = 'G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc2.shp'
st_write(huc2conus,ofilename)
