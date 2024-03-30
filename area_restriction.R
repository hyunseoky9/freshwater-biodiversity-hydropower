# get range maps of the study species to spatially restrict the sdm.
library(sp)
library(sf)
library(rgdal)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
names = spnamedata$name

fish = readOGR('G:/My Drive/research/sdm_modeling/spdata/iucn_rangemaps/FW_FISH/FW_FISH_PART1.shp')
idx = which(fish$binomial %in% names)
length(unique(fish$binomial[idx]))/length(names)
fish_select = fish[idx,]
fish2 = st_read('G:/My Drive/research/sdm_modeling/spdata/iucn_rangemaps/FW_FISH/FW_FISH_PART2.shp')
length(which(fish2$binomial %in% names))/length(names)
idx2 = which(fish2$binomial %in% names)
length(unique(fish2$binomial[idx2]))/length(names)
fish2_select = fish2[idx2,]

mussel = st_read('G:/My Drive/research/sdm_modeling/spdata/iucn_rangemaps/FW_MOLLUSCS/FW_MOLLUSCS.shp')
idx3 = which(mussel$binomial %in% names)
length(unique(mussel$binomial[idx3]))/length(names)
mussel_select = mussel[idx3,]


list = unique(c(unique(fish$binomial[idx]),unique(fish2$binomial[idx2]),unique(mussel$binomial[idx3])))
length(list)
length(spnamedata$mussel_or_fish) - sum(spnamedata$mussel_or_fish)

# clip each species' map with conus shapefile
us = readOGR('G:/My Drive/research/sdm_modeling/gis/2018_us_outline.shp')
coords <- spTransform(coords, us_crs)
fish_select = st_transform(fish_select,st_crs(us))
cropped.fish_select = list()
for(i in 1:nrow(fish_select))
{
  cropped.fish_select[[1]] = st_crop(fish_select$geometry[i],us)  
}



plot(cropped)

plot(us,add=TRUE)
plot(fish_select$geometry[1:535])


# merge same species shapes into one row. 