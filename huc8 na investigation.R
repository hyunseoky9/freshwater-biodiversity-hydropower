library(sf)
library(data.table)
huc8 =read.csv('G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/huc8 derived from the crosswalk table.csv')
huc8 = as.array(huc8$huc8)
tablehuc8s = huc8

huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)
spatialhuc8s = huc8conus$HUC8

length(tablehuc8s)
length(spatialhuc8s)

r = apply(as.matrix(tablehuc8s) ,1, function(x) x %in% spatialhuc8s)
r2 = apply(as.matrix(spatialhuc8s) ,1, function(x) x %in% tablehuc8s)
length(which(r))
length(which(r2))
#snomatch = spatialhuc8s[which(r2==FALSE)]
#nomatch = tablehuc8s[which(r==FALSE)]

# derive huc8s again from crosswalk
dd = read.csv('G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/CrosswalkTable_NHDplus_HU12.csv')
dh8 = dd$HUC_12
dh8 = dh8[which(dh8>=0)]
dh8 = floor(dh8/10^4)
dh8 = unique(dh8)
length(dh8)
r3 = apply(as.matrix(dh8),1,function(x) x %in% spatialhuc8s)
length(which(r3))
r4 = apply(as.matrix(dh8),1,function(x) x %in% tablehuc8s)
length(which(r4))

write.csv(dh8,'G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/huc8 derived from the crosswalk table2.csv',row.names=FALSE)

#derive huc8s from by_huc predictor dataset.
predhuc8s = c()
for( i in 1:18)
{
  ddd = fread(sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i))
  predhuc8s = c(predhuc8s,unique(floor(ddd$huc12/10^4)))
  print(i)
}
length(predhuc8s)

sum(apply(as.matrix(predhuc8s) ,1, function(x) x %in% spatialhuc8s))

#derive huc8s from by_sp predictor dataset
spdata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
sppredhuc8s = c()
for(i in 1:nrow(spdata))
{
  d4 = fread(sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i]))
  sppredhuc8s = c(sppredhuc8s,unique(floor(d4$huc12/10^4)))
  if( i %% 100 == 0)
  {
    print(i)
  }
}
length(sppredhuc8s)
sum(apply(as.matrix(sppredhuc8s) ,1, function(x) x %in% spatialhuc8s))