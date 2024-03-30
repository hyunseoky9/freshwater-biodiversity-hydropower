#making the huc projection files into processed files for snapping onto wbm cells. has lat long and catchment area info. 

#relhucs = 1:18
relhucs = c(3,4,5,17,18)
for (i in relhucs)
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
  data = read.csv(filename)
  newdata = data[c('decimalLatitude','decimalLongitude','catchment_areasqkm')]
  newdata = cbind(1:nrow(newdata),newdata)
  names(newdata) = c('id','lat','lon','catchment_area')
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/for_snapping/huc%d_tosnap.csv',i)
  write.csv(newdata,ofilename,row.names=FALSE)
}  
