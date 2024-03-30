# get rid of occurrence pts whose comids are duplicated. Also only take species in 2000-2020
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_allyr.csv')
spname_list = spnamedata$name
for ( i in 1:length(spname_list))
{

  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  data = read.csv(filename)
  # get only occ pts from 2000 to 2020
  data = data[which(data$year>=2000 & data$year<=2020),]
  # get rid of pts with no comids (NA)
  if(length(which(is.na(data$comid)))>0)
  {
    print('there are na comids')
    data = data[-which(is.na(data$comid)),]
  }
  # get rid of duplicate comids
  if(sum(duplicated(data$comid))>0)
  {
    data = data[-which(duplicated(data$comid)),]  
  }

  write.csv(data,filename,row.names=FALSE)
  print(i)
}


# get rid of species with <30 occ pts.
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_allyr.csv')
num = c()
occ_or_proj = 1
for(i in 1:nrow(spnamedata))
{
  if(occ_or_proj)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    data = read.csv(filename)
    head(data,50)
  } else {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
    data = read.csv(filename)
  }
  num[i] = nrow(data)
}
rmidx = which(num<30)

for (i in rmidx)
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  unlink(filename)
}
filename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spnamedatao = spnamedata[-rmidx,]
write.csv(spnamedatao,filename)
