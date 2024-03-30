# adding coordinate uncertainty in meters to species occurrence data. 
library(rgbif)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
names = spnamedata$name

startfrom = 11 # if you want to start downloading from the middle of the list
lifestage =c()
splist =  names #g1g2sp retrieved from g1g2sp.R
nomatchnum = c()

for (i in startfrom:length(splist))
{
  i=26
  s = splist[i]
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',s)
  spdata = read.csv(spfilename)  
  gotdata = 0
  # gbif calling
  while(!gotdata){ 
    result <- try(gbif_data <- occ_data(scientificName = s, country='US',eventDate = '2000,2020', hasCoordinate = TRUE, limit = 100000))  
    if(class(result)!="try-error"){
      gotdata = 1      
    } else {
      sprintf('i=%d, error occurred',i)
    }
  }
  matchID = match(spdata$gbifID,result$data$gbifID)
  nomatchnum[i] = sum(is.na(matchID))
  if(!all(is.na(matchID)))
  {
    spdata$uncertaintym = result$data$coordinateUncertaintyInMeters[matchID]  
  } else 
  {
    spdata$uncertaintym = NA
  }
  #print(spdata$uncertaintym)
  #print(nomatchnum[i])
  write.csv(spdata,spfilename,row.names=FALSE)
  msg = sprintf('%s done', s)
  print(msg)
}

# get all the uncertainty info of all species.
allun = c()
for (i in 1:length(splist))
{
  s = splist[i]
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',s)
  spdata = read.csv(spfilename)  
  allun = c(allun, spdata$uncertaintym[which(!is.na(spdata$uncertaintym))])
  #print(spdata$uncertaintym)
  #readline()
}


# move the species occ files with less than 30 occpts after getting rid of occ pts with uncertainty
# greater than 1km (=1000).
spnamedatafile = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spnamedata = read.csv(spnamedatafile)
names = spnamedata$name
splist= names
occnum = c()
occnumin = c()
for (i in 1:length(splist))
{
  s = splist[i]
  fromfile = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv'
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',s)
  spdata = read.csv(spfilename)  
  tofile = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/excluded sp/%s_wcomid.csv,s'
  uns = spdata$uncertaintym[which(is.na(spdata$uncertaintym) | spdata$uncertaintym<1000)]
  occnum[i] = length(spdata$uncertaintym)
  occnumin[i] = length(uns)
  if(length(uns)<30)
  {
    spnamedata$keep_in_study[i] = 0
    file.rename(from=fromfile,to=tofile)
  }
}
write.csv(spnamedata,spnamedatafile,row.names=FALSE)
