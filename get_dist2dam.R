# uses cooper data to get distance to upstream dams for all occ pts with comids identified.
rm(list=ls())
library(sf)
library(foreach)
library(Parallel)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spnamedata = read.csv('./comprehensive_sp_info.csv')
spname_list = spnamedata$name
damdistdata = read.csv('G:/My Drive/research/sdm_modeling/gis/distance2dam_data.csv')
damdistdata = data.frame(cbind(damdistdata$UM2D,damdistdata$COMIDV2))
names(damdistdata) = c('updist','comid')
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area
if(occ_or_proj)
{
  looplength = nrow(spnamedata)
} else {
  looplength = 18 # number of huc units
}
hucswo5 = 1:18
hucswo5 = hucswo5[-5]

for(i in hucswo5)#looplength)#1:length(spname_list))
{
  # **spdata could mean species occurrence points or projection area.
  
  if(occ_or_proj)
  {
    spname = spnamedata$name[i]
    # for getting predictor values of fitting points.(sp occ pts)
    spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
    data = read.csv(spfilename)  
  } else{
    # for getting predictor values of projection points.
    hucnum = i
    projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",hucnum)
    data = read.csv(projfilename)
    print(nrow(data))
  }
  if(ncol(data)==1)
  {
    data = read.csv(filename, sep="\t")
  }
  data$udd = damdistdata$updist[match(data$comid,damdistdata$comid)] # udd=upstream dam distance
  # match occ data comid to coopers damdist comid. reference the script before this one.
  if(occ_or_proj)
  {
    spdata = write.csv(data,spfilename,row.names=FALSE)  
  } else{
    spdata = write.csv(data,projfilename,row.names = FALSE)
  }
}




