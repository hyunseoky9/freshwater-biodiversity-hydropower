# getting stream order for all comids in all hucs as well as in all occpts and proj areas.

library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spname_list = spnamedata$name
if (occ_or_proj)
{
  looplength = length(spname_list)
} else {
  looplength = 18 # number of huc units
}
 
# getting streamorder for by_huc projection area
for(i in 1:looplength)
{
  if(occ_or_proj)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    data = read.csv(filename)
  } else {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
    data = read.csv(filename)
  }
  comid = data$comid[which(!is.na(data$comid))]
  numloops = ceiling(length(comid)/100)
  streamorder = c()
  comids = c()
  for( j in 1:numloops)
  {
    start = (j-1)*100+1
    if(j!=numloops)
    {
      end = j*100  
    } else {
      end = length(comid)
    }
    subset_file <- tempfile(fileext = ".gpkg")
    subset <- subset_nhdplus(comids = comid[start:end],
                             output_file = subset_file,
                             nhdplus_data = "download", 
                             flowline_only = TRUE,
                             return_data = TRUE, overwrite = TRUE)
    if(length(comid[start:end])!= length(subset$NHDFlowline_Network$streamorde))
    {
      idx = match(comid[start:end],subset$NHDFlowline_Network$comid)
      sto = subset$NHDFlowline_Network$streamorde[idx]
    } else {
      sto = subset$NHDFlowline_Network$streamorde
    }
    comids = c(comids, comid[start:end])
    streamorder = c(streamorder,sto)
  }
  print('subset no error')
  data$streamorder = streamorder #stream order
  #write.csv(data,filename,row.names=FALSE)
  print(sprintf('%d done' , i))
}



# 2. get predictor value for each species projection area
rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')

for(i in 1:nrow(spdata))
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  spproj = read.csv(path1)
  hucs = unique(spproj$huc2)
  spproj$streamorder = NA
  for(j in 1:length(hucs))
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$comid[which(spproj$huc2==hucs[j])], hucd$comid)
    # get rest of the predictors from by_huc data.
    spproj$streamorder[which(spproj$huc2==hucs[j])] = hucd$streamorder[idx]
  }
  # write out the data in a separate file
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}


# 3. preictor value for sp occurrence data. 

rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')

for(i in 1:nrow(spdata))#predictor_done[16:length(predictor_done)])
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  spproj = read.csv(path1)
  hucs = unique(floor(spproj$huc12/10^10))
  hucs = hucs[which(hucs>0)] # get rid of points matched with -9999 for hucs.
  spproj$streamorder = NA
  for(j in 1:length(hucs))
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$comid[which(spproj$huc2==hucs[j])], hucd$comid)
    # get rest of the predictors from by_huc data.
    spproj$streamorder[which(spproj$huc2==hucs[j])] = hucd$streamorder[idx]
  }
  # write out the data in a separate file
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}
