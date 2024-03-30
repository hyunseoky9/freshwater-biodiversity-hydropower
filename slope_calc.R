# getting stream order for all comids in all hucs as well as in all occpts and proj areas.

library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)
occ_or_proj= 1 #1= sp occurrene pts. 0= projection area

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spname_list = spnamedata$name
if (occ_or_proj)
{
  looplength = length(spname_list)
} else {
  looplength = 18 # number of huc units
}


currentofuture.list = c('current','future')
gcmver = c('DOE-ACCESS-CM2','DOE-BCC-CSM2-MR','DOE-CNRM-ESM2-1')
scenario = c('pristine w gcm','pristine_gcm_reservoir')

for(i in 1:nrow(spnamedata))#:looplength)
{
  if(occ_or_proj)
  {
    spdatafilenametf = sprintf(
      'G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv',
      scenario,gcmver,currentofuture,spnamedata$name[i])
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp//%s_wcomid.csv',spnamedata$name[i])
    data = read.csv(filename)
  } else {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
    data = read.csv(filename)
  }
  comid = data$comid[which(!is.na(data$comid))]
  numloops = ceiling(length(comid)/100)
  streamorder = c()
  slope = c()
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
      slo = subset$NHDFlowline_Network$slope[idx]
    } else {
      sto = subset$NHDFlowline_Network$streamorde
      slo = subset$NHDFlowline_Network$slope
    }
    comids = c(comids, comid[start:end])
    streamorder = c(streamorder,sto)
    slope = c(slope,slo)
    slope = as.numeric(slope)
    print(sprintf('%d/%d done',j,numloops))  
  }
  print('subset no error')
  #data$streamorder = streamorder #stream order
  data$slope = slope
  data$slope[which(data$slope<0)] = NA
  write.csv(data,filename,row.names=FALSE)
  print(sprintf('%d done' , i))
}


# 2. get predictor value for each species projection area
rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')

for(i in 2:nrow(spdata))
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  spproj = read.csv(path1)
  hucs = unique(spproj$huc2)
  spproj$slope = NA
  for(j in 1:length(hucs))
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$comid[which(spproj$huc2==hucs[j])], hucd$comid)
    # get rest of the predictors from by_huc data.
    spproj$slope[which(spproj$huc2==hucs[j])] = hucd$slope[idx]
  }
  # write out the data in a separate file
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}



# 3.  get predictor value for each species occurrence data
occ_or_proj= 1 #1= sp occurrene pts. 0= projection area

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spname_list = spnamedata$name
if (occ_or_proj)
{
  looplength = length(spname_list)
} else {
  looplength = 18 # number of huc units
}


currentofuture.list = c('current','future')
gcmver.list = c('DOE-ACCESS-CM2','DOE-BCC-CSM2-MR','DOE-CNRM-ESM2-1')
scenario.list = c('pristine w gcm','pristine_gcm_reservoir')
for(currentofuture in currentofuture.list)
{
  for(gcmver in gcmver.list)
  {
    for(scenario in scenario.list )
    {
      if(currentofuture=='current' & gcmver=='DOE-ACCESS-CM2' & scenario=='pristine w gcm')
      {
        next
        for(i in 49:looplength)
        {
          if(occ_or_proj)
          {
            filename = sprintf(
              'G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv',
              scenario,gcmver,currentofuture,spnamedata$name[i])
            data = read.csv(filename)
          } else {
            filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
            data = read.csv(filename)
          }
          comid = data$comid[which(!is.na(data$comid))]
          numloops = ceiling(length(comid)/100)
          streamorder = c()
          slope = c()
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
            if(length(comid[start:end])!= length(subset$NHDFlowline_Network$slope))
            {
              print('wat')
              idx = match(comid[start:end],subset$NHDFlowline_Network$comid)
              slo = subset$NHDFlowline_Network$slope[idx]
            } else {
              sto = subset$NHDFlowline_Network$streamorde
              slo = subset$NHDFlowline_Network$slope
            }
            comids = c(comids, comid[start:end])
            streamorder = c(streamorder,sto)
            slope = c(slope,slo)
            slope = as.numeric(slope)
            print(sprintf('%d/%d done',j,numloops))  
          }
          print('subset no error')
          #data$streamorder = streamorder #stream order
          data$slope = slope
          data$slope[which(data$slope<0)] = NA
          write.csv(data,filename,row.names=FALSE)
          print(sprintf('%d done' , i))
        }      
      } else {
        for(i in 2:looplength)
        {
          if(occ_or_proj)
          {
            filename = sprintf(
              'G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv',
              scenario,gcmver,currentofuture,spnamedata$name[i])
            data = read.csv(filename)
          } else {
            filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
            data = read.csv(filename)
          }
          copydata.filename = sprintf(
            'G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv',
            'pristine w gcm','DOE-ACCESS-CM2','current',spnamedata$name[i])
          copydata = fread(copydata.filename)
          if(sum(copydata$comid==data$comid)==nrow(data))
          {
            data$slope = copydata$slope            
          } else {
            stop("copydata comid does not match with the subject data comid!")
          }
          write.csv(data,filename,row.names=FALSE)
          print(sprintf('%d done' , i))
        }      
      }
    }
  }
}
