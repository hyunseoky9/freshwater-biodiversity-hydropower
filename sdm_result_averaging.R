# averaging the prediction results across data produced with different GCMS.
# doing SDM with ENMeval. Unlike previous versions, model-tuning and partitioning for k-fold validation is conducted.
# also sdm4 does calibration as well as projection with different scenario background data 
# code reference: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html#null
rm(list=ls())
library(devtools)
library(dplyr)
library(data.table)
set.seed(458)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
nolownetsymdiff = 1
threshold = 0.4
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
gcmver = c('DOE-ACCESS-CM2','DOE-BCC-CSM2-MR','DOE-CNRM-ESM2-1')  #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
start <- proc.time()
for(i in idx0)
{
  # get directory path
  dirpath_pre = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s',gcmver)
  dataversion = 2
  if(dataversion == 1)
  {
    dirpath_post = sprintf('/%s_pristinerun_dfall',spnamedata$name[i])
  } else if(dataversion == 2)
  {
    dirpath_post = sprintf('/%s_pristinerun_dfdd90_8c',spnamedata$name[i])
  } else if(dataversion == 3)
  {
    dirpath_post = sprintf('/%s_pristinerun_dfdd90_10c',spnamedata$name[i])
  }
  if(nolownetsymdiff)
  {
    dirpath_post = paste(dirpath_post,sprintf('_lownetsymdiff%.2f',threshold),sep='')  
  }
  dirpath = paste0(dirpath_pre,dirpath_post)
  
  ## average project presence across gcms
  
  # create output directory path
  odirpath = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s',dirpath_post)
  dir.create(odirpath)
  # get threshold values
  filename = sprintf('%s/prediction_thresholds.csv',dirpath)
  thra = read.csv(filename[1])
  thrb = read.csv(filename[2])
  thrc = read.csv(filename[3])
  thrm = as.data.frame(t(apply(rbind(thra,thrb,thrc),2,mean)))
  ofilename = sprintf('%s/avg_prediction_thresholds.csv',odirpath)
  write.csv(thrm,ofilename,row.names=FALSE)
  mtp = thrm$mtp
  maxsss = thrm$maxsss
  p10 = thrm$p10
  
  # a)current reservoir scenario
  {
    filename = sprintf('%s/%s_binary_predictions_baseline.csv',dirpath,spnamedata$name[i])
    currentresa = fread(filename[1])
    currentresb = fread(filename[2])
    currentresc = fread(filename[3])
    probs = cbind(currentresa$probability,currentresb$probability,currentresc$probability)
    avgprob = apply(probs,1,mean)
    
    s_mtp = rep(0,length(avgprob))
    s_mtp[which(is.na(avgprob))] = NA
    s_mtp[which(avgprob>mtp)] = 1
    s_maxsss = rep(0, length(avgprob))
    s_maxsss[which(is.na(avgprob))] = NA
    s_maxsss[which(avgprob>maxsss)] = 1
    s_p10 = rep(0, length(avgprob))
    s_p10[which(is.na(avgprob))] = NA
    s_p10[which(avgprob>p10)] = 1
    
    currentresavg = data.frame(comid = currentresa$comid,
                               probability = avgprob, mtp=s_mtp,
                               maxsss=s_maxsss, p10=s_p10)
    ofilename = sprintf('%s/%s_avg_binary_predictions_baseline.csv',odirpath,spnamedata$name[i])
    write.csv(currentresavg,ofilename,row.names=FALSE)
  }
  print('base scenario projection complete')
  # b) future reservoir scenario 
  {
    filename = sprintf('%s/%s_binary_predictions_futureres.csv',dirpath,spnamedata$name[i])
    futureresa = fread(filename[1])
    futureresb = fread(filename[2])
    futureresc = fread(filename[3])
    probs = cbind(futureresa$probability,futureresb$probability,futureresc$probability)
    avgprob = apply(probs,1,mean)
    
    s_mtp = rep(0,length(avgprob))
    s_mtp[which(is.na(avgprob))] = NA
    s_mtp[which(avgprob>mtp)] = 1
    s_maxsss = rep(0, length(avgprob))
    s_maxsss[which(is.na(avgprob))] = NA
    s_maxsss[which(avgprob>maxsss)] = 1
    s_p10 = rep(0, length(avgprob))
    s_p10[which(is.na(avgprob))] = NA
    s_p10[which(avgprob>p10)] = 1
    
    futureresavg = data.frame(comid = futureresa$comid,
                               probability = avgprob, mtp=s_mtp,
                               maxsss=s_maxsss, p10=s_p10)
    ofilename = sprintf('%s/%s_avg_binary_predictions_futureres.csv',odirpath,spnamedata$name[i])
    write.csv(futureresavg,ofilename,row.names=FALSE)
  }
  print('future reservoir scenario projection complete')
  # c) project to current pristine scenario bg data using the calibrated model
  {
    filename = sprintf('%s/%s_binary_predictions_currentpri.csv',dirpath,spnamedata$name[i])
    currentpria = fread(filename[1])
    currentprib = fread(filename[2])
    currentpric = fread(filename[3])
    probs = cbind(currentpria$probability,currentprib$probability,currentpric$probability)
    avgprob = apply(probs,1,mean)
    
    s_mtp = rep(0,length(avgprob))
    s_mtp[which(is.na(avgprob))] = NA
    s_mtp[which(avgprob>mtp)] = 1
    s_maxsss = rep(0, length(avgprob))
    s_maxsss[which(is.na(avgprob))] = NA
    s_maxsss[which(avgprob>maxsss)] = 1
    s_p10 = rep(0, length(avgprob))
    s_p10[which(is.na(avgprob))] = NA
    s_p10[which(avgprob>p10)] = 1
    
    currentpriavg = data.frame(comid = currentpria$comid,
                              probability = avgprob, mtp=s_mtp,
                              maxsss=s_maxsss, p10=s_p10)
    ofilename = sprintf('%s/%s_avg_binary_predictions_currentpri.csv',odirpath,spnamedata$name[i])
    write.csv(currentpriavg,ofilename,row.names=FALSE)
  }
  print('current pristine scenario projection complete')
  # d) project to future pristine scenario bg data using the calibrated model
  {
    filename = sprintf('%s/%s_binary_predictions_futurepri.csv',dirpath,spnamedata$name[i])
    futurepria = fread(filename[1])
    futureprib = fread(filename[2])
    futurepric = fread(filename[3])
    probs = cbind(futurepria$probability,futureprib$probability,futurepric$probability)
    avgprob = apply(probs,1,mean)
    
    s_mtp = rep(0,length(avgprob))
    s_mtp[which(is.na(avgprob))] = NA
    s_mtp[which(avgprob>mtp)] = 1
    s_maxsss = rep(0, length(avgprob))
    s_maxsss[which(is.na(avgprob))] = NA
    s_maxsss[which(avgprob>maxsss)] = 1
    s_p10 = rep(0, length(avgprob))
    s_p10[which(is.na(avgprob))] = NA
    s_p10[which(avgprob>p10)] = 1
    
    futurepriavg = data.frame(comid = futurepria$comid,
                               probability = avgprob, mtp=s_mtp,
                               maxsss=s_maxsss, p10=s_p10)
    ofilename = sprintf('%s/%s_avg_binary_predictions_futurepri.csv',odirpath,spnamedata$name[i])
    write.csv(futurepriavg,ofilename,row.names=FALSE)
  }
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}


