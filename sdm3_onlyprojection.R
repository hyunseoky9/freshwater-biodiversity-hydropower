# sdm3_onlyprojection.R
# using the calibrated parameters from sdm3.R with reservoir scenario,
# project presence onto background points in other scenarios.
library(devtools)
#install_github('johnbaums/rmaxent')
library(rmaxent)
library(rJava) # need to have Rtools installed: https://cran.rstudio.com/bin/windows/Rtools/
library(dplyr)
library(sf)
library(sp)
library(usdm)
library(sdm)
library(raster)
library(dismo)
library(car)
library(beepr)
library(data.table)
library(dismotools)#install_github('BigelowLab/dismotools')

#Choose descriptive name for model run.  
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
nolownetsymdiff = 1
threshold = 0.4

idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
gcmver = 'DOE-ACCESS-CM2'
scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
dataversion = 2
noflowdata = c()
for(i in idx0)
{
  # read in data
  #spdatafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens.csv'
  spdatafilenametf = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv',scenario,gcmver,currentofuture,spnamedata$name[i])
  projectionareantffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[i])
  projectionareatffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv',scenario,gcmver,currentofuture,spnamedata$name[i])
  sp = fread(spdatafilenametf)
  projtf = fread(projectionareatffilename)
  projntf = fread(projectionareantffilename)
  if(scenario=='pristine_gcm_reservoir')
  {
    projectionareaflowfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,currentofuture,spnamedata$name[i])
    projflow = fread(projectionareaflowfilename)
    proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c','dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c','dd150_10c','avgtemp')],
                 projflow[,c('maxflow','maxflowdate','minflow','minflowdate','avgflow','maxminflowdiff')])
    
  } else {
    proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c','dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c','dd150_10c','avgtemp','maxflow','maxflowdate','minflow','minflowdate','avgflow','maxminflowdiff')])
  } 
  # get rid of points with low netsymmetricdifference value 
  if(nolownetsymdiff)
  {
    sp = sp[which(sp$wbmID_30sec_netsymdiff<threshold),]
    proj = proj[which(proj$wbmID_30sec_netsymdiff<threshold),]
    if(nrow(sp)<30)
    {
      cat(i)
    }
  }
  # NA BFI values to 0
  sp$BFI[which(is.na(sp$BFI))] = 0
  proj$BFI[which(is.na(proj$BFI))] = 0
  # exclude rows with streamorder value of -9
  if(length(which(sp$streamorder==-9))>0)
  {
    sp = sp[-which(sp$streamorder==-9),]  
  }
  if(length(which(proj$streamorder==-9))>0)
  {
    proj = proj[-which(proj$streamorder==-9),]
  }
  
  
  # presence 
  p = rep(0, length(proj$comid))
  p[which(!is.na(match(proj$comid,sp$comid)))] = 1 # presence
  proj$p = p
  sp$p = 1
  
  # predictor
  path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s/%s',scenario,gcmver,currentofuture)
  
  if(dataversion == 1)
  {
    path2 = sprintf('%s/%s_pristinerun_dfall',path2,spnamedata$name[i])
    mod_path = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/pristine_gcm_reservoir/%s/current/%s_pristinerun_dfall',gcmver,spnamedata$name[i])
  } else if(dataversion == 2)
  {
    path2 = sprintf('%s/%s_pristinerun_dfdd90_8c',path2,spnamedata$name[i])
    mod_path = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/pristine_gcm_reservoir/%s/current/%s_pristinerun_dfdd90_8c',gcmver,spnamedata$name[i])
  } else if(dataversion == 3)
  {
    path2 = sprintf('%s/%s_pristinerun_dfdd90_10c',path2,spnamedata$name[i])
    mod_path = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/pristine_gcm_reservoir/%s/current/%s_pristinerun_dfdd90_10c',gcmver,spnamedata$name[i])
  }
  
  if(nolownetsymdiff)
  {
    path2 = paste(path2,sprintf('_lownetsymdiff%.2f',threshold),sep='')  
    mod_path = paste(mod_path,sprintf('_lownetsymdiff%.2f',threshold),sep='')  
  }
  
  
  Descriptive_Model_Name = read_maxent(mod_path)
  
  
  comid_addon = proj$comid
  p_addon = proj$p
  predall <- project(Descriptive_Model_Name, proj)
  predalldf <- as.data.frame(predall)
  
  # suitability data based on minimum training presence (MTP)
  mtp = min(predalldf$prediction_cloglog[which(p_addon==1)])
  s_mtp = rep(0,nrow(predalldf))
  s_mtp[which(predalldf$prediction_cloglog>mtp)] = 1
  
  # Maxsss 
  # res: Liu et al. 2013; Frans 2018
  filename = sprintf('%s/maxentResults.csv',mod_path)
  res_df <- read.csv(filename, header=TRUE)
  max3strain = res_df$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold # this is maxsss. decide whether to do training or test.
  max3stest = res_df$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold
  s_max3s = rep(0,nrow(predalldf))
  s_max3s[which(predalldf$prediction_cloglog>mtp)] = 1
  
  
  # write out put of the binary prediction and cloglog probability.
  o = data.frame(comid=comid_addon,probability=predalldf$prediction_cloglog,maxsss=s_max3s,mtp=s_mtp)
  dir.create(path2)
  ofilename = sprintf('%s/%s_binary_predictions.csv',path2,spnamedata$name[i])
  
  
  write.csv(o,ofilename,row.names=FALSE)
  # for graphic output, use sdmresult_graph.R
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}


