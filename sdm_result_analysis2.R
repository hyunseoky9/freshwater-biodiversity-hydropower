# get difference in degreeday and numday_above_tmax between pristine and reservoir scenario
library(data.table)
library(usdm)
rm(list=ls())

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
nolownetsymdiff = 1
threshold = 0.4

gcmver = 'DOE-CNRM-ESM2-1'#'DOE-ACCESS-CM2'
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
for(i in idx0[1:10])
{
  spdatafilenametf = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv','pristine_gcm_reservoir',gcmver,'current',spnamedata$name[i])
  projectionareantffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[i])
  #projectionareatffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine_gcm_reservoir',gcmver,'current',spnamedata$name[i])
  projectionareatffilename = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/%s/%s/%s/%s_projarea.csv','pristine_gcm_reservoir',gcmver,'current',spnamedata$name[i])
  sp = fread(spdatafilenametf)
  projtf = fread(projectionareatffilename)
  projntf = fread(projectionareantffilename)
  projtf_res = projtf
  #projectionareaflowfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
  projectionareaflowfilename = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
  projflow = fread(projectionareaflowfilename)
  
  
  proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c','dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c','dd150_10c','avgtemp')],
               projflow[,c('maxflow','maxflowdate','minflow','minflowdate','avgflow','maxminflowdiff')])
  
  
  
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
  
  if(length(which(is.na(sp$BFI)))>0)
  {
    sp = sp[-which(is.na(sp$BFI)),]
  }
  if(length(which(is.na(proj$BFI)))>0)
  {
    proj = proj[-which(is.na(proj$BFI)),]    
  }
  
  if(length(which(is.na(sp$slope)))>0)
  {
    sp = sp[-which(is.na(sp$slope)),]
  }
  if(length(which(is.na(proj$slope)))>0)
  {
    proj = proj[-which(is.na(proj$slope)),]    
  }
  # exclude rows with streamorder value of -9
  if(length(which(sp$streamorder==-9))>0)
  {
    sp = sp[-which(sp$streamorder==-9),]  
  }
  if(length(which(proj$streamorder==-9))>0)
  {
    proj = proj[-which(proj$streamorder==-9),]
  }
  
  if(any(vif(na.omit(proj[,c('minflow','avgflow')]))[2]>10))
  {
    proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,slope))      
  } else
  {
    proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
  }
  
  if(sum(na.omit(proj$numday_above_tmax)) == 0)
  {
    proj = dplyr::select(proj,-c(numday_above_tmax)) 
  }
  proj_res = proj

  
  
  
  
  
  
  spdatafilenametf = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
  #projectionareatffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine_gcm_reservoir',gcmver,'current',spnamedata$name[i])
  projectionareatffilename = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
  sp = fread(spdatafilenametf)
  projtf = fread(projectionareatffilename)
  projtf_pri = projtf
  
  #projectionareaflowfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
  projectionareaflowfilename = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
  projflow = fread(projectionareaflowfilename)
  
  
  proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c','dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c','dd150_10c','avgtemp')],
               projflow[,c('maxflow','maxflowdate','minflow','minflowdate','avgflow','maxminflowdiff')])
  
  
  
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
  
  if(length(which(is.na(sp$BFI)))>0)
  {
    sp = sp[-which(is.na(sp$BFI)),]
  }
  if(length(which(is.na(proj$BFI)))>0)
  {
    proj = proj[-which(is.na(proj$BFI)),]    
  }
  
  if(length(which(is.na(sp$slope)))>0)
  {
    sp = sp[-which(is.na(sp$slope)),]
  }
  if(length(which(is.na(proj$slope)))>0)
  {
    proj = proj[-which(is.na(proj$slope)),]    
  }
  # exclude rows with streamorder value of -9
  if(length(which(sp$streamorder==-9))>0)
  {
    sp = sp[-which(sp$streamorder==-9),]  
  }
  if(length(which(proj$streamorder==-9))>0)
  {
    proj = proj[-which(proj$streamorder==-9),]
  }
  
  if(any(vif(na.omit(proj[,c('minflow','avgflow')]))[2]>10))
  {
    proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,slope))      
  } else
  {
    proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
  }
  
  if(sum(na.omit(proj$numday_above_tmax)) == 0)
  {
    proj = dplyr::select(proj,-c(numday_above_tmax)) 
  }
  proj_pri = proj
  
  tailwaterlen = length(which(!is.na(projntf$udd2hilarri_umdamcomid)))
  tailwater = projntf$comid[which(!is.na(projntf$udd2hilarri_umdamcomid))]
  
  length(which(proj_pri$numday_above_tmax!=proj_res$numday_above_tmax)) # number of reaches where numday_above_tmax is different btw pri and res
  length(which(proj_pri$dd90_8c!=proj_res$dd90_8c)) # number of reaches where degreeday is different btw pri and res
  length(which(projtf_pri$dd90_8c!=projtf_res$dd90_8c)) # number of reaches where 
  #dddiffcomid = projtf_pri$comid[which(projtf_pri$dd90_8c!=projtf_res$dd90_8c)]
  #print(length(which(!dddiffcomid %in% tailwater)))

  dddiff = abs(proj_pri$dd90_8c[which(proj_pri$comid %in% tailwater)] - proj_res$dd90_8c[which(proj_res$comid %in% tailwater)]  )
  hist(dddiff,main="degreeday by day 90 (8C baseline) difference between res and pri scenario")
  namdiff = abs(proj_pri$numday_above_tmax[which(proj_pri$comid %in% tailwater)] - proj_res$numday_above_tmax[which(proj_res$comid %in% tailwater)])
  hist(namdiff,main="maximum number of days above tmax difference between res and pri scenario")
}
