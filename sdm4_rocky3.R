# doing SDM with ENMeval. Unlike previous versions, model-tuning and partitioning for k-fold validation is conducted.
# also sdm4 does calibration as well as projection with different scenario background data 
# code reference: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html#null
# sdm4.R with some modification to be run on Rocky in Nimbios. (sdm4_rocky1 and 2 are test versions)
library(rmaxent)
library(ENMeval)
library(ecospat)
library(devtools)
library(dplyr)
library(sf)
library(sp)
library(data.table)
library(tidyverse)
library(usdm)
arrayid = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(arrayid)

spnamedata = read.csv('./spdata/comprehensive_sp_info.csv')
nolownetsymdiff = 1
threshold = 0.4

idxfilename = './sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

doerrored_ones = 0 # if 1, only run for species that errored previously. Which ones errored are marked in the errorspidx.csv
if(doerrored_ones)
{
  idx0 = read.csv('./sdm_results/errorspidx.csv')
  idx0 = idx0$x
}


currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
gcmver = 'DOE-CNRM-ESM2-1'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
scenario = 'pristine w gcm' #'pristine_gcm_reservoir'

maxnumoccidx = 362
minnumoccidx = 282
start <- proc.time()
for(i in idx0[arrayid])
{
  print(spnamedata$name[i])
  ## 1. call in data and process them
  # baseline scenario data read
  spdatafilenametf = sprintf('./spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv','pristine_gcm_reservoir',gcmver,'current',spnamedata$name[i])
  projectionareantffilename = sprintf('./spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[i])
  projectionareatffilename = sprintf('./spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine_gcm_reservoir',gcmver,'current',spnamedata$name[i])
  sp = fread(spdatafilenametf)
  projtf = fread(projectionareatffilename)
  projntf = fread(projectionareantffilename)
  
  projectionareaflowfilename = sprintf('./spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i])
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
  
  bckdata = proj
  # select the set of predictors to use to run the sdm with
  setwd('./')
  dirpath = sprintf('./sdm_results/%s',gcmver)
  dataversion = 2
  if(dataversion == 1)
  {
    bckdata = data.frame(dplyr::select(bckdata,decimalLongitude,decimalLatitude,BFI,waterbody,numday_above_tmax,
                                       numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                       dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                       dd150_10c,maxflow,maxflowdate,minflow,
                                       minflowdate,avgtemp,streamorder))
    spdf = data.frame(dplyr::select(sp,decimalLongitude,decimalLatitude,BFI,waterbody,numday_above_tmax,
                                    numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                    dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                    dd150_10c,maxflow,maxflowdate,minflow,
                                    minflowdate,avgtemp,streamorder))
    proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,numday_above_tmax,
                                    numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                    dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                    dd150_10c,maxflow,maxflowdate,minflow,
                                    minflowdate,avgtemp,streamorder))    
    dirpath = sprintf('%s/%s_pristinerun_dfall',dirpath,spnamedata$name[i])
  } else if(dataversion == 2)
  {
    if(any(vif(na.omit(bckdata[,c('minflow','avgflow')]))[2]>10))
    {
      bckdata = data.frame(dplyr::select(bckdata,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,slope))
      spdf = data.frame(dplyr::select(sp,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,slope))
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,slope))      
    } else
    {
      bckdata = data.frame(dplyr::select(bckdata,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
      spdf = data.frame(dplyr::select(sp,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
    }
    
    if(sum(na.omit(proj$numday_above_tmax)) == 0)
    {
      bckdata = dplyr::select(bckdata,-c(numday_above_tmax)) 
      spdf = dplyr::select(spdf,-c(numday_above_tmax)) 
      proj = dplyr::select(proj,-c(numday_above_tmax)) 
    }

    dirpath = sprintf('%s/%s_pristinerun_dfdd90_8c',dirpath,spnamedata$name[i])
  } else if(dataversion == 3)
  {
    bckdata = data.frame(dplyr::select(bckdata,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,numday_above_tmax,dd90_10c,minflow,minflowdate,maxflowdate,avgflow))
    spdf = data.frame(dplyr::select(sp,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,numday_above_tmax,dd90_10c,minflow,minflowdate,maxflowdate,avgflow))
    proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,numday_above_tmax,dd90_10c,minflow,minflowdate,maxflowdate,avgflow))
    dirpath = sprintf('%s/%s_pristinerun_dfdd90_10c',dirpath,spnamedata$name[i])
  }
  
  if(nolownetsymdiff)
  {
    dirpath = paste(dirpath,sprintf('_lownetsymdiff%.2f',threshold),sep='')  
  }
  
  # occurrence opints
  occ = spdf
  # randomly select 10000 background points for maxent
  if(nrow(bckdata)<10000)
  {
    samplesize = nrow(bckdata)
  } else {
    samplesize = 10000  
  }
  bg = bckdata[sample(1:nrow(bckdata), samplesize),]
  
  #waterbody predictor as factor
  bg$waterbody = as.factor(bg$waterbody)
  occ$waterbody = as.factor(occ$waterbody)
  
  
  # change colnames of longitude and latitude to 'longitude' and 'latitude'
  names(bg)[which(names(bg)=='decimalLongitude')] = 'longitude'
  names(occ)[which(names(occ)=='decimalLongitude')] = 'longitude'
  names(bg)[which(names(bg)=='decimalLatitude')] = 'latitude'
  names(occ)[which(names(occ)=='decimalLatitude')] = 'latitude'
  
  ## 3. partition k-fold crossvalidation, tune, and calibrate the model
  tune.args <- list(fc = c("L", "LQ","LQH","LQHP","LQHPT"), rm = 1:5) # model tuning parameter range
  #tune.args <- list(fc = c("L"), rm = 1:2) # model tuning parameter range
  #running the model
  
  # using maxnet
  #e<- try({
  #  ENMevaluate(occ, bg = bg, algorithm = "maxnet", tune.args = tune.args, 
  #                     partitions = "block",parallel=FALSE)
  #}, silent=TRUE)
  
  # using maxent.jar
  e = ENMevaluate(occ, bg = bg, algorithm = "maxent.jar", tune.args = tune.args, 
                  partitions = "block",parallel=TRUE)  
  
  # when there's error using maxnet
  #if(class(e)=='try-error')
  #{
  #  msg = geterrmessage()
  #  if(grepl('mm %*%',msg))
  #  {
  #    e = ENMevaluate(occ, bg = bg, algorithm = "maxent.jar", tune.args = tune.args, 
  #                partitions = "block",parallel=FALSE)  
  #  }
  #  if(grepl('m\\[p == 1,',msg))
  #  {
  #    e = ENMevaluate(occ, bg = bg, algorithm = "maxent.jar", tune.args = tune.args, 
  #                    partitions = "block",parallel=FALSE)  
  #  }
  #}
  
  print('enmevaluate complete')
  # the result stats
  res <- eval.results(e)
  
  if(all(is.na(res$cbi.val.avg)))
  {
    e = ENMevaluate(occ, bg = bg, algorithm = "maxent.jar", tune.args = tune.args, 
                    partitions = 'randomkfold',parallel=FALSE)
    res <- eval.results(e)
  }
  
  # model selection
  # a) highest aicc
  opt.aicc <- res %>% filter(delta.AICc == 0)
  
  # b) minimum 10percentile omission rate and tie-breaker with auc
  opt.seq1 <- res %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>% 
    filter(auc.val.avg == max(auc.val.avg))
  opt.seq1
  
  # c) maximum continuous boyce index (cbi)
  res2 = res[which(!is.na(res$cbi.val.avg)),]
  opt.seq2 <- res2 %>% 
    filter(cbi.val.avg == max(cbi.val.avg))
  opt.seq2  
  
  opt.seq = opt.seq2 # choose which selection method to go with
  if(nrow(opt.seq)>1)
  {
    opt.seq = opt.seq[1,]
  }
  mod.seq <- eval.models(e)[[opt.seq$tune.args]]
  
  print('model selection complete')
  
  ## 3.1 save result informations
  # first, make a file 
  dir.create(dirpath)
  # save betas
  if(!(class(mod.seq)[1]=='MaxEnt'))
  {
    filename = sprintf('%s/betas.csv',dirpath)
    write.csv(t(as.data.frame(mod.seq$betas)),filename,row.names=FALSE)
  }
  
  print('save betas.csv complete')
  # save response curve figure
  filename = sprintf('%s/response_curves.png',dirpath)
  if(class(mod.seq)[1]=='Maxent')
  {
    png(file=filename,
        width=1200, height=985,type='cairo')
    plot(mod.seq, type = "cloglog")
    dev.off()
  } else {
    png(file=filename,
        width=1200, height=985,type='cairo')
    dismo::response(mod.seq)
    dev.off()
  }
  print('response_curves complete')
  # save predictor percent contribution
  if(class(mod.seq)[1]!='Maxent')
  {
    varcont <- mod.seq@results[grepl('contribution', rownames(mod.seq@results)),] # variable contribution  
    filename = sprintf('%s/predictor_contribution.csv',dirpath)
    namesave = names(varcont)
    varcont = as.data.frame(t(varcont))
    names(varcont) = gsub('\\.contribution','',names(varcont))
    write.csv(as.data.frame(varcont),filename,row.names=FALSE)
  }
  print('predictor percent contribution complete')
  
  # save all possible model's eval result
  filename = sprintf('%s/evalresult.csv',dirpath)
  write.csv(res,filename,row.names=FALSE)
  print('evalresult complete')
  # save the selected model's eval result
  filename = sprintf('%s/evalresult_selected_model.csv',dirpath)
  write.csv(res[which(res$tune.args==opt.seq$tune.args),],filename,row.names=FALSE)
  print('evalresult_selected_model complete')
  
  # **Lastly, save the enmeval object and the optimal moel as an rds file to be able to use it later to project in a separate session in the future.
  filename = sprintf('%s/enmeval_model.rds',dirpath)
  write_rds(e,filename)
  filename2 = sprintf('%s/enmeval_optimal_model.rds',dirpath)
  write_rds(mod.seq,filename2)
  #e_load = read_rds(sprintf('%s/enmeval_model.rds',dirpath))
  #mod.seq_load = read_rds(sprintf('%s/enmeval_optimal_model.rds',dirpath))
  print('model save complete')
  ## 4. compare to null model
  # We first run the null simulations with 100 iterations to get a reasonable null distribution 
  # for comparisons with the empirical values
  # *not doing it. Takes too long...
  # mod.null <- ENMnulls(e, mod.settings = list(fc = "L", rm = 1), no.iter = 100)
  
  ## 5. project presence
  # a) first project to the current reservoir scenario which you used for calibration
  {
    # predict presence probability on projection area
    # predict function below derived from enm.maxnet@predict(). 
    # enm.maxnet@predict found from this source: https://groups.google.com/g/maxent/c/87_Xnuc5gBs
    if(class(mod.seq)[1]=='MaxEnt')
    {
      projpred = rmaxent::project(mod.seq,proj[,2:ncol(proj)])  
      projpred = projpred$prediction_cloglog
    } else {
      e@other.settings$doClamp = e@doClamp
      projpred = predict(mod.seq,proj[,2:ncol(proj)],type = e@other.settings$pred.type, 
                         clamp = e@other.settings$doClamp, e@other.settings$other.args)
    }
    
    # predict presence based on mtp
    if(class(mod.seq)[1]=='MaxEnt')
    {
      occpred = rmaxent::project(mod.seq,occ[,3:ncol(occ)])
      occpred = occpred$prediction_cloglog
    } else {
      occpred = predict(mod.seq,occ[,3:ncol(occ)],type = e@other.settings$pred.type, 
                        clamp = e@other.settings$doClamp, e@other.settings$other.args)
    }
    
    mtp = min(occpred)
    
    s_mtp = rep(0,length(projpred))
    s_mtp[which(projpred>mtp)] = 1
    s_mtp[which(projpred<=mtp)] = 0
    
    # maxsss
    if(class(mod.seq)[1]=='MaxEnt')
    {
      bgpred = rmaxent::project(mod.seq,bg[,3:ncol(bg)])
      bgpred = bgpred$prediction_cloglog
    } else 
    {
      bgpred = predict(mod.seq,bg[,3:ncol(bg)],type = e@other.settings$pred.type, 
                       clamp = e@other.settings$doClamp, e@other.settings$other.args)
    }
    Pred = c(occpred,bgpred)
    Sp.occ = c(rep(1,length(occpred)),rep(0,length(bgpred)))
    maxsss = ecospat.max.tss(Pred, Sp.occ)
    maxsss = maxsss$max.threshold #maxsss threshold
    
    s_maxsss = rep(0, length(projpred))  
    s_maxsss[which(projpred>maxsss)] = 1
    s_maxsss[which(projpred<=maxsss)] = 0
    
    # 10p=10th percentile of presence porbability 
    p10 = quantile(occpred, probs = 0.1)
    
    s_p10 = rep(0, length(projpred))
    s_p10[which(projpred>p10)] = 1
    s_p10[which(projpred<=p10)] = 0
    
    # save all 3 thresholds
    thresholds = data.frame(mtp=mtp,maxsss=maxsss,p10=p10)
    filename = sprintf('%s/prediction_thresholds.csv',dirpath)
    write.csv(thresholds,filename,row.names=FALSE)
    
    # save the prediction and the calibrated model details
    #df = cbind(projpred,s_mtp,s_maxsss,s_p10)
    rowidx = as.numeric(row.names(projpred))
    if(class(mod.seq)[1]=='MaxEnt')
    {
      df = data.frame(comid=proj$comid,probability=projpred,mtp=s_mtp,maxsss=s_maxsss,p10=s_p10)    
    } else {
      df = cbind(proj$comid,NA,NA,NA,NA)
      df[rowidx,2] = as.array(projpred)
      df[rowidx,3] = s_mtp
      df[rowidx,4] = s_maxsss
      df[rowidx,5] = s_p10
      df = as.data.frame(df)
      colnames(df) = c('comid','probability','mtp','maxsss','p10')
    }
    
    ofilename = sprintf('%s/%s_binary_predictions_baseline.csv',dirpath,spnamedata$name[i])
    write.csv(df,ofilename,row.names=FALSE)
  }
  print('base scenario projection complete')
  # b) project to future reservoir scenario bg data using the calibrated model
  # read in future reservoir scenario data
  {
    projectionareatffilename = sprintf('./spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine_gcm_reservoir',gcmver,'future',spnamedata$name[i]) # future reservoir proj
    projtf = fread(projectionareatffilename)
    
    projectionareaflowfilename = sprintf('./spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'future',spnamedata$name[i])
    projflow = fread(projectionareaflowfilename)
    proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c','dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c','dd150_10c','avgtemp')],
                 projflow[,c('maxflow','maxflowdate','minflow','minflowdate','avgflow','maxminflowdiff')])
    
    # get rid of points with low netsymmetricdifference value 
    if(nolownetsymdiff)
    {
      proj = proj[which(proj$wbmID_30sec_netsymdiff<threshold),]
    }
    if(length(which(is.na(proj$BFI)))>0)
    {
      proj = proj[-which(is.na(proj$BFI)),]
    }
    # exclude rows with streamorder value of -9
    if(length(which(proj$streamorder==-9))>0)
    {
      proj = proj[-which(proj$streamorder==-9),]
    }
    if(length(which(is.na(proj$slope)))>0)
    {
      proj = proj[-which(is.na(proj$slope)),]    
    }
    
    if(dataversion == 1)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,numday_above_tmax,
                                      numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                      dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                      dd150_10c,maxflow,maxflowdate,minflow,
                                      minflowdate,avgtemp,streamorder,slope))    
    } else if(dataversion == 2)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,
                                      numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
    } else if(dataversion == 3)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,
                                      numday_above_tmax,dd90_10c,minflow,minflowdate,maxflowdate,avgflow,slope))
    }
    
    # predict presence probability on projection area
    if(class(mod.seq)[1]=='MaxEnt')
    {
      projpred = rmaxent::project(mod.seq,proj[,2:ncol(proj)])  
      projpred = projpred$prediction_cloglog
    } else {
      projpred = predict(mod.seq,proj[,2:ncol(proj)],type = e@other.settings$pred.type, 
                         clamp = e@other.settings$doClamp, e@other.settings$other.args)
    }
    
    # predict binary presence based on mtp
    s_mtp = rep(0,length(projpred))
    s_mtp[which(projpred>mtp)] = 1
    s_mtp[which(projpred<=mtp)] = 0
    
    # maxsss
    s_maxsss = rep(0, length(projpred))  
    s_maxsss[which(projpred>maxsss)] = 1
    s_maxsss[which(projpred<=maxsss)] = 0
    
    # 10p=10th percentile of presence porbability 
    s_p10 = rep(0, length(projpred))
    s_p10[which(projpred>p10)] = 1
    s_p10[which(projpred<=p10)] = 0
    
    # save the prediction and the calibrated model details
    #df = cbind(projpred,s_mtp,s_maxsss,s_p10)
    rowidx = as.numeric(row.names(projpred))
    if(class(mod.seq)[1]=='MaxEnt')
    {
      df = data.frame(comid=proj$comid,probability=projpred,mtp=s_mtp,maxsss=s_maxsss,p10=s_p10)    
    } else {
      df = cbind(proj$comid,NA,NA,NA,NA)
      df[rowidx,2] = as.array(projpred)
      df[rowidx,3] = s_mtp
      df[rowidx,4] = s_maxsss
      df[rowidx,5] = s_p10
      df = as.data.frame(df)
      colnames(df) = c('comid','probability','mtp','maxsss','p10')
    }
    
    ofilename = sprintf('%s/%s_binary_predictions_futureres.csv',dirpath,spnamedata$name[i])
    write.csv(df,ofilename,row.names=FALSE)
  }
  print('future reservoir scenario projection complete')
  # c) project to current pristine scenario bg data using the calibrated model
  {
    projectionareatffilename = sprintf('./spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'current',spnamedata$name[i]) # current pristine proj  
    projtf = fread(projectionareatffilename)
    proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c',
                                   'dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c',
                                   'dd150_8c','dd150_10c','avgtemp','maxflow','maxflowdate',
                                   'minflow','minflowdate','avgflow','maxminflowdiff')])
    
    # get rid of points with low netsymmetricdifference value 
    if(nolownetsymdiff)
    {
      proj = proj[which(proj$wbmID_30sec_netsymdiff<threshold),]
    }
    if(length(which(is.na(proj$BFI)))>0)
    {
      proj = proj[-which(is.na(proj$BFI)),]
    }
    # exclude rows with streamorder value of -9
    if(length(which(proj$streamorder==-9))>0)
    {
      proj = proj[-which(proj$streamorder==-9),]
    }
    if(length(which(is.na(proj$slope)))>0)
    {
      proj = proj[-which(is.na(proj$slope)),]    
    }
    
    if(dataversion == 1)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,numday_above_tmax,
                                      numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                      dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                      dd150_10c,maxflow,maxflowdate,minflow,
                                      minflowdate,avgtemp,streamorder,slope))    
    } else if(dataversion == 2)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,
                                      numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
    } else if(dataversion == 3)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,
                                      numday_above_tmax,dd90_10c,minflow,minflowdate,maxflowdate,avgflow,slope))
    }
    
    # predict presence probability on projection area
    if(class(mod.seq)[1]=='MaxEnt')
    {
      projpred = rmaxent::project(mod.seq,proj[,2:ncol(proj)])  
      projpred = projpred$prediction_cloglog
    } else {
      projpred = predict(mod.seq,proj[,2:ncol(proj)],type = e@other.settings$pred.type, 
                         clamp = e@other.settings$doClamp, e@other.settings$other.args)
    }
    
    # predict binary presence based on mtp
    s_mtp = rep(0,length(projpred))
    s_mtp[which(projpred>mtp)] = 1
    s_mtp[which(projpred<=mtp)] = 0
    
    # maxsss
    s_maxsss = rep(0, length(projpred))  
    s_maxsss[which(projpred>maxsss)] = 1
    s_maxsss[which(projpred<=maxsss)] = 0
    
    # 10p=10th percentile of presence porbability 
    s_p10 = rep(0, length(projpred))
    s_p10[which(projpred>p10)] = 1
    s_p10[which(projpred<=p10)] = 0
    
    # save the prediction and the calibrated model details
    #df = cbind(projpred,s_mtp,s_maxsss,s_p10)
    rowidx = as.numeric(row.names(projpred))
    if(class(mod.seq)[1]=='MaxEnt')
    {
      df = data.frame(comid=proj$comid,probability=projpred,mtp=s_mtp,maxsss=s_maxsss,p10=s_p10)    
    } else {
      df = cbind(proj$comid,NA,NA,NA,NA)
      df[rowidx,2] = as.array(projpred)
      df[rowidx,3] = s_mtp
      df[rowidx,4] = s_maxsss
      df[rowidx,5] = s_p10
      df = as.data.frame(df)
      colnames(df) = c('comid','probability','mtp','maxsss','p10')
    }
    
    ofilename = sprintf('%s/%s_binary_predictions_currentpri.csv',dirpath,spnamedata$name[i])
    write.csv(df,ofilename,row.names=FALSE)
  }
  print('current pristine scenario projection complete')
  # d) project to future pristine scenario bg data using the calibrated model
  {
    projectionareatffilename = sprintf('./spdata/projection_area/by_sp/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,'future',spnamedata$name[i]) # current pristine proj  
    projtf = fread(projectionareatffilename)
    proj = cbind(projntf,projtf[,c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c',
                                   'dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c',
                                   'dd150_8c','dd150_10c','avgtemp','maxflow','maxflowdate',
                                   'minflow','minflowdate','avgflow','maxminflowdiff')])
    
    # get rid of points with low netsymmetricdifference value 
    if(nolownetsymdiff)
    {
      proj = proj[which(proj$wbmID_30sec_netsymdiff<threshold),]
    }
    if(length(which(is.na(proj$BFI)))>0)
    {
      proj = proj[-which(is.na(proj$BFI)),]
    }
    # exclude rows with streamorder value of -9
    if(length(which(proj$streamorder==-9))>0)
    {
      proj = proj[-which(proj$streamorder==-9),]
    }
    if(length(which(is.na(proj$slope)))>0)
    {
      proj = proj[-which(is.na(proj$slope)),]    
    }
    
    if(dataversion == 1)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,numday_above_tmax,
                                      numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                      dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                      dd150_10c,maxflow,maxflowdate,minflow,
                                      minflowdate,avgtemp,streamorder,slope))    
    } else if(dataversion == 2)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,
                                      numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope))
    } else if(dataversion == 3)
    {
      proj = data.frame(dplyr::select(proj,comid,BFI,waterbody,streamorder,
                                      numday_above_tmax,dd90_10c,minflow,minflowdate,maxflowdate,avgflow,slope))
    }
    
    # predict presence probability on projection area
    if(class(mod.seq)[1]=='MaxEnt')
    {
      projpred = rmaxent::project(mod.seq,proj[,2:ncol(proj)])  
      projpred = projpred$prediction_cloglog
    } else {
      projpred = predict(mod.seq,proj[,2:ncol(proj)],type = e@other.settings$pred.type, 
                         clamp = e@other.settings$doClamp, e@other.settings$other.args)
    }

    # predict binary presence based on mtp
    s_mtp = rep(0,length(projpred))
    s_mtp[which(projpred>mtp)] = 1
    s_mtp[which(projpred<=mtp)] = 0
    
    # maxsss
    s_maxsss = rep(0, length(projpred))  
    s_maxsss[which(projpred>maxsss)] = 1
    s_maxsss[which(projpred<=maxsss)] = 0
    
    # 10p=10th percentile of presence porbability 
    s_p10 = rep(0, length(projpred))
    s_p10[which(projpred>p10)] = 1
    s_p10[which(projpred<=p10)] = 0
    
    # save the prediction and the calibrated model details
    #df = cbind(projpred,s_mtp,s_maxsss,s_p10)
    rowidx = as.numeric(row.names(projpred))
    if(class(mod.seq)[1]=='MaxEnt')
    {
      df = data.frame(comid=proj$comid,probability=projpred,mtp=s_mtp,maxsss=s_maxsss,p10=s_p10)    
    } else {
      df = cbind(proj$comid,NA,NA,NA,NA)
      df[rowidx,2] = as.array(projpred)
      df[rowidx,3] = s_mtp
      df[rowidx,4] = s_maxsss
      df[rowidx,5] = s_p10
      df = as.data.frame(df)
      colnames(df) = c('comid','probability','mtp','maxsss','p10')
    }
    
    ofilename = sprintf('%s/%s_binary_predictions_futurepri.csv',dirpath,spnamedata$name[i])
    write.csv(df,ofilename,row.names=FALSE)
  }
  print('current future scenario projection complete')
  
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}
runtimefilename = sprintf('./sdm_results/%s/runtime%d.csv',gcmver,arrayid)
runtime = proc.time() - start
write.csv(data.frame(elapsed_sec=runtime[3],finish=Sys.time()),runtimefilename,row.names=FALSE)
