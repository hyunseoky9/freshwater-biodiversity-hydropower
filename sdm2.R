# running SDM with 
#resource:
# Taylor et al. 2020
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md (for splitting occ data into
# test and training data)
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
#Choose descriptive name for model run.  
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
nolownetsymdiff = 1
threshold = 0.4

# select first 200 species with the most number of occpts with netsymdiff score >0.4
numoccs = c()
{
for ( i in 1:nrow(spnamedata))
{
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  spdata = read.csv(spdatafilename)
  numoccs[i] = length(which(spdata$wbmID_30sec_netsymdiff < threshold))
}
sorted  = sort(numoccs,decreasing=T)
idx0 = which(sorted>=30)
idx0 = idx0[length(idx0)]
best100 = order(numoccs,decreasing=T)[1:idx0]

best = c(362)
ptm <- proc.time()
for( i in best100)
{
  # read in data
    #spdatafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens.csv'
    #projectionareafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens_projarea.csv'
    spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    projectionareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
    sp = read.csv(spdatafilename)
    proj = fread(projectionareafilename)
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
    sp$udd2[which(is.na(sp$udd2))] = 1000000
    proj$udd2[which(is.na(proj$udd2))] = 1000000
    sp$BFI[which(is.na(sp$BFI))] = 0
    proj$BFI[which(is.na(proj$BFI))] = 0
    # change mop field into integer values (categorical)
    proj$mop[which(proj$mop=='(null)')] = NA
    moptypes = unique(proj$mop)[which(!is.na(unique(proj$mop)))]
    if(length(moptypes)>0)
    {
      moptypes = data.frame(moptypes = moptypes,code = 1:length(moptypes))
      proj$mop = match(proj[,'mop'],moptypes[,1])
    }
    proj$mop[which(is.na(proj$mop))] = 0
    proj$mop = as.factor(proj$mop)  
    
    sp$mop[which(sp$mop=='(null)')] = NA
    moptypes = unique(sp$mop)[which(!is.na(unique(sp$mop)))]
    if(length(moptypes)>0)
    {
      moptypes = data.frame(moptypes = moptypes,code = 1:length(moptypes))
      sp$mop = match(sp[,'mop'],moptypes[,1])
    }
    sp$mop[which(is.na(sp$mop))] = 0
    sp$mop = as.factor(sp$mop)
    
    # presence 
    p = rep(0, length(proj$comid))
    p[which(!is.na(match(proj$comid,sp$comid)))] = 1 # presence
    proj$p = p
    sp$p = 1
  }
  #making occurence and background data subsets (used later in model evaluation)
  #presdata <- filter(sp, get('p') ==1)
  bckdata <- filter(proj, get('p') ==0)
  
  
  # predictor
  dataversion = 4
  if(dataversion == 1)
  {
    bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
                                   numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                   dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                   dd150_10c,maxflow,maxflowdate,minflow,
                                   minflowdate,avgtemp,streamorder))
    spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
                                     numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                     dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                     dd150_10c,maxflow,maxflowdate,minflow,
                                     minflowdate,avgtemp,streamorer))
    path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df1',spnamedata$name[i])
  } else if(dataversion == 2)
  {
    bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
                                   dd90_8c,minflow,maxflowdate,
                                   minflowdate))
    
    
    spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
                                     dd90_8c,minflow,maxflowdate,
                                     minflowdate))
    path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df2',spnamedata$name[i])
  } else if(dataversion == 3)
  {
    bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
                                       numday_below_tmin,dd120_10c,avgflow,maxflowdate,
                                       minflowdate,streamorder))
    spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
                                    numday_below_tmin,dd120_10c,avgflow,maxflowdate,
                                    minflowdate,streamorder))
    
    #bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
    #                                   numday_below_tmin,dd120_10c,minflow,maxflowdate,
    #                                   minflowdate,streamorder))
    #spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
    #                                numday_below_tmin,dd120_10c,minflow,maxflowdate,
    #                                minflowdate,streamorder))
    path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df3',spnamedata$name[i])
  } else if(dataversion == 4)
  {
    bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,streamorder))
    spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,streamorder))
    
    #bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
    #                                   numday_below_tmin,dd120_10c,minflow,maxflowdate,
    #                                   minflowdate,streamorder))
    #spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
    #                                numday_below_tmin,dd120_10c,minflow,maxflowdate,
    #                                minflowdate,streamorder))
    path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df3',spnamedata$name[i])
  }
  
  
  if(nolownetsymdiff)
  {
    path2 = paste(path2,sprintf('_lownetsymdiff%.2f_testtest',threshold),sep='')  
  }

  #** running maxent
  
  
  #Paste Maxent.jar (v 3.4.1.) into the java folder of the dismo package at this location & code checks to make sure it will run
  system.file("java", package="dismo")
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
  #if (file.exists(jar) & require(rJava))
  
    #Creating output folder path
  
  #setwd('G:/My Drive/research/sdm_modeling/sdm_results')
  #savepath <- dir.create(eval(modelname))
  
  # split occurrence data into testing and training data
  # get the same random sample for training and testing
  set.seed(1)
  
  # randomly select 75% for training
  selected <- sample(1:nrow(sp), nrow(sp) * 0.75)
  
  occ_train <- spdf[selected,]  # this is the selection to be used for model training
  occ_test <- spdf[-selected,]    # this is the opposite of the selection which will be used for model testing
  
  # randomly select 10000 background points for maxent
  if(nrow(bckdata)<10000)
  {
    samplesize = nrow(bckdata)
  } else {
    samplesize = 10000  
  }
  bg = bckdata[sample(1:nrow(bckdata), samplesize),]
  
  occbg = rbind(occ_train,bg)
  occbgp = c(rep(1,nrow(occ_train)),rep(0,nrow(bg)))
  
  #Creating argument list for Maxent in dismo, including RM values from above
  RMargs <- c(paste0("allowpartialdata=true"))
  RMargs <- c(RMargs, paste0("writeplotdata=TRUE"))
  RMargs <- c(RMargs, paste0("removeDuplicates=true"))
  RMargs <- c(RMargs, paste0("responsecurves=true"))
  RMargs <- c(RMargs, paste0("Jackknife=true"))
  RMargs <- c(RMargs, paste0("writebackgroundpredictions=true"))
  #other useful args: randomtestpoints(int),randomseed(false),
  file.exists(jar)
  require(rJava)
  
  #nbgval = nrow(df)
  
  # 1. running maxent by separating test/train outside of the maxent function.
  # res: https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
  #testidx = which(p==1)[occ_test]
  #p_train = p[-testidx]
  #df_train = df[-testidx,]
  #df_bg = df_train[which(p_train==0),]
  #options(java.parameters = "-Xmx4g" ) # memory allocation.
  #path1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_maxent_output_pristinerun',spnamedata$name[i])
  #Descriptive_Model_Name <-maxent(occbg, occbgp, args=RMargs,
  #                                path=path1, 
  #                                silent=FALSE)
  
  
  # 1.1 first evaluate with training data to see if you get same auc
  #mod_eval_train <- dismo::evaluate(p = occ_train, a = bg, model = Descriptive_Model_Name) 
  #print(mod_eval_train)
  # 1.2evaluate the model with test data
  #mod_eval_train_test <- dismo::evaluate(p = occ_test, a = bg, model = Descriptive_Model_Name) 
  #print(mod_eval_train_test)
  
  # 2.running maxent by separating test/train inside of the maxent function. (not using it for sdm2.R) 
  
  RMargs2 = RMargs
  RMargs2 = c(RMargs2,paste0(sprintf("randomtestpoints=%d",25)))
  RMargs2 = c(RMargs2,paste0("randomseed=TRUE"))
  
  Descriptive_Model_Name <-maxent(occbg, occbgp, args=RMargs2,
                                  path=path2, 
                                  silent=FALSE)
  
  #Variable contributions and response curves (not plotting it.)
  #plot(Descriptive_Model_Name)
  #response(Descriptive_Model_Name)
  
  #######
  ##############################
  ####Projecting model back onto input data to save predictions to segments
  ###https://rdrr.io/github/johnbaums/rmaxent/f/README.md
  #####################################
  
  #library(devtools)
  #install_github('johnbaums/rmaxent')
  #library(rmaxent)
  #predictpres <- rmaxent::project(Descriptive_Model_Name, presdata)
  #predictpresdf <-as.data.frame(predictpres)
  
  #predictbck <- rmaxent::project(Descriptive_Model_Name, bckdata)
  #predictbckdf <- as.data.frame(predictbck)
  comid_addon = proj$comid
  p_addon = proj$p
  predall <- project(Descriptive_Model_Name, proj)
  predalldf <- as.data.frame(predall)
  #max(predalldf$prediction_cloglog)
  #min(predalldf$prediction_cloglog)
  
  # suitability data based on minimum training presence (MTP)
  mtp = min(predalldf$prediction_cloglog[which(p_addon==1)])
  #hist(predalldf$prediction_cloglog[which(proj$p==0)],breaks=10)
  #mean(predalldf$prediction_cloglog[which(proj$p==1)])
  #mean(predalldf$prediction_cloglog[which(proj$p==0)])
  #mean(predalldf$prediction_cloglog)
  #hist(predalldf$prediction_cloglog)
  s_mtp = rep(0,nrow(predalldf))
  s_mtp[which(predalldf$prediction_cloglog>mtp)] = 1
  
  # Maxsss 
  # res: Liu et al. 2013; Frans 2018
  filename = sprintf('%s/maxentResults.csv',path2)
  res_df <- read.csv(filename, header=TRUE)
  max3strain = res_df$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold # this is maxsss. decide whether to do training or test.
  max3stest = res_df$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold
  s_max3s = rep(0,nrow(predalldf))
  s_max3s[which(predalldf$prediction_cloglog>mtp)] = 1
  
  
  # write out put of the binary prediction and cloglog probability.
  o = data.frame(comid=comid_addon,probability=predalldf$prediction_cloglog,maxsss=s_max3s,mtp=s_mtp)
  ofilename = sprintf('%s/%s_binary_predictions.csv',path2,spnamedata$name[i])
  
  write.csv(o,ofilename,row.names=FALSE)
  # for graphic output, use sdmresult_graph.R
  print(sprintf('%d/%d done',which(i==best100),length(best100)))
}
beep(sound=5)
proc.time() - ptm

