#get all the temperature and flow related predictor values for each species in the interested points.
# difference from predictor_calc3: calculates only the temperature related predictors for reservoir scenarios since reservoir only affects temperature not flow in my model.
rm(list = ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
hucnum = 18
###when doing all the species, get an array of tmin and tmax and spnames
spname = spdata$name
tmins = spdata$tmin
tmaxs = spdata$tmax #dummy value for now #spnamedata$tmax

# just for freshwater darter
#idx = which(spdata$name == 'Aplodinotus grunniens')
#sp = spdata$name[idx]
#tmins = spdata$tmin[idx]
#tmaxs = spdata$tmax[idx]

# get tmin and tmax that occur more than 10 times in the species data. 
tmin_count = unique(table(spdata$tmin))
tmax_count = unique(table(spdata$tmax))
tmin.common = apply(as.matrix(tmin_count[tmin_count>10]),1,function(x) which(table(spdata$tmin)==x))
tmin.common = as.numeric(names(table(spdata$tmin)[tmin.common]))
tmax.common = apply(as.matrix(tmax_count[tmax_count>10]),1,function(x) which(table(spdata$tmax)==x))
tmax.common = as.numeric(names(table(spdata$tmax)[tmax.common]))

# get all tmin/tmaxs sorted
tmins = sort(unique(spdata$tmin))
tmaxs = sort(unique(spdata$tmax))


# 1. get some of the temperature predictor values and flow related predictor values for all hucs
# get the max min temp related temperature predictors for some of the common tmin and tmax values. 
# predictor values that will be calculated here are 
#1. average maximum number of days exceeding maximum temperature threshold in a year
#2. average maximum number of days going below minimum temperature threshold in a year
#3-11. average growing degree days by 90, 120, 150 Julian days with
#5,8, and 10 degree celsius as base temp.
#12. BFI= base flow index
#13-14. average date and amount of annual maximum flow
#15-16. average date an amount of annual minimum flow
irange = 1:18
reservoirmode = 1 # if making predictor variables with reservoir effect, this var is 1.
currentofuture = 'future'
gcmver = 'DOE-CNRM-ESM2-1'
if(currentofuture=='current')
{
  start = 2000
  finish = 2019
} else {
  start = 2060
  finish = 2079
}

if(reservoirmode)
{
  ifiledir = sprintf('D:/sdm_modeling/wbm/%s/pristine_gcm_reservoir/%s',gcmver, currentofuture)
  ofiledir = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/tempflow_predictors_by_huc/%s/pristine_gcm_reservoir/%s',gcmver,currentofuture)
} else {
  ifiledir = sprintf('D:/sdm_modeling/wbm/%s/pristine w gcm/%s',gcmver,currentofuture)
  ofiledir = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/tempflow_predictors_by_huc/%s/pristine w gcm/%s',gcmver, currentofuture)
}
for(i in 1)#:10) #irange)
{
  path2 ='G:/My Drive/research/sdm_modeling/wbmdata/pristine/reference_table'
  reffilename = sprintf('%s/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv',path2,i)
  ref = read.csv(reffilename)
  cellnum = nrow(ref)
  
  for(j in 1:(finish-start + 1))
  {
    # access the temperature data for those points for the relevant timeframe calculate the temperature and flow related predictor variables:
    print(j)
    yr = (j-1) + start
    tempfilename = sprintf('%s/Huc%d_CONUS_Output_RiverTemperature_%s-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',ifiledir,i,gcmver,yr)
    tempd =fread(tempfilename)
    print('tempd read')
    names(tempd)[1] = 'ID'
    
    if(nrow(tempd) != cellnum)
    {
      print(sprintf("TEMP DATA CELL COUNT DIFFERENT!! huc%d yr %d",i,j))
    }
    
    if(!reservoirmode)
    {
      flowfilename = sprintf('%s/Huc%d_CONUS_Output_RiverDischarge_%s-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',ifiledir,i,gcmver,yr)
      flowd = fread(flowfilename, header=TRUE)
      print('flowd read')
      names(flowd)[1] = 'ID'
      if(nrow(flowd)!=cellnum)
      {
        print(sprintf("DISCHARGE DATA CELL COUNT DIFFERENT!! huc%d yr%d",i,j))
      }
    }
    
    
    # calculate 1. for the year j.
    numday_above_tmaxs = c()
    all0s_fromhere = 0
    for(k in tmaxs)
    {
      if(all0s_fromhere==0)
      {
        matwhich = apply(cbind(rep(k-1,nrow(tempd)),as.matrix(tempd[,2:ncol(tempd)]),rep(k-1,nrow(tempd))),1,function(x) which(x<k))
        if("list" %in% class(matwhich)) # means all the temps 
        {
          numday_above_tmax = sapply(matwhich,function(x) max(x[2:length(x)] - x[1:(length(x)-1)])-1)  
        } else 
        {
          all0s_fromhere = 1
          numday_above_tmax = rep(0,nrow(tempd))
        }
      } else 
      {
        numday_above_tmax = rep(0,nrow(tempd))
      }
      numday_above_tmaxs = cbind(numday_above_tmaxs,numday_above_tmax)
    }
    # calculate 2. for the year j.
    numday_below_tmins = c()
    for(k in tmins)
    {
      ptm = proc.time()
      mat= cbind(as.matrix(tempd[,2:ncol(tempd)])<k,rep(0,nrow(tempd)))
      numday_below_tmin = apply(mat,1,sum)
      numday_below_tmins = cbind(numday_below_tmins,numday_below_tmin)
    }
    # calculate 3-11 for the year j.
    dd90_5c =  apply(as.matrix(tempd[,2:(90+1)])-5,1,function(x) sum(x[which(x>=0)])) # degree day by 90 with 5C as base temp
    dd90_8c =  apply(as.matrix(tempd[,2:(90+1)])-8,1,function(x) sum(x[which(x>=0)])) # degree day by 90 with 8C as base temp
    dd90_10c =  apply(as.matrix(tempd[,2:(90+1)])-10,1,function(x) sum(x[which(x>=0)])) # degree day by 90 with 10C as base temp
    dd120_5c = apply(as.matrix(tempd[,2:(120+1)])-5,1,function(x) sum(x[which(x>=0)])) # degree day by 120
    dd120_8c = apply(as.matrix(tempd[,2:(120+1)])-8,1,function(x) sum(x[which(x>=0)])) # degree day by 120
    dd120_10c = apply(as.matrix(tempd[,2:(120+1)])-10,1,function(x) sum(x[which(x>=0)])) # degree day by 120
    dd150_5c = apply(as.matrix(tempd[,2:(150+1)])-5,1,function(x) sum(x[which(x>=0)])) # degree day by 150
    dd150_8c = apply(as.matrix(tempd[,2:(150+1)])-8,1,function(x) sum(x[which(x>=0)])) # degree day by 150
    dd150_10c = apply(as.matrix(tempd[,2:(150+1)])-10,1,function(x) sum(x[which(x>=0)])) # degree day by 150
    
    #avg temp
    avgtemp = apply(as.matrix(tempd[,2:ncol(tempd)]),1,mean)
    
    # calculate 13-16 for the year j
    if(!reservoirmode)
    {
      maxflow = apply(as.matrix(flowd[,2:ncol(flowd)]),1,max)
      maxflowdate = apply(as.matrix(flowd[,2:ncol(flowd)]),1,function(x) min(which(x==max(x))))
      minflow = apply(as.matrix(flowd[,2:ncol(flowd)]),1,min)
      minflowdate = apply(as.matrix(flowd[,2:ncol(flowd)]),1,function(x) min(which(x==min(x))))
      avgflow = apply(as.matrix(flowd[,2:ncol(flowd)]),1,mean)
      maxminflowdiff = maxflow - minflow
    }
    
    # make the data frame with all the predictor variables in the list under the species' name
    if(j==1){
      if(!reservoirmode)
      {
        pred_perhuc = data.frame(wbmID_1min=tempd$ID,
                                 numday_above_tmaxs,numday_below_tmins,
                                 dd90_5c, dd90_8c, dd90_10c, dd120_5c, dd120_8c,
                                 dd120_10c, dd150_5c, dd150_8c, dd150_10c,avgtemp,
                                 maxflow, maxflowdate, minflow,
                                 minflowdate,avgflow,maxminflowdiff)
      } else {
        pred_perhuc = data.frame(wbmID_1min=tempd$ID,
                                 numday_above_tmaxs,numday_below_tmins,
                                 dd90_5c, dd90_8c, dd90_10c, dd120_5c, dd120_8c,
                                 dd120_10c, dd150_5c, dd150_8c, dd150_10c,avgtemp)
      }
      # get column names for numday above tmax and numday below tmin preds.
      names(pred_perhuc)[which(grepl('numday_above_tmax',names(pred_perhuc)))] = sprintf('numday_above_tmax_%.3f',tmaxs)
      names(pred_perhuc)[which(grepl('numday_below_tmin',names(pred_perhuc)))] = sprintf('numday_below_tmin_%.3f',tmins)
      
    }else{ # add the data if there's data in there already to ultimately calculate the average rate per yr.
      if(!reservoirmode)
      {
        pred_perhuc[,2:ncol(pred_perhuc)] =
          pred_perhuc[,2:ncol(pred_perhuc)] +
          data.frame(
            numday_above_tmaxs,numday_below_tmins, 
            dd90_5c, dd90_8c, dd90_10c, dd120_5c, dd120_8c,
            dd120_10c, dd150_5c, dd150_8c, dd150_10c,avgtemp,
            maxflow, maxflowdate, minflow,
            minflowdate,avgflow,maxminflowdiff)
      } else {
        pred_perhuc[,2:ncol(pred_perhuc)] =
          pred_perhuc[,2:ncol(pred_perhuc)] +
          data.frame(
            numday_above_tmaxs,numday_below_tmins, 
            dd90_5c, dd90_8c, dd90_10c, dd120_5c, dd120_8c,
            dd120_10c, dd150_5c, dd150_8c, dd150_10c,avgtemp)
      }
    }
    print(dim(pred_perhuc))
    print('predictor for the year added to the dataframe')
    print(sprintf('year %d done',yr))
  }
  # divide the sums of predictors with the number of years to get the average
  pred_perhuc[,2:ncol(pred_perhuc)] =
    pred_perhuc[,2:ncol(pred_perhuc)]/
    (finish - start + 1) # divide by data frame by number of years to get average.
  # write out the data in a separate file
  ofilename = sprintf('%s/huc%d_projarea_tempNflow_predictors.csv',ofiledir,i)
  write.csv(pred_perhuc,ofilename,row.names=FALSE)
  print(sprintf('huc%d done',i))
}









# 2. get predictor values for each species using their tmin tmax data. 
# for common tmin and tmax value, make a predictor value for all hucs 
# to not recalculate same values many times. 
rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
tmins = spdata$tmin
tmaxs = spdata$tmax

reservoirmode = 1
currentofuture = 'future'
gcmver = 'DOE-CNRM-ESM2-1'
if(currentofuture=='current')
{
  start = 2000
  finish = 2019
} else {
  start = 2060
  finish = 2079
}

if(!reservoirmode)
{
  ifiledir = sprintf('D:/sdm_modeling/wbm/%s/pristine w gcm/%s',gcmver,currentofuture) # for gettingg processed wbm temp flow data
  ofilepath = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/pristine w gcm/%s/%s',gcmver,currentofuture) #'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp'
  tempflowsourcepath = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/tempflow_predictors_by_huc/%s/pristine w gcm/%s',gcmver,currentofuture)
} else {
  ifiledir = sprintf('D:/sdm_modeling/wbm/%s/pristine_gcm_reservoir/%s',gcmver,currentofuture)
  ofilepath = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/pristine_gcm_reservoir/%s/%s',gcmver,currentofuture) #'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp'
  tempflowsourcepath = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/tempflow_predictors_by_huc/%s/pristine_gcm_reservoir/%s',gcmver,currentofuture)
}

tempodir = 0 # 1=change ofilepath to D drive.
if(tempodir)
{
  if(!reservoirmode)
  {
    ofilepath = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/pristine w gcm/%s/%s',gcmver,currentofuture) #'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp'
  } else {
    ofilepath = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/pristine_gcm_reservoir/%s/%s',gcmver,currentofuture) #'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp'
  }
}
# pre-load all the by_huc temp&flow predictors
hucd = list()
for(i in 1:18)
{
  filename = sprintf('%s/huc%d_projarea_tempNflow_predictors.csv',tempflowsourcepath,i)
  hucd[[i]] = read.csv(filename)
  print(sprintf('huc%d data ready',i))
}
#beep(sound=5)
  
for(i in 1:nrow(spdata))
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/only_ids/%s_projarea_onlyids.csv',spdata$name[i])
  tmin = tmins[i]
  tmax = tmaxs[i]
  spproj = fread(path1)
  hucs = unique(spproj$huc2)

  spproj$numday_above_tmax = NA
  spproj$numday_below_tmin = NA
  spproj$dd90_5c = NA
  spproj$dd90_8c = NA
  spproj$dd90_10c = NA
  spproj$dd120_5c = NA
  spproj$dd120_8c = NA
  spproj$dd120_10c = NA
  spproj$dd150_5c = NA
  spproj$dd150_8c = NA
  spproj$dd150_10c = NA
  spproj$avgtemp = NA
  if(!reservoirmode)
  {
    spproj$maxflow = NA
    spproj$maxflowdate = NA
    spproj$minflow = NA
    spproj$minflowdate = NA
    spproj$avgflow = NA
    spproj$maxminflowdiff = NA
  }
  
  for(j in 1:length(hucs))
  {
    calc_predictor1_separately = 0
    calc_predictor2_separately = 0
    #filename = sprintf('%s/huc%d_projarea_tempNflow_predictors.csv',tempflowsourcepath,hucs[j])
    idx = match(spproj$wbmID_1min[which(spproj$huc2==hucs[j])], hucd[[hucs[j]]]$wbmID_1min)
    #1. average maximum number of days exceeding maximum temperature threshold in a year
    # load it from the predictor data by_huc if the species' tmax is in the common tmaxs.
    if (tmax %in% tmaxs) # tmax of the sp is one of the common tmaxs
    {
      numdayabove_idx = which(grepl('numday_above_tmax',names(hucd[[hucs[j]]])))[1]
      tmaxs_in_colnames = as.numeric(sub('numday_above_tmax_','',names(hucd[[hucs[j]]])[which(grepl('numday_above_tmax',names(hucd[[hucs[j]]])))]))
      numdayabove_idx = numdayabove_idx + which(abs(tmax - tmaxs_in_colnames)== min(abs(tmax-tmaxs_in_colnames))) - 1
      #tmaxidx = which(tmaxs == tmax)
      spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]][idx,numdayabove_idx]
    } else
    {
      calc_predictor1_separately = 1
    }
    #2. average maximum number of days going below minimum temperature threshold in a year
    # load it from the predictor data by_huc if the species' tmin is in the common tmins.
    if (tmin %in% tmins) # tmax of the sp is one of the common tmaxs
    {
      numdaybelow_idx = which(grepl('numday_below_tmin',names(hucd[[hucs[j]]])))[1]
      names(hucd[[hucs[j]]])[which(grepl('\\.2\\.000',names(hucd[[hucs[j]]])))] = 'numday_below_tmin_-2.000'
      tmins_in_colnames = as.numeric(sub('numday_below_tmin_','',names(hucd[[hucs[j]]])[which(grepl('numday_below_tmin',names(hucd[[hucs[j]]])))]))
      numdaybelow_idx = numdaybelow_idx + which(abs(tmin - tmins_in_colnames)== min(abs(tmin-tmins_in_colnames))) - 1
      #tmaxidx = which(tmaxs == tmax)
      spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]][idx,numdaybelow_idx]
    } else
    {
      calc_predictor2_separately = 1
    }
    
    # get rest of the predictors from by_huc data.
    spproj$dd90_5c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd90_5c[idx]
    spproj$dd90_8c[which(spproj$huc2==hucs[j])]  = hucd[[hucs[j]]]$dd90_8c[idx]
    spproj$dd90_10c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd90_10c[idx]
    spproj$dd120_5c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd120_5c[idx]
    spproj$dd120_8c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd120_8c[idx]
    spproj$dd120_10c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd120_10c[idx]
    spproj$dd150_5c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd150_5c[idx]
    spproj$dd150_8c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd150_8c[idx]
    spproj$dd150_10c[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$dd150_10c[idx]
    spproj$avgtemp[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$avgtemp[idx]
    if(!reservoirmode)
    {
      spproj$maxflow[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$maxflow[idx]
      spproj$maxflowdate[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$maxflowdate[idx]
      spproj$minflow[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$minflow[idx]
      spproj$minflowdate[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$minflowdate[idx]
      spproj$avgflow[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$avgflow[idx]
      spproj$maxminflowdiff[which(spproj$huc2==hucs[j])] = hucd[[hucs[j]]]$maxminflowdiff[idx]
    }
    
    # calculate predictor 1 and 2 if the species tmin and tmax are not in common tmin /tmax
    if (calc_predictor2_separately + calc_predictor1_separately > 0)
    {
      path2 ='G:/My Drive/research/sdm_modeling/wbmdata/pristine/reference_table'
      reffilename = sprintf('%s/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv',path2,hucs[j])
      ref = read.csv(reffilename)
      cellnum = nrow(ref)
      for(k in 1:(finish-start + 1))
      {
        # access the temperature data for those points for the relevant timeframe calculate the temperature and flow related predictor variables:
        yr = (k-1) + start
        tempfilename = sprintf('%s/Huc%d_CONUS_Output_RiverDischarge_DOE-ACCESS-CM2-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',ifiledir,hucs[j],yr)
        tempd =fread(tempfilename,header=TRUE)
        names(tempd)[1] = 'ID'
        
        
        if(nrow(tempd) != cellnum)
        {
          print(sprintf("TEMP DATA CELL COUNT DIFFERENT!! huc%d yr %d",hucs[j],yr))
        }
        
        # calculate 1. for the year k
        if(calc_predictor1_separately)
        {
          mat = cbind(as.matrix(tempd[,2:ncol(tempd)])>tmax,rep(0,nrow(tempd)))
          cum = apply(mat,1,cumsum)
          numday_above_tmax = apply(cum,2,function(x) unique(x[duplicated(x)]))
          if(class(numday_above_tmax)=='list')
          {
            numday_above_tmax = unlist(lapply(numday_above_tmax,function(x) max(x[2:length(x)] - x[1:(length(x)-1)]))) # Average maximum number of days in a year that exceeds tmax
            numday_above_tmax[which(is.na(numday_above_tmax))] = 0
          }   
        }
        
        # calculate 2. for the year j.
        if(calc_predictor2_separately)
        {
          mat= cbind(as.matrix(tempd[,2:ncol(tempd)])<tmin,rep(0,nrow(tempd)))
          numday_below_tmin = apply(mat,1,sum)
        }
        
        # make the data frame with all the predictor variables in the list under the species' name
        if(k==1){
          if(calc_predictor1_separately)
          {
            numday_above_tmaxavg = numday_above_tmax
          }
          if(calc_predictor2_separately)
          {
            numday_below_tminavg = numday_below_tmin
          }  
        }else{ # add the data if there's data in there already to ultimately calculate the average rate per yr.
          if(calc_predictor1_separately)
          {
            numday_above_tmaxavg = numday_above_tmax + numday_above_tmaxavg
          }
          if(calc_predictor2_separately)
          {
            numday_below_tminavg = numday_below_tmin + numday_below_tminavg
          }  
        }
        print(sprintf('year %d done',yr))
      }
      idx = match(spproj$wbmID_1min[which(spproj$huc2==hucs[j])], tempd$ID)
      if(calc_predictor1_separately)
      {
        numday_above_tmaxavg = numday_above_tmaxavg/(finish - start + 1)  
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = numday_above_tmaxavg[idx]
      }
      if(calc_predictor2_separately)
      {
        numday_below_tminavg = numday_below_tminavg/(finish - start + 1)  
        spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = numday_below_tminavg[idx]
      }
    }
  }
  
  # write out the data in a separate file
  ofilename = sprintf('%s/%s_projarea.csv',ofilepath,spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}
beep(sound=5)




# 3. preictor calc for sp occurrence data. 

rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
tmins = spdata$tmin
tmaxs = spdata$tmax

reservoirmode =1
currentofuture = 'future'
gcmver = 'DOE-CNRM-ESM2-1' #'DOE-BCC-CSM2-MR'
if(currentofuture=='current')
{
  start = 2000
  finish = 2019
} else {
  start = 2060
  finish = 2079
}

tmin_count = unique(table(spdata$tmin))
tmax_count = unique(table(spdata$tmax))
tmin.common = apply(as.matrix(tmin_count[tmin_count>10]),1,function(x) which(table(spdata$tmin)==x))
tmin.common = as.numeric(names(table(spdata$tmin)[tmin.common]))
tmax.common = apply(as.matrix(tmax_count[tmax_count>10]),1,function(x) which(table(spdata$tmax)==x))
tmax.common = as.numeric(names(table(spdata$tmax)[tmax.common]))

if(!reservoirmode)
{
  ifiledir = sprintf('D:/sdm_modeling/wbm/%s/pristine w gcm/%s',gcmver,currentofuture) # for gettingg processed wbm temp flow data
  ofilepath = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/pristine w gcm/%s/%s',gcmver,currentofuture)
  tempflowsourcepath = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine w gcm/%s/temp_and_flow_predictors/%s/by_huc',gcmver,currentofuture)
} else {
  ifiledir = sprintf('D:/sdm_modeling/wbm/%s/pristine_gcm_reservoir/%s',gcmver,currentofuture)
  ofilepath = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/pristine_gcm_reservoir/%s/%s',gcmver,currentofuture) 
  tempflowsourcepath = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine_gcm_reservoir/%s/temp_and_flow_predictors/%s/by_huc',gcmver,currentofuture)
}

erroridx = c()
for(i in 1:nrow(spdata))#predictor_done[16:length(predictor_done)])
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  tmin = tmins[i]
  tmax = tmaxs[i]
  spproj = read.csv(path1)
  
  if(reservoirmode)
  { 
    #projareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/pristine_gcm_reservoir/%s/%s/%s_projarea.csv',gcmver,currentofuture,spdata$name[i])
    projareafilename = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/pristine_gcm_reservoir/%s/%s/%s_projarea.csv',gcmver,currentofuture,spdata$name[i])
  } else {
    #projareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/pristine w gcm/%s/%s/%s_projarea.csv',gcmver,currentofuture,spdata$name[i])  
    projareafilename = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/pristine w gcm/%s/%s/%s_projarea.csv',gcmver,currentofuture,spdata$name[i])
  }
  
  projarea = fread(projareafilename)
  
  #projarea_alltempflowpreds = projarea[,] # projarea all the temp flow predictor
  idx = match(spproj$comid, projarea$comid)
  
  head(spproj[which(is.na(idx)),])
  spproj$huc2
  length(which(is.na(idx)))
  length(idx)
  matcherror = 0
  if(any(is.na(idx)))
  {
    nahucs=  unique(spproj$huc2[which(is.na(idx))])
    if(length(nahucs) == 1 & -1 %in% nahucs )
    {
      matcherror = 0
    } else {
      matcherror = 1
    }
  }
  
  
  if(!matcherror)
  {
    spproj$numday_above_tmax = projarea$numday_above_tmax[idx]
    spproj$numday_below_tmin = projarea$numday_below_tmin[idx]
    spproj$dd90_5c = projarea$dd90_5c[idx]
    spproj$dd90_8c = projarea$dd90_8c[idx]
    spproj$dd90_10c = projarea$dd90_10c[idx]
    spproj$dd120_5c = projarea$dd120_5c[idx]
    spproj$dd120_8c = projarea$dd120_8c[idx]
    spproj$dd120_10c = projarea$dd120_10c[idx]
    spproj$dd150_5c = projarea$dd150_5c[idx]
    spproj$dd150_8c = projarea$dd150_8c[idx]
    spproj$dd150_10c = projarea$dd150_10c[idx]
    spproj$avgtemp = projarea$avgtemp[idx]
    if(!reservoirmode)
    {
      spproj$maxflow = projarea$maxflow[idx]
      spproj$maxflowdate = projarea$maxflowdate[idx]
      spproj$minflow = projarea$minflow[idx]
      spproj$minflowdate = projarea$minflowdate[idx]
      spproj$avgflow = projarea$avgflow[idx]
      spproj$maxminflowdiff = projarea$maxminflowdiff[idx]
    }
    ofilename = sprintf('%s/%s_wcomid.csv',ofilepath,spdata$name[i])
    write.csv(spproj,ofilename,row.names=FALSE)
    print(sprintf('%d done',i))
  } else
  {
    print('there are comids that did not match')
    erroridx = c(erroridx,i)    
  }
}


placeholder












































































rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
tmins = spdata$tmin
tmaxs = spdata$tmax

start = 2000
finish = 2019

tmin_count = unique(table(spdata$tmin))
tmax_count = unique(table(spdata$tmax))
tmin.common = apply(as.matrix(tmin_count[tmin_count>10]),1,function(x) which(table(spdata$tmin)==x))
tmin.common = as.numeric(names(table(spdata$tmin)[tmin.common]))
tmax.common = apply(as.matrix(tmax_count[tmax_count>10]),1,function(x) which(table(spdata$tmax)==x))
tmax.common = as.numeric(names(table(spdata$tmax)[tmax.common]))

predictor_done = read.csv("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/already_done.csv")
predictor_done = predictor_done$already_done

ifiledir = 'G:/My Drive/research/sdm_modeling/wbmdata/pristine w gcm/current'
ofilepath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/pristine w gcm/current' 
tempflowsourcepath = 'G:/My Drive/research/sdm_modeling/wbmdata/pristine w gcm/temp_and_flow_predictors/current/by_huc'
for(i in 1:nrow(spdata))#predictor_done[16:length(predictor_done)])
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  tmin = tmins[i]
  tmax = tmaxs[i]
  spproj = read.csv(path1)
  hucs = unique(floor(spproj$huc12/10^10))
  hucs = hucs[which(hucs>0)] # get rid of points matched with -9999 for hucs.
  
  
  spproj$numday_above_tmax = NA
  spproj$numday_below_tmin = NA
  spproj$dd90_5c = NA
  spproj$dd90_8c = NA
  spproj$dd90_10c = NA
  spproj$dd120_5c = NA
  spproj$dd120_8c = NA
  spproj$dd120_10c = NA
  spproj$dd150_5c = NA
  spproj$dd150_8c = NA
  spproj$dd150_10c = NA
  spproj$maxflow = NA
  spproj$maxflowdate = NA
  spproj$minflow = NA
  spproj$minflowdate = NA
  spproj$avgflow = NA
  spproj$maxminflowdiff = NA
  for(j in 1:length(hucs))
  {
    calc_predictor1_separately = 0
    calc_predictor2_separately = 0 
    filename = sprintf('%s/huc%d_projarea_tempNflow_predictors.csv',tempflowsourcepath,hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$wbmID_1min[which(spproj$huc2==hucs[j])], hucd$wbmID_1min)
    #1. average maximum number of days exceeding maximum temperature threshold in a year
    # load it from the predictor data by_huc if the species' tmax is in the common tmaxs.
    if (tmax %in% tmax.common) # tmax of the sp is one of the common tmaxs
    {
      tmaxidx = which(tmax.common == tmax)
      if(tmaxidx == 1)
      {
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd$numday_above_tmax26[idx]
      } else if (tmaxidx == 2)
      {
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd$numday_above_tmax29[idx]
      } else if (tmaxidx == 3)
      {
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd$numday_above_tmax31[idx]
      } else if (tmaxidx == 4)
      {
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd$numday_above_tmax31.4[idx]
      } else if (tmaxidx == 5)
      {
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd$numday_above_tmax33[idx]
      } else if (tmaxidx == 6)
      {
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = hucd$numday_above_tmax34[idx]
      }
    } else
    {
      calc_predictor1_separately = 1
    }
    #2. average maximum number of days going below minimum temperature threshold in a year
    # load it from the predictor data by_huc if the species' tmin is in the common tmins.
    if(tmin %in% tmin.common) # tmax of the sp is one of the common tmaxs
    {
      tminidx = which(tmin.common == tmin)
      if(tminidx == 1)
      {
        spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = hucd$numday_below_tmin2[idx]
      } else if (tminidx == 2)
      {
        spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = hucd$numday_below_tmin9[idx]
      } else if (tminidx == 3)
      {
        spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = hucd$numday_below_tmin10[idx]
      } else if (tminidx == 4)
      {
        spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = hucd$numday_below_tmin15[idx]
      }
    } else
    {
      calc_predictor2_separately = 1
    }
    
    # get rest of the predictors from by_huc data.
    spproj$dd90_5c[which(spproj$huc2==hucs[j])] = hucd$dd90_5c[idx]
    spproj$dd90_8c[which(spproj$huc2==hucs[j])]  = hucd$dd90_8c[idx]
    spproj$dd90_10c[which(spproj$huc2==hucs[j])] = hucd$dd90_10c[idx]
    spproj$dd120_5c[which(spproj$huc2==hucs[j])] = hucd$dd120_5c[idx]
    spproj$dd120_8c[which(spproj$huc2==hucs[j])] = hucd$dd120_8c[idx]
    spproj$dd120_10c[which(spproj$huc2==hucs[j])] = hucd$dd120_10c[idx]
    spproj$dd150_5c[which(spproj$huc2==hucs[j])] = hucd$dd150_5c[idx]
    spproj$dd150_8c[which(spproj$huc2==hucs[j])] = hucd$dd150_8c[idx]
    spproj$dd150_10c[which(spproj$huc2==hucs[j])] = hucd$dd150_10c[idx]
    spproj$maxflow[which(spproj$huc2==hucs[j])] = hucd$maxflow[idx]
    spproj$maxflowdate[which(spproj$huc2==hucs[j])] = hucd$maxflowdate[idx]
    spproj$minflow[which(spproj$huc2==hucs[j])] = hucd$minflow[idx]
    spproj$minflowdate[which(spproj$huc2==hucs[j])] = hucd$minflowdate[idx]
    spproj$avgflow[which(spproj$huc2==hucs[j])] = hucd$avgflow[idx]
    spproj$maxminflowdiff[which(spproj$huc2==hucs[j])] = hucd$maxminflowdiff[idx]
    
    # calculate predictor 1 and 2 if the species tmin and tmax are not in common tmin /tmax
    if(calc_predictor2_separately + calc_predictor1_separately > 0)
    {
      path2 ='G:/My Drive/research/sdm_modeling/wbmdata/pristine/reference_table'
      reffilename = sprintf('%s/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv',path2,hucs[j])
      ref = read.csv(reffilename)
      cellnum = nrow(ref)
      for(k in 1:(finish-start + 1))
      {
        # access the temperature data for those points for the relevant timeframe calculate the temperature and flow related predictor variables:
        yr = (k-1) + start
        tempfilename = sprintf('%s/Huc%d_CONUS_Output_RiverDischarge_DOE-ACCESS-CM2-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',ifiledir,hucs[j],yr)
        tempd =fread(tempfilename,header=TRUE)
        names(tempd)[1] = 'ID'
        
        
        if(nrow(tempd) != cellnum)
        {
          print(sprintf("TEMP DATA CELL COUNT DIFFERENT!! huc%d yr %d",hucs[j],yr))
        }
        
        # calculate 1. for the year k
        if(calc_predictor1_separately)
        {
          mat = cbind(as.matrix(tempd[,2:ncol(tempd)])>tmax,rep(0,nrow(tempd)))
          cum = apply(mat,1,cumsum)
          numday_above_tmax = apply(cum,2,function(x) unique(x[duplicated(x)]))
          if(class(numday_above_tmax)=='list')
          {
            numday_above_tmax = unlist(lapply(numday_above_tmax,function(x) max(x[2:length(x)] - x[1:(length(x)-1)]))) # Average maximum number of days in a year that exceeds tmax
            numday_above_tmax[which(is.na(numday_above_tmax))] = 0
          }   
        }
        
        # calculate 2. for the year j.
        if(calc_predictor2_separately)
        {
          mat= cbind(as.matrix(tempd[,2:ncol(tempd)])<tmin,rep(0,nrow(tempd)))
          numday_below_tmin = apply(mat,1,sum)
        }
        
        # make the data frame with all the predictor variables in the list under the species' name
        if(k==1){
          if(calc_predictor1_separately)
          {
            numday_above_tmaxavg = numday_above_tmax
          }
          if(calc_predictor2_separately)
          {
            numday_below_tminavg = numday_below_tmin
          }  
        }else{ # add the data if there's data in there already to ultimately calculate the average rate per yr.
          if(calc_predictor1_separately)
          {
            numday_above_tmaxavg = numday_above_tmax + numday_above_tmaxavg
          }
          if(calc_predictor2_separately)
          {
            numday_below_tminavg = numday_below_tmin + numday_below_tminavg
          }  
        }
        print(sprintf('year %d done',yr))
      }
      idx = match(spproj$wbmID_1min[which(spproj$huc2==hucs[j])], tempd$ID)
      if(calc_predictor1_separately)
      {
        numday_above_tmaxavg = numday_above_tmaxavg/(finish - start + 1)  
        spproj$numday_above_tmax[which(spproj$huc2==hucs[j])] = numday_above_tmaxavg[idx]
      }
      if(calc_predictor2_separately)
      {
        numday_below_tminavg = numday_below_tminavg/(finish - start + 1)  
        spproj$numday_below_tmin[which(spproj$huc2==hucs[j])] = numday_below_tminavg[idx]
      }
    }
  }
  
  # write out the data in a separate file
  ofilename = sprintf('%s/%s_wcomid.csv',ofilepath,spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}
