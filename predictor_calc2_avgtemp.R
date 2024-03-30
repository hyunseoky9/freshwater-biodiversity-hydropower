#get average temperature for projected areas and species occurrence data.
rm(list = ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
start = 2000
finish = 2019
hucnum = 18
spname = spdata$name

# 1. get average temp data for huc based projection areas
irange = 1
for(i in irange)
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
    tempfilename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/Huc%d_CONUS_Output_RiverTemperature_DOERunoff+WBM20WTempPrist_01min_dTS%d_v2.csv',i,yr)
    tempd =fread(tempfilename,header=TRUE)
    print('tempd read')
    names(tempd)[1] = 'ID'
    
    if(nrow(tempd) != cellnum)
    {
      print(sprintf("TEMP DATA CELL COUNT DIFFERENT!! huc%d yr %d",i,j))
    }
    
    avgtemp = apply(as.matrix(tempd[,2:ncol(tempd)]),1,mean)
    
    # make the data frame with all the predictor variables in the list under the species' name
    if(j==1){
      pred_perhuc = data.frame(wbmID_1min=tempd$ID,avgtemp)
    }else{ # add the data if there's data in there already to ultimately calculate the average rate per yr.
      pred_perhuc[,c('avgtemp')] =
        pred_perhuc[,c('avgtemp')] +
        data.frame(avgtemp)
    }
    print('predictor for the year added to the dataframe')
    print(sprintf('year %d done',yr))
  }
  # divide the sums of predictors with the number of years to get the average
  pred_perhuc[,c('avgtemp')] =
    pred_perhuc[,c('avgtemp')]/
    (finish - start + 1) # divide by data frame by number of years to get average.
  
  # read in data 
  filename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/temp_and_flow_predictors/by_hucs/huc%d_projarea_tempNflow_predictors.csv',i)
  d = read.csv(filename)
  d = cbind(d,avgtemp)
  # write out the data in a separate file
  write.csv(d,filename,row.names=FALSE)
  print(sprintf('huc%d done',i))
}

# 2. get predictor values for each species projection area
rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')

start = 2000
finish = 2019

for(i in 1:nrow(spdata))
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spdata$name[i])
  spproj = read.csv(path1)
  hucs = unique(spproj$huc2)
  spproj$avgtemp = NA
  for(j in 1:length(hucs))
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/temp_and_flow_predictors/by_hucs/huc%d_projarea_tempNflow_predictors.csv',hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$wbmID_1min[which(spproj$huc2==hucs[j])], hucd$wbmID_1min)
    # get rest of the predictors from by_huc data.
    spproj$avgtemp[which(spproj$huc2==hucs[j])] = hucd$avgtemp[idx]
  }
  # write out the data in a separate file
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}


# 3. preictor calc for sp occurrence data. 

rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')

start = 2000
finish = 2019

for(i in 1:nrow(spdata))#predictor_done[16:length(predictor_done)])
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  spproj = read.csv(path1)
  hucs = unique(floor(spproj$huc12/10^10))
  hucs = hucs[which(hucs>0)] # get rid of points matched with -9999 for hucs.
  spproj$avgtemp = NA
  for(j in 1:length(hucs))
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/temp_and_flow_predictors/by_hucs/huc%d_projarea_tempNflow_predictors.csv',hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$wbmID_1min[which(spproj$huc2==hucs[j])], hucd$wbmID_1min)
    #1. average maximum number of days exceeding maximum temperature threshold in a year
    # load it from the predictor data by_huc if the species' tmax is in the common tmaxs.

    # get rest of the predictors from by_huc data.
    spproj$avgtemp[which(spproj$huc2==hucs[j])] = hucd$avgtemp[idx]
  }
  
  # write out the data in a separate file
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}
