# quality check for the workflow of turning wbm raw temp and flow data into predictors by huc and by species for projection area and for occurrence points.
library(data.table)



#3.2 quality check: there are no NAs in the by_huc predictors for pristine and reservoir

gcmver = 'DOE-ACCESS-CM2'
currentofuture= 'future'
reservoirmode = 0
if(reservoirmode)
{
  tempflowsourcepath = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine_gcm_reservoir/%s/temp_and_flow_predictors/%s/by_huc',gcmver,currentofuture)  
} else 
{
  tempflowsourcepath = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine w gcm/%s/temp_and_flow_predictors/%s/by_huc',gcmver,currentofuture)  
}

#tempflowsourcepath
for( m in 1:18)
{
  print(sprintf('huc%d:',m))
  filename = sprintf('%s/huc%d_projarea_tempNflow_predictors.csv',tempflowsourcepath,m)
  hucd = read.csv(filename)
  for(j in 1:ncol(hucd))
  {
    ii = which(is.na(hucd[[colnum]]))
    if(length(ii)>0)
    {
      print(sprintf('colname: %s, number of NAs in that column: %d',names(colnum),length(ii)))
    }
  }
}


#4.2 quality check: there are no NAs in the predictors except when there is no cellID for a given comid. 
currentofuture = 'future'
reservoirmode = 0
gcmver='DOE-ACCESS-CM2'

nal =c()
allidna = c()
for(i in 1:nrow(spdata))
{
  if(reservoirmode)
  {
    ofilepath2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/pristine_gcm_reservoir/%s/%s',gcmver,currentofuture) #'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp'  
  } else {
    ofilepath2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/pristine w gcm/%s/%s',gcmver,currentofuture) #'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp'    
  }
  ofilename2 = sprintf('%s/%s_projarea.csv',ofilepath2,spdata$name[i])
  spproj2 = fread(ofilename2)
  
  
  print(sprintf('number of points with NA for wbm cell ids: %d',length(which(is.na(spproj$wbmID_1min)))))
  idx2 = which(is.na(spproj2$numday_above_tmax))
  
  if(length(which(!is.na(spproj2$wbmID_1min[idx2])))>0)
  {
    print('*********************')
    print('there are no NAs in the predictors for a given comid that has a corresponding cellID!!!')
    print('*********************')
  }
  
  print(sprintf('%d done',i))
}

#5.5 sample 1 or 2 occurrence pt predictors and see if all the comids with upstream dams have non-NA temp & flow related predictor values 
