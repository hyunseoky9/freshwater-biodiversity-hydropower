# number of ocurrence data check for each scenario
threshold = 0.4
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
currentofuture.list = c('current','future')
gcmver.list = c('DOE-ACCESS-CM2','DOE-BCC-CSM2-MR','DOE-CNRM-ESM2-1')
scenario.list = c('pristine w gcm','pristine_gcm_reservoir')
numscenarios = length(currentofuture.list)*length(gcmver.list)*length(scenario.list)
numoccs = matrix(rep(NA,nrow(spnamedata)*numscenarios), ncol = numscenarios)
j=1
for(currentofuture in currentofuture.list)
{
  for(gcmver in gcmver.list)
  {
    for(scenario in scenario.list )
    {
      for ( i in 1:nrow(spnamedata))
      {
        filename = sprintf(
          'G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv',
          scenario,gcmver,currentofuture,spnamedata$name[i])
        spdata = read.csv(filename)
        spdata = dplyr::select(spdata,wbmID_30sec_netsymdiff,decimalLongitude,decimalLatitude,BFI,waterbody,streamorder,
                      numday_above_tmax,dd90_8c,minflow,minflowdate,maxflowdate,avgflow,slope)
        if(length(which(spdata$streamorder==-9))>0)
        {
          spdata = spdata[-which(spdata$streamorder==-9),]  
        }
        spdata = na.omit(spdata)
        numoccs[i,j] = length(which(spdata$wbmID_30sec_netsymdiff < threshold))
        if(i==nrow(spnamedata))
        {
          j = j + 1
        }
      }
    }
  }
}
rr=apply(numoccs,1,unique)
idx0 = which(rr>30)
length(idx0)
write.csv(idx0,'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv',row.names=FALSE)
