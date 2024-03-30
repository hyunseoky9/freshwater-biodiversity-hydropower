# making reservoir scenario wbm temperature data
# general gist is I add the etc value to the daily temperature for the respective wbm cell. 

library(data.table)

gcmver = 'DOE-CNRM-ESM2-1'

for( i in 1:18)
{
  #1. get the projection area and the etc values from it.
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  d = fread(filename)
  
  #2. get which etc values  belong to which cell. 
  # potential problem: there may be multiple comids with different etc values but same ll. what to do then?
  # answer, get an average etc value for the cell. 
  etc4wbmid = apply(as.matrix(unique(d$wbmID_1min)),1,function(x) d$etc[which(d$wbmID_1min==x)])
  for(j in 1:length(etc4wbmid))
  {
    if(length(which(!is.na(etc4wbmid[[j]]))) > 0)
    {
      etc4wbmid[[j]] = mean(unique(etc4wbmid[[j]][which(!is.na(etc4wbmid[[j]]))]))  
    } else
    {
      etc4wbmid[[j]] = 0
    }
  }
  etc4wbmid = cbind(unique(d$wbmID_1min),unlist(etc4wbmid))
  
  #for(yr in 2000:2019) # current
  #{
  #  #3. get the v2 temperature data  and add the etc value to the daily tempearture data. If the etc is NA, add 0.
  #  filename = sprintf('D:/sdm_modeling/wbm/%s/pristine_gcm_reservoir/current/Huc%d_CONUS_Output_RiverTemperature_%s-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',gcmver,i,gcmver,yr)
  #  d2 = fread(filename)
  #  idx = match(as.matrix(d2[,1]),etc4wbmid[,1])
  #  #d2 = d2 + matrix(rep(etc4wbmid[idx,2], times = ncol(d2)), ncol = ncol(d2))
  #  d2[,2:ncol(d2)] = d2[,2:ncol(d2)] - matrix(rep(etc4wbmid[idx,2], times = (ncol(d2)-1)), ncol = (ncol(d2)-1))
  #  # make sure no temperatures are below 0\
  #  idxn = which(d2<0)
  #  d2 = as.matrix(d2)
  #  d2[idxn] = 0
  #  d2 = as.data.frame(d2)
  #  #4. save the result
  #  write.csv(d2,filename,row.names=FALSE)
  #  print(yr)
  #}
  print('current done')
  for( yr in 2060:2079) # future
  {
    #3. get the v2 temperature data  and add the etc value to the daily tempearture data. If the etc is NA, add 0.
    filename = sprintf('D:/sdm_modeling/wbm/%s/pristine_gcm_reservoir/future/Huc%d_CONUS_Output_RiverTemperature_%s-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',gcmver,i,gcmver,yr)
    d2 = fread(filename)
    idx = match(as.matrix(d2[,1]),etc4wbmid[,1])
    d2[,2:ncol(d2)] = d2[,2:ncol(d2)] - matrix(rep(etc4wbmid[idx,2], times = (ncol(d2)-1)), ncol = (ncol(d2)-1))
    # make sure no temperatures are below 0\
    idxn = which(d2<0)
    d2 = as.matrix(d2)
    d2[idxn] = 0
    d2 = as.data.frame(d2)
    #4. save the result
    write.csv(d2,filename,row.names=FALSE)
    print(yr)
  }
  print('future done')
  print(i)
}

