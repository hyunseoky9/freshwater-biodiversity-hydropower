# fixing reservoir scenario projection area and occ data temp predictors.
library(tictoc)
library(data.table)
rm(list=ls())
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')

# fix by huc first
reservoirmode = 0 # if making predictor variables with reservoir effect, this var is 1.
#currentofuture = 'current'
gcmver = 'DOE-CNRM-ESM2-1' #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 

predtaildiff = c()
occtaildiff = c()
projerror = c()
sperror = c()
for(i in 1:nrow(spnamedata))
{
  for(currentofuture in c('future'))#,'future'))
  {
    # projarea fix
    
    fileres = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/%s/%s/%s/%s_projarea.csv','pristine_gcm_reservoir',gcmver,currentofuture,spnamedata$name[i])
    filepri = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/%s/%s/%s/%s_projarea.csv','pristine w gcm',gcmver,currentofuture,spnamedata$name[i])
    filenontf = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[i])
    projres = fread(fileres)
    projpri = fread(filepri)
    nrowb4 = nrow(projres)
    projnontf = fread(filenontf)
    nontailwater = which(is.na(projnontf$udd2hilarri))
    tailwater = which(!is.na(projnontf$udd2hilarri))
    projres = as.data.frame(projres)
    projpri = as.data.frame(projpri)
    
    temppreds = names(projres)[c(grep('numday',names(projres)),grep('dd',names(projres)),grep('avgtemp',names(projres)))]
    projresnontail = projres[nontailwater,temppreds]
    projprinontail = projpri[nontailwater,temppreds]

    projrestail = projres[tailwater,temppreds]
    projpritail = projpri[tailwater,temppreds]
    priresdiff = projrestail - projpritail
    rowsum = rowSums(priresdiff)
    rows_equaltail = abs(rowsum)<10^-15
    predtaildiff = c(predtaildiff,length(which(rows_equaltail==FALSE))>0)
    #print(length(which(rows_equaltail==FALSE))>0)
    priresdiff = projresnontail - projprinontail
    rowsum = rowSums(priresdiff)
    rows_equal = abs(rowsum)<10^-15
    
    if(length(which(rows_equal==FALSE))>0)
    {
      nontailwater_wrong = nontailwater[which(rows_equal==FALSE)]
      projres[nontailwater_wrong,temppreds] = projpri[nontailwater_wrong,temppreds]
      projresnontail = projres[nontailwater,temppreds]
      projprinontail = projpri[nontailwater,temppreds]
      
      priresdiff = projresnontail - projprinontail
      rowsum = rowSums(priresdiff)
      rows_equal = abs(rowsum)<10^-15
      if(length(which(rows_equal==FALSE))!=0)
      {
        projerror = c(projerror,i)
        print('not all nontailwater reaches have same temp predictors between the res and pri scenarios!!!')
      }
      if(nrowb4 != nrow(projres))
      {
        projerror = c(projerror,i)
        print('nrow not the same as it was loaded!!!')
      }
      write.csv(projres,fileres,row.names=FALSE)
    }
    
    # occ data fix
    filenamespres = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv','pristine_gcm_reservoir',gcmver,currentofuture,spnamedata$name[i]) 
    filenamesppri = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/occpt_tempflow_predictors/%s/%s/%s/%s_wcomid.csv','pristine w gcm',gcmver,currentofuture,spnamedata$name[i]) 
    spres = fread(filenamespres)
    sppri = fread(filenamesppri)
    nontailwater = which(is.na(spres$udd2hilarri))
    tailwater = which(!is.na(spres$udd2hilarri))
    spres = as.data.frame(spres)
    sppri = as.data.frame(sppri)
    nrowb4 = nrow(spres)
    
    temppreds = names(spres)[c(grep('numday',names(spres)),grep('dd\\d\\d',names(spres)),grep('avgtemp',names(spres)))]
    spresnontail = spres[nontailwater,temppreds]
    spprinontail = sppri[nontailwater,temppreds]
    
    sprestail = spres[tailwater,temppreds]
    sppritail = sppri[tailwater,temppreds]
    
    rows_equaltail <- apply(as.matrix(1:nrow(sprestail)), 1, function(x) all(sprestail[x,] == sppritail[x,]))
    #print(length(which(rows_equaltail==FALSE))>0)
    rows_equal <- apply(as.matrix(1:nrow(spresnontail)), 1, function(x) all(spresnontail[x,] == spprinontail[x,]))
    priresdiff = spresnontail - spprinontail
    rowsum = rowSums(priresdiff)
    rows_equal = abs(rowsum)<10^-15
    
    occtaildiff = c(occtaildiff,length(which(rows_equal==FALSE))>0)
    #print(length(which(rows_equal==FALSE))>0)
    if(length(which(rows_equal==FALSE))>0)
    {
      nontailwater_wrong = nontailwater[which(rows_equal==FALSE)]
      spres[nontailwater_wrong,temppreds] = sppri[nontailwater_wrong,temppreds]
      spresnontail = spres[nontailwater,temppreds]
      spprinontail = sppri[nontailwater,temppreds]

      rows_equal <- apply(as.matrix(1:nrow(spresnontail)), 1, function(x) all(spresnontail[x,] == spprinontail[x,]))
      priresdiff = spresnontail - spprinontail
      rowsum = rowSums(priresdiff)
      rows_equal = abs(rowsum)<10^-15
      
      if(length(which(rows_equal==FALSE))!=0)
      {
        sperror = c(sperror,i)
        print('not all nontailwater reaches have same temp predictors between the res and pri scenarios!!!')
      }
      if(nrowb4 != nrow(spres))
      {
        sperror = c(sperror,i)
        print('nrow not the same as it was loaded!!!')
      }
      write.csv(spres,filenamespres,row.names=FALSE)
    }
    print(currentofuture)
  }
  print(sprintf('%d/%d done',i,nrow(spnamedata)))
}
