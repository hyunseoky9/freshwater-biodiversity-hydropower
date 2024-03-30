# getting catchment area for all the comids of all huc 2 units. 
catchmentarea <- function(comid)
{
  comidread = 0
  class(comidread) = 'try-error'
  comidread = try({
    subset_file <- tempfile(fileext = ".gpkg")
    subset <- subset_nhdplus(comids = comid,
                             output_file = subset_file,
                             nhdplus_data = "download", 
                             flowline_only = FALSE,
                             return_data = TRUE, overwrite = TRUE)
    area = subset$CatchmentSP$areasqkm
  })
  if(class(comidread) == 'try-error')
  {
    area = NA
  }
  return(area)
}

#catchmentarea <- function(comid)
#{
#  subset_file <- tempfile(fileext = ".gpkg")
# subset <- subset_nhdplus(comids = comid,
#                           output_file = subset_file,
#                           nhdplus_data = "download", 
#                           flowline_only = FALSE,
#                           return_data = TRUE, overwrite = TRUE)
#  area = subset$CatchmentSP$areasqkm
#  return(area)
#}

library(parallel)
library(MASS)
library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)

n.cores <- detectCores()
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area

looplength  = 18


for(i in 10) #looplength)
{
  if(occ_or_proj)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    data = read.csv(filename)
  } else {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
    data = read.csv(filename)
  }
  comid_all = data$comid[which(!is.na(data$comid))]
  numinterval = ceiling(length(comid_all)/10000)
  newdata = data.frame()
  for ( j in 40:numinterval)
  {
    if(j!= numinterval)
    {
      startidx = ((j-1)*10000 + 1)
      finishidx = startidx + 9999
      comid = comid_all[startidx:finishidx]
    } else {
      startidx = ((j-1)*10000 + 1)
      finishidx = length(comid_all)
      comid = comid_all[startidx:finishidx]
    }
    clust <- makeCluster(n.cores)
    clusterEvalQ(clust, {
      library(nhdplusTools)
      library(sf)
      library(dplyr)
    })
    xx = parApply(clust,as.matrix(comid),1,catchmentarea)
    stopCluster(clust)
    xx[sapply(xx, is.null)] <- NA
    xx = unlist(xx)
    key = as.data.frame(cbind(comid,unlist(xx)))
    names(key) = c('comid','catchment_areasqkm')
    write.csv(key,sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/sub/huc%d-%d.csv',i,j))
    if((finishidx - startidx +1)!= nrow(key))
    {
      sprintf("data length of key and data are unequal.")
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    } else {
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    }
    print(sprintf('%d/%d for huc%d done', j,numinterval,i))
  }
  
  # get rid of waterbodies that are not reservoir or lakes and flowlines that are not streamriver or canal/ditch.
  newdata$X = 1:nrow(newdata) # re-index
  ca = newdata$catchment_areasqkm
  ca[sapply(ca, is.null)] <- NA
  ca = unlist(ca)
  newdata$catchment_areasqkm = ca
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
  #write.csv(newdata,ofilename,row.names=FALSE)
  print(sprintf('%d done', i))
}






# serial version
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area

looplength  = 18


for(i in 17) #looplength)
{
  if(occ_or_proj)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    data = read.csv(filename)
  } else {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
    data = read.csv(filename)
  }
  comid_all = data$comid[which(!is.na(data$comid))]
  intlen = 10000
  numinterval = ceiling(length(comid_all)/intlen)
  newdata = data.frame()
  for ( j in 4)#1:numinterval)
  {
    if(j!= numinterval)
    {
      startidx = ((j-1)*intlen + 1)
      finishidx = startidx + (intlen-1)
      comid = comid_all[startidx:finishidx]
    } else {
      startidx = ((j-1)*intlen + 1)
      finishidx = length(comid_all)
      comid = comid_all[startidx:finishidx]
    }
    xx = pbapply(as.matrix(comid),1,catchmentarea)
    key = as.data.frame(cbind(comid,xx))
    names(key) = c('comid','catchment_areasqkm')
    write.csv(key,sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/sub/huc%d-%d.csv',i,j))
    #key$wbd_classes[wbd_idx] = pbapply(as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
    if((finishidx - startidx +1)!= nrow(key))
    {
      sprintf("data length of key and data are unequal.")
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    } else {
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    }
    print(sprintf('%d/%d for huc%d done', j,numinterval,i))
  }
  
  newdata$X = 1:nrow(newdata) # re-index
  ca = newdata$catchment_areasqkm
  ca[sapply(ca, is.null)] <- NA
  ca = unlist(ca)
  newdata$catchment_areasqkm = ca
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
  #write.csv(newdata,ofilename,row.names=FALSE)
  print(sprintf('%d done', i))
}

