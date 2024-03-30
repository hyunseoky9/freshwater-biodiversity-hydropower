# getting flowline type and indicator variable whether the point is on a waterbody or not
# for all the occpts and projection areas 

# UPDATE 2022.5.19.: really no need to get wbd types anymore cuz wbd types given by the 
# nhdplustools not very accurate...
wbd_class_identify <- function(comid)
{
  # define the waterbody type. THere's sometimes more than one waterbody type spat out
  # so I choose one of the types for the comid. 
  
  print(sprintf('%d',comid))
  comidread = 0
  class(comidread) = 'try-error'
  while(class(comidread)=='try-error')
  {
    comidread = try({
      subset_file <- tempfile(fileext = ".gpkg")
      subset <- subset_nhdplus(comids = comid,
                               output_file = subset_file,
                               nhdplus_data = "download", 
                               flowline_only = FALSE,
                               return_data = TRUE, overwrite = TRUE)
    })
  }
  print('subset function worked in wbd_class_identify')
  MaxTable <- function(InVec, mult = TRUE) {
    if (!is.factor(InVec)) InVec <- factor(InVec)
    A <- tabulate(InVec)
    if (isTRUE(mult)) {
      levels(InVec)[A == max(A)]
    } 
    else levels(InVec)[which.max(A)]
  }
  wbd_class = MaxTable(subset$NHDWaterbody$ftype)
  if(length(wbd_class)>1)
  {
    if( "LakePond" %in% wbd_class)
    {
      wbd_class = "LakePond"      
    } else if ("Reservoir" %in% wbd_class)
    {
      wbd_class = "Reservoir"
    } else
    {
      wbd_class = wbd_class[1]
    }
  }
  return(wbd_class)
}

library(parallel)
library(MASS)
library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spname_list = spnamedata$name
n.cores <- detectCores()

if (occ_or_proj)
{
  looplength = length(spname_list)
} else {
  looplength = 18 # number of huc units
}

for(i in 14) #looplength)
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
  intlen = 8000 # interval length. 
  numinterval = ceiling(length(comid_all)/intlen)
  newdata = data.frame()
  for ( j in 1:numinterval) # put a set amount of comids into waterbody calculation function.
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
    comidread = 0 # variable made up for error-catching.
    class(comidread) = 'try-error'
    while(class(comidread)=='try-error') # get ftype of the comid
    {
      comidread = try({
        subset_file <- tempfile(fileext = ".gpkg")
        subset <- subset_nhdplus(comids = comid,
                                 output_file = subset_file,
                                 nhdplus_data = "download", 
                                 flowline_only = TRUE,
                                 return_data = TRUE, overwrite = TRUE)
      })
    }
    key =  data.frame(comid=as.integer(subset$NHDFlowline_Network$comid),ftype=subset$NHDFlowline_Network$ftype)
    wbd_idx = which(key$ftype=='ArtificialPath')
    key$waterbody = rep(0,nrow(key))
    key$wbd_classes = rep('NA',nrow(key))
    if(length(wbd_idx)>0)
    {
      key$waterbody[wbd_idx] = 1
      clust <- makeCluster(n.cores)
      clusterEvalQ(clust, {
        library(nhdplusTools)
        library(sf)
        library(dplyr)
      })
      xx = parApply(clust,as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
      stopCluster(clust)
      key$wbd_classes[wbd_idx] = xx
      #key$wbd_classes[wbd_idx] = pbapply(as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
    }
    if((finishidx - startidx +1)!= nrow(key))
    {
      sprintf("data length of key and data are unequal.")
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    } else {
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    }
    print(sprintf('%d/%d done',j,numinterval))
  }
  
  # get rid of waterbodies that are not reservoir or lakes 
  # and flowlines that are not streamriver or canal/ditch. wbd_rid_idx = which(newdata$wbd_classes != 'NA' & newdata$wbd_classes != 'LakePond' & newdata$wbd_classes != 'Reservoir')
  wbd_rid_idx = which((newdata$wbd_classes != 'NA' & newdata$wbd_classes != 'LakePond') | (is.na(newdata$wbd_classes) & newdata$waterbody==1))
  if(length(wbd_rid_idx))
  {newdata = newdata[-wbd_rid_idx,]} # get rid of occpts that are not lakeponds or reservoirs
  flowline_rid_idx = which(newdata$ftype != 'StreamRiver' & newdata$ftype != 'CanalDitch' & newdata$ftype != 'ArtificialPath'|is.na(newdata$ftype))
  if(length(flowline_rid_idx)>0)
  {newdata = newdata[-flowline_rid_idx,]} # get rid of occpts whose flowlines are not streamriver or canalditch or aritificial path(waterbody).
  #get rid of excess index columns
  #if(!is.null(data$X.1) && !is.null(data$X) )
  #{
  #  data = select(data, -X.1)
  #} else {
  #  print("there's X.1 but not X???")
  #}
  #if(!is.null(data$X.2) && !is.null(data$X))
  #{
  #  data = select(data, -X.2)
  #}
  newdata$X = 1:nrow(newdata) # re-index
  write.csv(newdata,filename,row.names=FALSE)
  print(sprintf('%d done' , i))
}
