# getting flowline type and indicator variable whether the point is on a waterbody or not
# for all the occpts and projection areas 

wbd_class_identify <- function(comid)
{
  subset_file <- tempfile(fileext = ".gpkg")
  subset <- subset_nhdplus(comids = comid,
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = FALSE,
                           return_data = TRUE, overwrite = TRUE)
  
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

library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area

looplength  = 18
for(i in 5) #looplength)
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
  for ( j in 1:numinterval)
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
    subset_file <- tempfile(fileext = ".gpkg")
    subset <- subset_nhdplus(comids = comid,
                             output_file = subset_file,
                             nhdplus_data = "download", 
                             flowline_only = TRUE,
                             return_data = TRUE, overwrite = TRUE)
    key =  data.frame(comid=as.integer(subset$NHDFlowline_Network$comid),ftype=subset$NHDFlowline_Network$ftype)
    wbd_idx = which(key$ftype=='ArtificialPath')
    key$waterbody = rep(0,nrow(key))
    key$wbd_classes = rep('NA',nrow(key))
    if(length(wbd_idx)>0)
    {
      key$waterbody[wbd_idx] = 1
      key$wbd_classes[wbd_idx] = pbapply(as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
    }
    if((finishidx - startidx +1)!= nrow(key))
    {
      sprintf("data length of key and data are unequal.")
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    } else {
      newdata = rbind(newdata,merge(data[startidx:finishidx,],key))
    }
  }
  
  # get rid of waterbodies that are not reservoir or lakes and flowlines that are not streamriver or canal/ditch.
  wbd_rid_idx = which(data$wbd_classes != 'NA' & data$wbd_classes != 'LakePond' & data$wbd_classes != 'Reservoir')
  if(length(wbd_rid_idx))
  {data = data[-wbd_rid_idx,]} # get rid of occpts that are not lakeponds or reservoirs
  flowline_rid_idx = which(data$ftype != 'StreamRiver' & data$ftype != 'CanalDitch' & data$ftype != 'ArtificialPath')
  if(length(flowline_rid_idx)>0)
  {data = data[-flowline_rid_idx,]} # get rid of occpts whose flowlines are not streamriver or canalditch or aritificial path(waterbody).
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
  newdata$X = 1:nrow(data) # re-index
  write.csv(newdata,filename,row.names=FALSE)
  print(sprintf('%d done' , i))
}










# play codes
length(subset$NHDFlowline_Network$comid)
length(comid)
cbind(sort(subset$NHDFlowline_Network$comid),sort(comid))
key = data.frame(comid=as.integer(subset$NHDFlowline_Network$comid),ftype=subset$NHDFlowline_Network$ftype)
key$waterbody = rep(0,nrow(key))
key$waterbody[which(key$ftype=='ArtificialPath')] = 1




library(nhdplusTools)
library(sf)
tahoe = st_sfc(st_point(c(-120.103342,39.119359)), crs = 4269)
tahoe = discover_nhdplus_id(point = tahoe)
truckee = st_sfc(st_point(c(-120.199234,39.316836)), crs = 4269)
truckee = discover_nhdplus_id(point = truckee)
summer = st_sfc(st_point(c(-80.888956,38.226310)), crs=4269)
summer = discover_nhdplus_id(point= summer)

f_tahoe <- navigate_nldi(list(featureSource = "comid", 
                               featureID = tahoe), 
                          mode = "DM", 
                          distance_km = 1)

f_truckee <- navigate_nldi(list(featureSource = "comid", 
                              featureID = truckee), 
                         mode = "DM", 
                         distance_km = 1)
f_summer <- navigate_nldi(list(featureSource = "comid", 
                               featureID = summer), 
                          mode = "DM", 
                          distance_km = 1)

subset_file <- tempfile(fileext = ".gpkg")
subset_tahoe <- subset_nhdplus(comids = as.integer(f_tahoe$origin$comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
subset_tahoe$NHDFlowline_Network$ftype
subset_tahoe$NHDWaterbody$ftype



subset_file <- tempfile(fileext = ".gpkg")
subset_truckee <- subset_nhdplus(comids = as.integer(f_truckee$origin$comid),
                               output_file = subset_file,
                               nhdplus_data = "download", 
                               flowline_only = FALSE,
                               return_data = TRUE, overwrite = TRUE)
subset_truckee$NHDFlowline_Network$ftype

subset_file <- tempfile(fileext = ".gpkg")
subset_summer <- subset_nhdplus(comids = as.integer(f_summer$origin$comid),
                                 output_file = subset_file,
                                 nhdplus_data = "download", 
                                 flowline_only = FALSE,
                                 return_data = TRUE, overwrite = TRUE)
subset_summer$NHDFlowline_Network$ftype
subset_summer$NHDWaterbody$ftype

subset_file <- tempfile(fileext = ".gpkg")
subset_all <- subset_nhdplus(comids = as.integer(c(tahoe, truckee, summer)),
                                 output_file = subset_file,
                                 nhdplus_data = "download", 
                                 flowline_only = TRUE,
                                 return_data = TRUE, overwrite = TRUE)

subset_all$NHDFlowline_Network
subset_all$NHDWaterbody$ftype
