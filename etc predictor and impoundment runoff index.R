# Calculates Impoundment runoff index (IR) (1.) and Equilibrium Temperature Concept (ETC) (2.)


# 1. getting IR
# read in maximum water storage data from WBM (dam list incomplete...)
#filename = 'G:/My Drive/research/sdm_modeling/dam data/GMLC_DAMS_01MIN_HYedition.csv'
#damdata = read.csv(filename)
# get maximum water storage data from NID
require(dams)
library(data.table)
#nid_subset$nidid
# identify which nidids in sdm data are not featured in the dams package data (NID data).
{
nidids = c()
ehaids = c()
eiaids = c()
for( i in 1:18)
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  spproj = fread(path1)
  ehaids = c(ehaids,unique(spproj$ud_ehaID))
  nidids = c(nidids,unique(spproj$udd2hilarri_nidid))
}
nidids= unique(nidids)
nidids= nidids[-which(is.na(nidids))]

# get all the dam information that are on nid for the sdm data dams.
idx = apply(as.matrix(nidids),1,function(x) which(nid_subset$nidid ==x))
nid_subset$nidid[idx[[286]]]

for( i in 1:length(idx))
{
  if(length(idx[[i]])>1)
  {
    #print(i)
    #print(nid_subset[idx[[i]],c(4,5)])
    #readline()
    idx[[i]] = idx[[i]][1] # only take the first dam.
  }
}

idx = unlist(idx)
idx = idx[which(!is.na(idx))]
featured_dams = nid_subset[idx,]


a = which(nidids %in% nid_subset$nidid)
b = which(!(nidids %in% nid_subset$nidid)) # dams in sdm not featured in NID.
length(a)
length(b)
nidids[b]
# add in the dam data for the sdm data dams that are un-featured in the NID manually.
unfeatured_dams = data.frame()
unfeatured_dams = rbind(unfeatured_dams,c('ME83016',955,44.5792,-69.5552))
unfeatured_dams = rbind(unfeatured_dams,c('ME00054',60,43.3946205,-70.4436353))
unfeatured_dams = rbind(unfeatured_dams,c('ME96105',126,44.285,-70.5371))
unfeatured_dams = rbind(unfeatured_dams,c('ME83030',711,43.497637, -70.451265))
unfeatured_dams = rbind(unfeatured_dams,c('GA00599',207,34.076389, -83.804167))
unfeatured_dams = rbind(unfeatured_dams,c('WI00726',2640,46.746699, -91.485091))
unfeatured_dams = rbind(unfeatured_dams,c('WI00136',140,43.687772, -89.047921))
unfeatured_dams = rbind(unfeatured_dams,c('WI00288',180,44.494025, -89.311200))
unfeatured_dams = rbind(unfeatured_dams,c('WI00284',120,43.472253, -87.991488))
unfeatured_dams = rbind(unfeatured_dams,c('NC05713',1400,35.4646, -82.5442))
unfeatured_dams = rbind(unfeatured_dams,c('WI00036',740,46.237210, -91.783966))
unfeatured_dams = rbind(unfeatured_dams,c('WI00056',710,45.761023, -91.220270))
unfeatured_dams = rbind(unfeatured_dams,c('WI00184',670,42.843677, -89.172776))
unfeatured_dams = rbind(unfeatured_dams,c('WI00219',200,43.586347, -90.644188))
unfeatured_dams = rbind(unfeatured_dams,c('WI00301',95,43.458664, -89.714883))
unfeatured_dams = rbind(unfeatured_dams,c('WI00181',500,43.223567, -90.466741))
unfeatured_dams = rbind(unfeatured_dams,c('WI00302',200,43.466794, -89.742323))
unfeatured_dams = rbind(unfeatured_dams,c('WI00149',1000,45.192620, -89.688284))
unfeatured_dams = rbind(unfeatured_dams,c('CA10411',165,36.9837,	-120.5))
names(unfeatured_dams)= c('nidids','max_storage_acreft','lat','long')
head(unfeatured_dams)


all_coords  = data.frame(nidid=c(featured_dams$nidid,unfeatured_dams$nidids),maxstorage=c(featured_dams$max_storage,unfeatured_dams$max_storage_acreft),long=c(featured_dams$longitude,unfeatured_dams$long),
                         lat=c(featured_dams$latitude,unfeatured_dams$lat))
}
# read in volumetric runoff data derived from McCabe and Wolock 2011
# first load hilarri to add in the mean annual runoff.
filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_V2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)

featured_dams[1,] # maxstorage in acreft (both featured and unfeatured)

# read in mean annual runoff volumetric version for the dam basin areas.
filename = 'G:/My Drive/research/sdm_modeling/dam data/runoff data/mean_annual_runoff_volume_km3_bydam.csv'
mar_km3 = read.csv(filename)

#use nid_storage value from nid if max_storage value is NA
idx0 = which(is.na(all_coords$maxstorage))
ids = all_coords$nidid[idx0]
idx = apply(as.matrix(ids),1,function(x) which(nid_subset$nidid==x))
for( i in 1:length(idx))
{
  if(length(idx[[i]])>1)
  {
    idx[[i]] = idx[[i]][1]
  }
}
idx = unlist(idx)
subset = nid_subset[idx,]
subset$nidid == all_coords$nidid[idx0]
all_coords$maxstorage[idx0] = subset$nid_storage
all_coords$maxstorage = as.numeric(all_coords$maxstorage)

# check the dams with max storage of 0 and see if nid_storage value is non-zero
idx0 = which(all_coords$maxstorage==0)
ids = all_coords$nidid[idx0]
idx = apply(as.matrix(ids),1,function(x) which(nid_subset$nidid==x))
for( i in 1:length(idx))
{
  if(length(idx[[i]])>1)
  {
    idx[[i]] = idx[[i]][1]
  }
}
idx = unlist(idx)
subset = nid_subset[idx,]
subset$max_storage
subset$normal_storage
subset$nid_storage
all_coords$maxstorage[idx0] = as.numeric(subset$nid_storage)
# for those that are still 0, get it from nid website manually (only 9 of them)
idx1=  which(all_coords$maxstorage==0)
ids = all_coords$nidid[idx1] 
manually_retrieved_storagedata = c(1646560,1,1,1048700,175,285552,0,0,0,0)
all_coords$maxstorage[idx1] = manually_retrieved_storagedata
# convert max storage to cubic kilometer (km3)
all_coords$maxstorage_km3 = all_coords$maxstorage*1.23348e-6

# calculate impoundment runoff index (=maximum storage/mean annual runoff)
all_coords$IR = all_coords$maxstorage_km3/mar_km3$mean_annual_runoff_km3
all_coords$maxstorage_km3[which(is.na(all_coords$IR))] # check if there are any na's for maxstorage




# get the max storage, volumetric mean annual runoff, and impoundment runoff index into HY edition hilarri dataset
matchidx = match(all_coords$nidid,hilarri$nididfull)

hilarri$maxstorage_km3 = NA
hilarri$mar_km3 = NA
hilarri$IR = NA

hilarri$maxstorage_km3[matchidx] = all_coords$maxstorage_km3
hilarri$mar_km3[matchidx] = mar_km3$mean_annual_runoff_km3
hilarri$IR[matchidx] = all_coords$IR
filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_V2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
write.csv(hilarri,filename,row.names=FALSE)
boxplot(all_coords$IR) # distribution from iR seems ok. It's similar to Buendia 2015





#2. getting etc for predictors.
# parameters rom buendia
b1 = 0.07
b2 = 0.15
b3 = -0.11
# first huc based predictors

filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_V2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
info = hilarri[which(!is.na(hilarri$IR)),]
for( i in 2:18)
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  spproj = fread(path1)
  idx = match(spproj$udd2hilarri_nidid,info$nididfull)
  spproj$IR = info$IR[idx]
  spproj$maxstorage_km3 = info$maxstorage_km3[idx]
  spproj$mean_annual_runoff_km3 = info$mar_km3[idx]
  write.csv(spproj,path1,row.names=FALSE)  
}
# getting etc for by_huc projection area
for( i in 2:18)
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  spproj = fread(path1)
  spproj$etc = b1 + b2*log(spproj$udd2hilarri) + b3*log(spproj$IR)
  write.csv(spproj,path1,row.names=FALSE)  
}

# next sp based projarea

setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
for( i in 1:nrow(spdata))
{
  path2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  spproj = fread(path2)
  idx = match(spproj$udd2hilarri_nidid,info$nididfull)
  spproj$IR = info$IR[idx]
  spproj$maxstorage_km3 = info$maxstorage_km3[idx]
  spproj$mean_annual_runoff_km3 = info$mar_km3[idx]
  write.csv(spproj,path2,row.names=FALSE)  
  print(i)
}
# etc for projection area by_sp
for( i in  1:nrow(spdata))
{
  path2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  spproj = fread(path2)
  spproj$etc = b1 + b2*log(spproj$udd2hilarri) + b3*log(spproj$IR)
  write.csv(spproj,path2,row.names=FALSE)  
}
# next sp occpts

for( i in 1:nrow(spdata))
{
  path2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  spproj = fread(path2)
  idx = match(spproj$udd2hilarri_nidid,info$nididfull)
  spproj$IR = info$IR[idx]
  spproj$maxstorage_km3 = info$maxstorage_km3[idx]
  spproj$mean_annual_runoff_km3 = info$mar_km3[idx]
  write.csv(spproj,path2,row.names=FALSE)  
  print(i)
}
for( i in  1:nrow(spdata))
{
  path2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  spproj = fread(path2)
  spproj$etc = b1 + b2*log(spproj$udd2hilarri) + b3*log(spproj$IR)
  write.csv(spproj,path2,row.names=FALSE)  
}



## get rid of dams that have 0 IR.
# by huc
for( i in 1:18)
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  spproj = fread(path1)
  idx = which(spproj$IR==0)
  if(length(idx)>0)
  {
    spproj$IR[idx] = NA
    spproj$maxstorage_km3[idx] = NA
    spproj$mean_annual_runoff_km3[idx] = NA
    spproj$etc[idx] = NA
    spproj$udd2hilarri[idx] = NA
    spproj$udd2hilarri_nidid[idx] = NA
    spproj$udd2hilarri_umdamcomid[idx] = NA
    write.csv(spproj,path1,row.names=FALSE)
    print(i)
  }
}

# sp proj
for( i in 1:nrow(spdata))
{
  path2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  spproj = fread(path2)
  idx = which(spproj$IR==0)
  if(length(idx)>0)
  {
    spproj$IR[idx] = NA
    spproj$maxstorage_km3[idx] = NA
    spproj$mean_annual_runoff_km3[idx] = NA
    spproj$etc[idx] = NA
    spproj$udd2hilarri[idx] = NA
    spproj$udd2hilarri_nidid[idx] = NA
    spproj$udd2hilarri_umdamcomid[idx] = NA
    write.csv(spproj,path2,row.names=FALSE)
    print('from projarea')
    print(i)
  }
  path2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  spproj = fread(path2)
  
  idx = which(spproj$IR==0)
  if(length(idx)>0)
  {
    spproj$IR[idx] = NA
    spproj$maxstorage_km3[idx] = NA
    spproj$mean_annual_runoff_km3[idx] = NA
    spproj$etc[idx] = NA
    spproj$udd2hilarri[idx] = NA
    spproj$udd2hilarri_nidid[idx] = NA
    spproj$udd2hilarri_umdamcomid[idx] = NA
    write.csv(spproj,path2,row.names=FALSE)
    print('from occpts')
    print(i)
  }
}
# sp occpt