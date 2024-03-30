#processing HILAREE hydropower subset data. Adding in comids mostly. Also, getting WBM 1minute cell id and cell center lat longs to add in runoff data for the hy edition.
# Also has adding in eia id into the hy edition.
library(nhdplusTools)
library(sf)
hilarrifilename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams.csv'
hilarri = read.csv(hilarrifilename)
hilarriorigin_filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2.csv'
hilarrio = read.csv(hilarriorigin_filename)
a = apply(as.matrix(hilarri$nididfull),1,function(x) hilarrio$nhdv2comid[which(hilarrio$nididfull==x)])
for(i in 1:length(a))
{
  if(length(a[[i]])>1)
  {
    a[[i]] = a[[i]][1]
  }
  if(length(a[[i]])==0)
  {
    a[[i]] = NA
  }
}
hilarri$comid = unlist(a,use.names=FALSE)



# try to see if NA comids can be found through nhdtool snapping process
idx = which(is.na(hilarri$comid))
occ_list = cbind(hilarri$longitude[idx],hilarri$latitude[idx])
comid_list = c()
for (i in 1:nrow(occ_list)) # for each occ pt get comid.
{
  occ = occ_list[i,]
  start_point <- st_sfc(st_point(occ), crs = 4269)
  start_comid <- tryCatch(
    {
      withCallingHandlers(discover_nhdplus_id(point = start_point), message = function(c) if (inherits(c, "message")){stop("")})
      discover_nhdplus_id(point = start_point)
    },
    error = function(e) {
      return(NA)
    }
  )
  #start_comid <- discover_nhdplus_id(start_point)
  comid_list[i] = start_comid
  print(i)
}
hilarri$comid[idx] = comid_list

ofilename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
write.csv(hilarri,ofilename,row.names=FALSE)




# checking if all the comids are consistent with nhdtool.
filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
for(i in 1529:nrow(hilarri))
{
  occ = cbind(hilarri$longitude[i], hilarri$latitude[i])
  start_point <- st_sfc(st_point(occ), crs = 4269)
  comid = discover_nhdplus_id(point = start_point)
  if(hilarri$comid[i]!=comid)
  {
    print('comid different!')
    hilarri$comid[i] = comid
  }
  print(i)
}
write.csv(hilarri,filename,row.names=FALSE)
occ = cbind(first.dam$longitude, first.dam$latitude)
start_point <- st_sfc(st_point(occ), crs = 4269)
discover_nhdplus_id(point = start_point)


# finding wbm 1 minute cell ids.

filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
filename2 = filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2.csv'
hilarri_origin = read.csv(filename)
idx = apply(as.matrix(hilarri$nididfull),1,function(x) which(hilarri_origin$nididfull==x))
for(i in 1:length(idx))
{
  idx[[i]] = idx[[i]][1]
}
length(idx)
idx = unlist(idx)
hilarri$huc_12 = hilarri_origin$huc_12[idx]
hilarri$huc_02 = hilarri_origin$huc_02[idx]
hilarri$wbmID_1min = NA
hilarri$wbmcelllat = NA
hilarri$wbmcelllong = NA

library(data.table)
for( i in c(11))
{
  relidx = which(hilarri$huc_02==i)
  comids = hilarri$comid[relidx]
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  d = read.csv(filename)
  idx = apply(as.matrix(comids),1,function(x) which(d$comid == x))
  if(class(idx) == 'list')
  {
    for(j in 1:length(idx))
    {
      if(length(idx[[j]])==0)
      {
        idx[[j]] = NA    
      }
    }
    idx = unlist(idx,use.names = FALSE)
  }

  if(sum(is.na(idx))>0)
  {
    print('theres nas..')
    print(sum(is.na(idx)))
    break
  }
  hilarri$wbmID_1min[relidx] = d$wbmID_1min[idx]
  
  filename2 = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/reference_table/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv',i)
  d2 = fread(filename2)
  idx2 = apply(as.matrix(d$wbmID_1min[idx]),1,function(x) which(d2$CellID==x))
  
  if(class(idx2) == 'list')
  {
    for(j in 1:length(idx2))
    {
      if(length(idx2[[j]])==0)
      {
        idx2[[j]] = NA    
      }
    }
    idx2 = unlist(idx2,use.names = FALSE)
  }
  hilarri$wbmcelllat[relidx] = d2$yCoord[idx2]
  hilarri$wbmcelllong[relidx] = d2$xCoord[idx2]
  print(i)
}

filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
write.csv(hilarri,filename,row.names=FALSE)



# get eia ids.
filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2_SubsetHydropowerDams_HYedition.csv'
hilarri = read.csv(filename)
filename2 = filename = 'G:/My Drive/research/sdm_modeling/dam data/HILARRI_v2/HILARRI_v2.csv'
hilarri_origin = read.csv(filename)
hilarri_origin$eia_ptid
idx = match(hilarri$nididfull,hilarri_origin$nididfull)
hilarri$eia_ptid = hilarri_origin$eia_ptid[idx]
write.csv(hilarri,filename,row.names=FALSE)

