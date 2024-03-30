# wbm data processing code
# wbm data link: https://cloud.environmentalcrossroads.net/s/ZT4TGz3NBFFodp9?path=%2Fdata%2F01min%2FSDM%2Ffuture%2FBCC-CSM2-MR%2FCONUS_DOE-BCC-CSM2-MR-Runoff%2BWBM20WTempPrist_01min

library(data.table)
library(beepr)
path ='G:/My Drive/research/sdm_modeling/wbmdata/pristine'
path2 ='G:/My Drive/research/sdm_modeling/wbmdata/pristine/reference_table'
path3 = 'G:/My Drive/research/sdm_modeling/wbmdata/pristine/hyun_01min_sample'
path4 = 'G:/My Drive/research/sdm_modeling/wbmdata/pristine/hyun_01min_sample/hyun_01min_sample/CSVTables_split'
# first, erase all the data for year thats not in 2000-2019 (DONE)
files = list.files(path4)
noyear = as.character(1980:1998)

for(i in noyear)
{
  unlink(paste0(path4,'/',files[grep(i,files)]))
}


# take out all the reference files from endless number of directories... (DONE)
hucnum = 18
for(i in 2:hucnum)
{
  file.copy(from = paste0(sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/hyun_01min_sample/hyun_01min_sample/RGISlocal_csv/CONUS/Huc%d/Hyun+HydroSTN30/01min/Static/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv.gz',i,i)),
            to = paste0(path2, sprintf('/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv.gz',i)))
}


# converting the raw temp/flow data from CUNY collaborators and turning it into 
# csv table that is in (# cell)X(# days of the yr) format (call it version 2, "v2")
hucnum = 18
currentofuture = 'future' #1=current, 0=future
gcmver = 'DOE-BCC-CSM2-MR'
if(currentofuture=='current')
{
  start = 2000
  finish = 2019
  opath = sprintf('D:/sdm_modeling/wbm/%s/pristine w gcm/current',gcmver)
  path = sprintf('E:/sdm_modeling/wbm raw data/%s/current',gcmver)
} else
{
  start = 2060
  finish = 2079
  opath = sprintf('D:/sdm_modeling/wbm/%s/pristine w gcm/future',gcmver)
  path = sprintf('E:/sdm_modeling/wbm raw data/%s/future',gcmver)
}
path2 ='G:/My Drive/research/sdm_modeling/wbmdata/pristine/reference_table'
yridx = c(20)

for(i in 1:18)
{
  reffilename = sprintf('%s/CONUS_Huc%d_Hyun+HydroSTN30_01min_Static.csv',path2,i)
  ref = read.csv(reffilename)
  cellnum = nrow(ref)
  for(j in 1:(finish-start + 1))
  {
    # make the original temp and discharge data into a matrix form where col is dates and row is cell
    yr = (j-1) + start
    #tempfilename = sprintf('%s/Huc%d_CONUS_Output_RiverTemperature_%s-Runoff+WBM20WTempPrist_01min_dTS%d.csv',path,i,gcmver,yr)
    #d = fread(tempfilename,header=TRUE)
    #if(! nrow(d)/cellnum %in% c(365,366))
    #{
    #  print('cell num not matching')
    #}
    #newd = apply(as.matrix(1:(nrow(d)/cellnum)),1,function(x) d$RiverTemperature[((x-1)*cellnum+1):(x*cellnum)])
    #newd = as.data.frame(newd)
    #newd = cbind(ref$CellID,newd) # add a column of cellid. 
    #colnames(newd) = c('1minCellID',as.character(1:365))
    #outputtempfilename = sprintf('%s/Huc%d_CONUS_Output_RiverTemperature_%s-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',opath,i,gcmver,yr)
    #write.csv(newd,outputtempfilename,row.names=FALSE)
    
    flowfilename = sprintf('%s/Huc%d_CONUS_Output_RiverDischarge_%s-Runoff+WBM20WTempPrist_01min_dTS%d.csv',path,i,gcmver,yr)
    d = fread(flowfilename,header=TRUE)
    newd = apply(as.matrix(1:(nrow(d)/cellnum)),1,function(x) d$RiverDischarge[((x-1)*cellnum+1):(x*cellnum)])
    newd = as.data.frame(newd)
    newd = cbind(ref$CellID,newd)
    colnames(newd) = c('1minCellID',as.character(1:365)) # add a column of cellid. 
    outputtempfilename = sprintf('%s/Huc%d_CONUS_Output_RiverDischarge_%s-Runoff+WBM20WTempPrist_01min_dTS%d_v2.csv',opath,i,gcmver,yr)
    write.csv(newd,outputtempfilename,row.names=FALSE)
    print(sprintf('%d done',yr))
  }
}
# copy temperature wbm raw data version2 from pristine to reservoir folder in D:
gcmver = 'DOE-CNRM-ESM2-1'
currentofuture = 'future'
opath = sprintf('D:/sdm_modeling/wbm/%s/pristine w gcm/%s',gcmver,currentofuture)

files = list.files(opath)
files = files[grepl('RiverTemperature',files)]
files = paste(opath,files,sep='/')
to = sprintf("D:/sdm_modeling/wbm/%s/pristine_gcm_reservoir/%s",gcmver,currentofuture)
file.copy(files, to, overwrite = FALSE, recursive = FALSE,
          copy.mode = TRUE, copy.date = FALSE)







# get all 1min cell id onto projected area (both by_huc and by_sp) (DONE)
#by_huc first
hucnum = 18
for(i in 1:hucnum)
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',i)
  d = read.csv(filename)
  reffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/for_snapping/1min_snapping/huc%d_1min_tosnap_ref.csv',i)
  ref = read.csv(reffilename)
  # NAs in matching?
  idx = match(d$wbmID_30sec_snapped,ref$CellID_30sec)
  d$wbmID_1min = ref$CellID_01min[idx]
  write.csv(d,filename,row.names=FALSE)
}
# now for by_species.
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
for(i in 1:nrow(spnamedata))
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  d = read.csv(filename)
  relhucs = unique(d$huc2)
  d$wbmID_1min = NA
  for( j in relhucs)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',j)
    d2 = read.csv(filename)
    idx = match(d$wbmID_30sec_snapped,d2$wbmID_30sec_snapped)
    d$wbmID_1min[which(!is.na(idx))] = d2$wbmID_1min[idx[which(!is.na(idx))]]
  }
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  print(sprintf('number of NAs or 1min cellid = %d',sum(is.na(d$wbmID_1min))))
  write.csv(d,ofilename,row.names=FALSE)
  print(i)
}
# now for species presence data. (DONE)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
nomatches_found = c()
for(i in 496)#281:nrow(spnamedata))
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  d = read.csv(filename)
  d$wbmID_1min = NA
  filename2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  d2 = read.csv(filename2)
  idx = match(d$wbmID_30sec_snapped,d2$wbmID_30sec_snapped)
  if(sum(is.na(idx))>0)
  {
    which(is.na(idx))
    nomatches_found = c(nomatches_found,i)
    print(sprintf('number of NAs in the match = %d',sum(is.na(idx))))
  }
  d$wbmID_1min = d2$wbmID_1min[idx]
  #print(sprintf('number of NAs or 1min cellid = %d',sum(is.na(d$wbmID_1min))))
  ofilename = filename
  if(sum(is.na(idx))==0)
  {
    write.csv(d,ofilename,row.names=FALSE)
  }
  print(i)
}
