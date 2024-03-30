# get the processed species occpt data again.

# move the wcomid data to spdata/
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/no_date_filter'
files = list.files(path)
files = files[which(grepl('_wcomid',files))]
for ( i in 1:length(files))
{
  oldpath = path
  filename = sprintf('%s/%s',oldpath,files[i])
  file = read.csv(filename)
  newpath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
  ofilename = sprintf('%s/%s',newpath,files[i])
  write.csv(file, ofilename, row.names=FALSE)
}

# get rid of reaches with na comid.
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  spdata = read.csv(filename)
  rid_idx = which(is.na(spdata$comid))
  if(length(rid_idx)>0)
  {spdata_new = spdata[-rid_idx,]} else
  {
    spdata_new = spdata
  }
  if(nrow(spdata_new)<30)
  {
    # get rid of species that already have less than 30 occ pt. 
    unlink(filename)
  } else
  {
    write.csv(spdata_new,filename,row.names=FALSE)
  }
  #if(nrow(spdata)<30)
  #{
    #unlink(filename)
  #  print(sprintf('%s removed',files[i]))
  #}
}

# get rid of reaches with year collected before 2000, after 2020, and NA year.
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  file = read.csv(filename)
  #namidpt= which(is.na(file$decimalLongitude))
  idx = which(file$year <2000 | file$year > 2020 | is.na(file$year))
  if(length(idx))
  {
    print(i)
    #file = file[-idx,]
    #write.csv(file,filename,row.names=FALSE)
  }
  #if(length(namidpt))
  #{
  #  print(i)
  #  print(length(namidpt))
  #  print('\n')
  #}
  #print(filename)
  #print(i)
  #print(head(file))
  #readline()
}






# get rid of reaches with duplicate comid
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  spdata = read.csv(filename)
  rid_idx = which(duplicated(spdata$comid))
  if(length(rid_idx)>0)
  {
    print(i)
    spdata_new = spdata[-rid_idx,]
    write.csv(spdata_new,filename,row.names=FALSE)
  }
}




path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]

for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  spdata = read.csv(filename)
  #if(length(which(is.na(spdata$comid)))>0)
  {
    print(length(which(is.na(spdata$comid))))
  }
  #if(nrow(spdata)<30)
  #{
  #unlink(filename)
  #  print(sprintf('%s removed',files[i]))
  #}
}



# recycle midpoints and ftype that are already there 
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
for ( i in 1:17)
{
  ofile_exist = files[i] %in% oldfiles
  if(ofile_exist)
  {
    newfilename = sprintf('%s/%s',path,files[i])
    oldfilename = sprintf('%s/%s',opath,oldfiles[which(oldfiles == files[i])])
    newfile = read.csv(newfilename)
    oldfile = read.csv(oldfilename)
    newmerge = merge(newfile,oldfile,by.x = 'comid',by.y='comid',all=TRUE)
    # decimalLongitude/Latitude.y is comid midpoints
    # decimalLongitude/Latitude.x is gbif coordinates
    # d-.x should become sampleptlong/lat
    # d-.y should become d-
    newmerge$decimalLongitude = newmerge$decimalLongitude.y
    newmerge$decimalLatitude = newmerge$decimalLatitude.y
    newmerge$sampleptlat = newmerge$decimalLatitude.x
    newmerge$sampleptlong = newmerge$decimalLongitude.x
    # get rid of duplicate attr
    newmerge = newmerge[,c(names(newmerge)[which(!grepl('\\.y',names(newmerge)))])]
    # get rid of '.x' marks on the attr names and get rid of decimalLong/Lat.x
    xnames = which(grepl('\\.x',names(newmerge)))
    newmerge = newmerge[,c(names(newmerge)[which(!grepl('decimalLongitude\\.x',names(newmerge)))])]
    newmerge = newmerge[,c(names(newmerge)[which(!grepl('decimalLatitude\\.x',names(newmerge)))])]
    names(newmerge)[xnames] = gsub('\\.x','',names(newmerge)[xnames])
    write.csv(newmerge,newfilename,row.names=FALSE)
  } else # if there's no matching previous data
  {
    filename = sprintf('%s/%s',path,files[i])
    file = read.csv(filename)
    file$sampleptlat = file$decimalLatitude
    file$sampleptlong = file$decimalLongitude
    file$decimalLatitude = NA
    file$decimalLongitude = NA
    file$ftype = NA
    file$waterbody = NA
    file$wbd_classes = NA
    file$hdamid = NA
    file$mode = NA
    write.csv(file,filename,row.names=FALSE)
  }
}





# data processing check tool
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
probspidx = c()
for ( i in 1:length(files))
{
    filename = sprintf('%s/%s',path,files[i])
    file = read.csv(filename)
    if(!'ftype' %in% names(file) )
    {
      print(i)
    }
    #namidpt= which(is.na(file$decimalLongitude))
    #idx = which(file$year <2000 | file$year > 2020 | is.na(file$year))
    #if(length(idx))
    #{
    #  probspidx = c(probspidx,i)
    #}
    #if(length(namidpt))
    #{
    #  print(i)
    #  print(length(namidpt))
    #  print('\n')
    #}
    #print(filename)
    #print(i)
    #print(head(file))
    #readline()
}











# get midpoints and ftype that aren't already there. 
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
library(sf)
library(sp)
library(pbapply)
library(nhdplusTools)
# getting midpoint
### RUN THE FUNCTION MIDPOINT IN get_midpoint_of_occpts_comids.R before running the below for-loop
for ( i in 1:17)
{
  filename = sprintf('%s/%s',path,files[i])
  d0 = read.csv(filename)
  getmididx = which(is.na(d0$decimalLatitude)) # indexes that needs midpoint coords
  if(length(getmididx)>0)
  {
    d = d0[getmididx,]
    comids = d$comid
    # get the midpoints using apply.
    coords = pbapply(as.matrix(comids),1,midpoint)
    coords = as.data.frame(t(matrix(unlist(coords),nrow=2)))
    d0$decimalLongitude[getmididx] = coords[,1]
    d0$decimalLatitude[getmididx] = coords[,2]
    #points = sfheaders::sf_point(coords)
    
    ofilename = filename
    write.csv(d0,ofilename,row.names=FALSE)
  }
  
  print(sprintf('%d/%d done',i,length(files)))
}



# get rid of occpts with no midopint (na)
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  file = read.csv(filename)
  namidpt = which(is.na(file$decimalLatitude))
  if(length(namidpt)>0)
  {
    file = file[-namidpt,]
  }
  write.csv(file,filename,row.names=FALSE)
}


# exclude occ pts outside of conus
us = readOGR('G:/My Drive/research/sdm_modeling/gis/2018_us_outline.shp')
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  spdata = read.csv(filename)
  spcoords_sf = st_as_sf(spdata, coords = c("decimalLongitude", "decimalLatitude"), 
                         crs = 4326, agr = "constant")
  # resource: https://campus.datacamp.com/courses/spatial-analysis-with-sf-and-raster-in-r/preparing-layers-for-spatial-analysis?ex=10
  spcoords_sp = as(spcoords_sf,Class='Spatial')
  spcoords_sp = spTransform(spcoords_sp, CRS(proj4string(us)))
  spcoords_subset <- spcoords_sp[us, ] # take occpts only in CONUS
  #which reach idxs are not in conus
  comid.notinCONUS =which(! spcoords_sp$comid %in% spcoords_subset$comid)
  
  if(length(comid.notinCONUS))
  {
    print(sprintf('occpts outside of CONUS detected i=%d',i))
    #spdata = spdata[-comid.notinCONUS,]
  }
  #write.csv(spdata,filename,row.names=FALSE)
}

# delete species with less than 30 occurrence
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  file = read.csv(filename)
  if(nrow(file)<30)
  {
    #print(i)
    unlink(filename)
  }
}

# delete select marine species
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
splist =  gsub('_wcomid.csv','',files)

exclude.marinesp = c('Dasyatis sabina','Anchoa mitchilli','Paralichthys lethostigma','Syngnathus scovelli','Strongylura marina','Mugil cephalus',
               'Ameiurus brunneus', 'Hiodon alosoides', 'Hiodon tergisus', "Acanthogobius flavimanus", "Awaous banana", "Ctenogobius boleosoma",
               "Ctenogobius shufeldti", "Eucyclogobius newberryi", "Gillichthys mirabilis", "Gobionellus oceanicus", "Lentipes concolor",
               "Neogobius melanostomus", "Proterorhinus semilunaris", "Sicyopterus stimpsoni", "Tridentiger bifasciatus")
for(i in 1:length(exclude.marinesp))
{
  sp = exclude.marinesp[i]
  if (sp %in% splist)
  {
    spidx = which(splist == sp)
    filename = sprintf('%s/%s',path,files[spidx])
    #print(files[spidx])
    unlink(filename)
  }
}
#exclude.marinesp[which(apply(as.matrix(exclude.marinesp),1,function(x,sp) x %in% sp, sp= splist)==FALSE)]
#sum(apply(as.matrix(exclude.marinesp),1,function(x,sp) x %in% sp, sp= splist))



# getting rid of species that are not in comprehensive_sp_info_b4waterbodycalc.csv
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
persp = gsub("_wcomid.csv",'',files)
spdatab4wbd = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_b4waterbodycalc.csv')
idx = which(apply(as.matrix(persp),1,function(x,sp) ! x %in% sp, sp= spdatab4wbd$name))
for( i in idx)
{
  filename = sprintf('%s/%s',path,files[i])
  unlink(filename)
}




# play codes
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
i=53
filename = sprintf('%s/%s',path,files[i])
file = read.csv(filename)
nrow(file)
oldfilename = sprintf('%s/%s',opath,oldfiles[which(oldfiles == files[i])])
oldfile = read.csv(oldfilename)
nrow(oldfile)

idx = which(! file$comid %in% oldfile$comid) # which comid in new file not in old file

temp = file$comid[-idx]
length(temp)

path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
spdatab4wbd = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_b4waterbodycalc.csv')
idx = which(!files %in% oldfiles)
for( i in idx)
{
filename = sprintf('%s/%s',path,files[i])
file = read.csv(filename)
}
spdata  = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spdatab4wbd = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_b4waterbodycalc.csv')
oldspdata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_allyr.csv')
occnum = c()
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  file = read.csv(filename)
  occnum[i] = nrow(file)
}
length(occnum)
head(files,10)
head(spdatab4wbd,10)

filename = sprintf('%s/%s',path,files[1])
file = read.csv(filename)
nrow(file)
spdatab4wbd[1,]


cbind(occnum,c(spdatab4wbd$occnum1,rep(NA,length(files) - nrow(spdatab4wbd))))





# getting ftype
### RUN THE FUNCTION wbd_class_identify IN waterbody_indicator_calc_parallel.R before running the below for-loop
library(parallel)
library(MASS)
library(dplyr)
library(pbapply)
n.cores <- detectCores()
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]

for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  d0 = read.csv(filename)
  getftypeidx = which(is.na(d0$ftype)) # indexes that needs midpoint coords
  print(length(getftypeidx))
  if(length(getftypeidx)>0)
  {
    data = d0[getftypeidx,]
    comid_all = data$comid 
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
      # make a dataframe with ftype and waterbody related stuff
      key =  data.frame(comid=as.integer(subset$NHDFlowline_Network$comid),ftype=subset$NHDFlowline_Network$ftype)
      wbd_idx = which(key$ftype=='ArtificialPath')
      key$waterbody = rep(0,nrow(key))
      key$wbd_classes = rep('NA',nrow(key))
      # get waterbody types for ariticial flowlines.
      # not running the waterbody type calculation as of now since the function seems to output 
      # inaccurate result.
      #if(length(wbd_idx)>0)
      #{
      #  key$waterbody[wbd_idx] = 1
      #  clust <- makeCluster(n.cores)
      #  clusterEvalQ(clust, {
      #    library(nhdplusTools)
      #    library(sf)
      #    library(dplyr)
      #  })
      #  xx = parApply(clust,as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
      #  stopCluster(clust)
      #  key$wbd_classes[wbd_idx] = xx
      #  #key$wbd_classes[wbd_idx] = pbapply(as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
      #}
      if((finishidx - startidx +1)!= nrow(key))
      {
        sprintf("data length of key and data are unequal.")
        newdata = rbind(newdata,key)
      } else {
        newdata = rbind(newdata,key)
      }
      print(sprintf('%d/%d done',j,numinterval))
    }
    newdata[,c('comid','ftype','waterbody','wbd_classes')]
    
    d0$ftype[getftypeidx] = newdata$ftype
    d0$waterbody[getftypeidx] = newdata$waterbody
    d0$wbd_classes[getftypeidx] = newdata$wbd_classes
    #points = sfheaders::sf_point(coords)
    
    ofilename = filename
    #write.csv(d0,ofilename,row.names=FALSE)
  }
  print(sprintf('%d/%d done',i,length(files)))
}

# get rid of occpts that is outside of CONUS


# get rid of species that have less than 30 occ pts and update the comprehensive sp info 
# you may need to add back in some species to the comprehensive
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
opath = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/wbd_calc_mishap'
oldfiles = list.files(opath)
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  file = read.csv(filename)
  if(nrow(file)<30)
  {
    print(sprintf('%d has less than 30 occpts',i))
    #unlink(filename)
  }
}



# check if any species in the per_sp file is not in the comprehensive species info.
path = 'G:/My Drive/research/sdm_modeling/spdata/per_sp'
files = list.files(path)
files = files[which(grepl('.csv',files))]
persp = gsub("_wcomid.csv",'',files)
spdata  = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
oldspdata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_allyr.csv')
for(i in 1:length(persp))
{
  if(! persp[i] %in% spdata$name)
  {
    print(i)
  }
}






















# recover projection area reaches. 

# recycle midpoints and ftype that are already there 
path = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/temp'
files = list.files(path)
files = files[which(grepl('.csv',files))]

opath = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/wbd_calc_mishap'
oldfiles = list.files(opath)
for ( i in 1:length(files))
{
  newfilename = sprintf('%s/%s',path,files[i])
  newfile = read.csv(newfilename)
  #names(newfile)[2] = 'comid'
  print(head(newfile))
  #write.csv(newfile,newfilename,row.names=FALSE)
}

# recycle midpoints and ftype.
for ( i in 18)#1:length(files))
{
  ofile_exist = files[i] %in% oldfiles
  if(ofile_exist)
  {
    newfilename = sprintf('%s/%s',path,files[i])
    oldfilename = sprintf('%s/%s',opath,oldfiles[which(oldfiles == files[i])])
    newfile = read.csv(newfilename)
    oldfile = read.csv(oldfilename)
    newmerge = merge(newfile,oldfile,by.x = 'comid',by.y='comid',all=TRUE)
    newmerge = subset(newmerge, select=-c(X.x,X.y))
    newmerge$X = 1:nrow(newmerge)
    write.csv(newmerge,newfilename,row.names=FALSE)
  }
}

# get udd  

damdistdata = read.csv('G:/My Drive/research/sdm_modeling/gis/distance2dam_data.csv')
damdistdata = data.frame(cbind(damdistdata$UM2D,damdistdata$COMIDV2))
names(damdistdata) = c('updist','comid')
path = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/temp'
files = list.files(path)
files = files[which(grepl('.csv',files))]

for(i in 9)#1:length(files))#looplength)#1:length(spname_list))
{
  # **spdata could mean species occurrence points or projection area.
  hucnum = i
  projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/temp/huc%d.csv",hucnum)
  data = read.csv(projfilename)
  # code next line only works if you haven't gotten the ftype for areas that didn't have ftype data in the projection area data in wbd_mishap.
  undecided.udd.idx = which(is.na(data$ftype))
  #print(head(data$udd[undecided.udd.idx],300))
  if(length(undecided.udd.idx))
  {
    data$udd[undecided.udd.idx] = damdistdata$updist[match(data$comid[undecided.udd.idx],damdistdata$comid)] # udd=upstream dam distance
    write.csv(data,projfilename,row.names = FALSE)    
  }
}





# get ftype that weren't in wbd_mishap data
### RUN THE FUNCTION wbd_class_identify IN waterbody_indicator_calc_parallel.R before running the below for-loop
library(parallel)
library(MASS)
library(dplyr)
library(pbapply)
library(nhdplusTools)
n.cores <- detectCores()
path = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc'
files = list.files(path)
files = files[which(grepl('.csv',files))]

for (i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  d0 = read.csv(filename)
  getftypeidx = which(is.na(d0$ftype)) # indexes that needs midpoint coords
  print(length(getftypeidx))
  if(length(getftypeidx)>0)
  {
    data = d0[getftypeidx,]
    comid_all = data$comid 
    intlen = 1000 # interval length. 
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
      # make a dataframe with ftype and waterbody related stuff
      key =  data.frame(comid=as.integer(subset$NHDFlowline_Network$comid),ftype=subset$NHDFlowline_Network$ftype)
      wbd_idx = which(key$ftype=='ArtificialPath')
      key$waterbody = rep(0,nrow(key))
      key$wbd_classes = rep('NA',nrow(key))
      print(nrow(key))
      # get waterbody types for ariticial flowlines.
      # not running the waterbody type calculation as of now since the function seems to output 
      # inaccurate result.
      #if(length(wbd_idx)>0)
      #{
      #  key$waterbody[wbd_idx] = 1
      #  clust <- makeCluster(n.cores)
      #  clusterEvalQ(clust, {
      #    library(nhdplusTools)
      #    library(sf)
      #    library(dplyr)
      #  })
      #  xx = parApply(clust,as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
      #  stopCluster(clust)
      #  key$wbd_classes[wbd_idx] = xx
      #  #key$wbd_classes[wbd_idx] = pbapply(as.matrix(key$comid[wbd_idx]),1,wbd_class_identify)
      #}
      if((finishidx - startidx +1)!= nrow(key))
      {
        sprintf("data length of key and data are unequal.")
        newdata = rbind(newdata,key)
      } else {
        newdata = rbind(newdata,key)
      }
      print(sprintf('%d/%d done',j,numinterval))
    }
    newdata[,c('comid','ftype','waterbody','wbd_classes')]
    
    order = apply(as.matrix(newdata$comid),1,function(x,d) which(d == x),d= d0$comid)
    d0$ftype[order] = newdata$ftype
    d0$waterbody[order] = newdata$waterbody
    d0$wbd_classes[order] = newdata$wbd_classes
    #d0[order[1:100],c('ftype','waterbody','wbd_classes')]
    
    print(sprintf('d0$ftype length = %d',length(d0$ftype)))
    print(sprintf('getftypeidx length = %d',length(getftypeidx)))
    print(sprintf('nrow(newdata)= %d',nrow(newdata)))
    print(sprintf('num na ftype=%d',length(which(is.na(d0$ftype)))))
    print(sprintf('length(newdata$ftype)=%d',length(newdata$ftype)))
    #points = sfheaders::sf_point(coords)
    
    ofilename = filename
    write.csv(d0,ofilename,row.names=FALSE)
  }
  print(sprintf('%d/%d done',i,length(files)))
}


path = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc'
files = list.files(path)
files = files[which(grepl('.csv',files))]
nas = c()
for ( i in 1:length(files))
{
  filename = sprintf('%s/%s',path,files[i])
  d0 = read.csv(filename)
  getftypeidx = which(is.na(d0$decimalLatitude)) # indexes that needs midpoint coords
  print(length(getftypeidx))
  nas[i] = length(getftypeidx)
  print(sprintf('%d/%d done',i,length(files)))
}

# get midpoints that weren't in wbd_mishap data
path = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc'
files = list.files(path)
files = files[which(grepl('.csv',files))]
library(sf)
library(sp)
library(pbapply)
library(nhdplusTools)
# getting midpoint
### RUN THE FUNCTION MIDPOINT IN get_midpoint_of_occpts_comids.R before running the below for-loop
for ( i in 10)
{
  filename = sprintf('%s/%s',path,files[i])
  d0 = read.csv(filename)
  getmididx = which(is.na(d0$decimalLatitude)) # indexes that needs midpoint coords
  if(length(getmididx)>0)
  {
    d = d0[getmididx,]

    comids = d$comid
    # get the midpoints using apply.
    coords = pbapply(as.matrix(comids),1,midpoint)
    coords = as.data.frame(t(matrix(unlist(coords),nrow=2)))
    d0$decimalLongitude[getmididx] = coords[,1]
    d0$decimalLatitude[getmididx] = coords[,2]
    #points = sfheaders::sf_point(coords)
    
    ofilename = filename
    write.csv(d0,ofilename,row.names=FALSE)
  }
  
  print(sprintf('%d/%d done',i,length(files)))
}





# check num na's in midpoints in projected areas
path = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc'
files = list.files(path)
files = files[which(grepl('.csv',files))]
library(sf)
library(sp)
library(pbapply)
library(nhdplusTools)
# getting midpoint
### RUN THE FUNCTION MIDPOINT IN get_midpoint_of_occpts_comids.R before running the below for-loop
for ( i in 10)
{
  filename = sprintf('%s/%s',path,files[i])
  d0 = read.csv(filename)
  print(i)
  print(length(which(is.na(d0$decimalLongitude))))
}




