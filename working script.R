library(sf)
library(sp)
library(nhdplusTools)
library(beepr)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
which(spnamedata$name=='Catostomus clarkii')

#dir = 'G:/My Drive/research/sdm_modeling/gis/2018_us_outline.shp'
#conus_shape <- read_sf(dir)
dir = 'G:/My Drive/research/sdm_modeling/gis/tl_2017_us_state.shp'
conus_shape <- read_sf(dir)

#idx = c(19,43,65,85,108,109,111,113,142,170,231,232,235,237,245,246,258,259,274,302,303,304,313,330,409,410,413,422,438,443,446,498,503,504,545,554,563,582)
numocc_rmf0 = c() # numocc after removing minflow 0
for ( i in 1:nrow(spnamedata))
{
  if(spnamedata$iod_wbmmatching[i]==1)
  {
    spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    spdata = read.csv(spdatafilename)
    
    # rid low match score points
    spdata = spdata[which(spdata$wbmID_30sec_netsymdiff>0.4),]
    print(nrow(spdata))
    if(length(spdata$minflow)!=0)
    {
      par(mfrow=c(1,2))
      hist(spdata$minflow,main=sprintf('historgram of numdays below tmin (%f) for %s',spnamedata$tmin[i],spnamedata$name[i]))  
      plot(sf::st_geometry(conus_shape),lwd=2)
      spcoords_sf = st_as_sf(spdata, coords = c("decimalLongitude", "decimalLatitude"), 
                             crs = 4326, agr = "constant")   
      plot(spcoords_sf,col='blue',add=TRUE)
      plot(spcoords_sf[which(spcoords_sf$minflow==0),],col='red',add=TRUE)
      readline()
    }
  }
}

# number of occurrence
numoccs = c()
numoccs2 = c()
for ( i in 1:nrow(spnamedata))
{
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  spdata = read.csv(spdatafilename)
  numoccs[i] = length(which(spdata$wbmID_30sec_netsymdiff>0.4))
  numoccs2[i] = nrow(spdata)
}

sorted  = sort(numoccs,decreasing=T)
idx0 = which(sorted>=30)
idx0 = idx0[length(idx0)]
idx = order(numoccs,decreasing=T)[1:idx0]

# number of species with >30 occpts above symmetricdiff score of x.
insp = c()
inthsp = c()
netsymdiffth = seq(0.2,0.9,by=0.1)
for ( j in netsymdiffth)
{
  numoccs = c()
  for ( i in 1:nrow(spnamedata))
  {
    spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    spdata = read.csv(spdatafilename)
    numoccs[i] = length(which(spdata$wbmID_30sec_netsymdiff>j))
  }
  insp = c(insp,length(which(numoccs>30)))
  inthsp = c(inthsp,length(which(numoccs>30 & spnamedata$threatened==1)))
}
plot(insp~netsymdiffth,ylab='number of species with sufficient data pts (>30)',
     xlab='net symmetric difference threshold',type='l')

plot(inthsp~netsymdiffth,ylab='number of threatened species with sufficient data pts (>30)',
     xlab='net symmetric difference threshold',type='l')

nauddp = c()
for ( i in 1:nrow(spnamedata))
{
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  spdata = read.csv(spdatafilename)
  nauddp[i] = length(which(is.na(spdata$udd2)))/nrow(spdata)
}




order(numoccs)
idx = sort(numoccs)[400:500]
catchmentareadiff = c()
streamorderdiff = c()
idx = c( 96, 142, 369,538)
for ( i in 1:length(idx))
{
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[idx[i]])
  spdata = read.csv(spdatafilename)
  subset_file <- tempfile(fileext = ".gpkg")
  
  subset <- subset_nhdplus(comids = spdata$comid,
                           output_file = subset_file,
                           nhdplus_data = "download",
                           flowline_only = FALSE,
                           return_data = TRUE, overwrite = TRUE)
  
  good = which(spdata$wbmID_30sec_netsymdiff>=0.4)
  bad = which(spdata$wbmID_30sec_netsymdiff<0.4)
  catchmentareadiff[i] = mean(subset$CatchmentSP$areasqkm[good]) - mean(subset$CatchmentSP$areasqkm[bad])
  streamorderdiff[i] = mean(subset$NHDFlowline_Network$streamorde[good]) - mean(subset$NHDFlowline_Network$streamorde[bad])
}
beep(sound=6)



numocc = c()
numocc_rmf0 = NA # numocc after removing minflow 0
for ( i in 1:nrow(spnamedata))
{
  if(spnamedata$iod_wbmmatching[i]==1)
  {
    spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    spdata = read.csv(spdatafilename)
    
    # rid low match score points
    spdata = spdata[which(spdata$wbmID_30sec_netsymdiff>0.4),]
    numocc[i] = nrow(spdata)
    if(length(spdata$minflow)!=0)
    {
      numocc_rmf0[i] = nrow(spdata[which(spdata$minflow!=0),])
    }
  }
}

numocc_rmf02 = numocc_rmf0[!is.na(numocc_rmf0)]
length(which(numocc_rmf02>30))


# see if minflow is mostly 0 by hucs.

for ( i in 1:nrow(spnamedata))
{
    spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    spdata = read.csv(spdatafilename)
    hist(spdata$wbmID_30sec_netsymdiff)
    readline()
}

for ( i in 1:18)
{
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/temp_and_flow_predictors/by_hucs/huc%d_projarea_tempNflow_predictors.csv',11)
  d = read.csv(ofilename)
  #hist(d$minflow,main=sprintf('huc%d minflow',i))
  hist(d$avgflow,main=sprintf('huc%d avgflow',i))
  readline()
}

hist(d$maxflow)



# 0 minflow and catchment area/ stream order
for(  i in 1:18)
{
  i=1
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine/temp_and_flow_predictors/by_hucs/huc%d_projarea_tempNflow_predictors.csv',i)
  d = read.csv(ofilename)
  filename2 = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",i)
  d2 = read.csv(filename2)
  d3 = merge(d2,d)
  hist(d3$minflow)
  c1 = d3$catchment_areasqkm[which(d3$minflow==0)] 
  #c1 = c1[!is.na(c1)]
  #mean(c1)
  c2 = d3$catchment_areasqkm[which(d3$minflow>0 & d3$minflow<=10)] 
  #c2 = c2[!is.na(c2)]
  #mean(c2)
  c3 = d3$catchment_areasqkm[which(d3$minflow>10 & d3$minflow<=20)] 
  #c3 = c3[!is.na(c3)]
  #mean(c3)
  e = d3$catchment_areasqkm[which(is.na(d3$wbmID_30sec_netsymdiff))]
  
  head(f)
  f$wbmID_30sec_naive
  a = d3$catchment_areasqkm[which(d3$wbmID_30sec_netsymdiff<=0.4)]
  #b = d3$catchment_areasqkm[which(d3$wbmID_30sec_netsymdiff>0.4)]
  b = d3[which(d3$wbmID_30sec_netsymdiff>0.4),]
  b1 = b$catchment_areasqkm[which(b$minflow==0)] 
  #c1 = c1[!is.na(c1)]
  mean(b1)
  b2 = b$catchment_areasqkm[which(b$minflow>0 & b$minflow<=10)] 
  #c2 = c2[!is.na(c2)]
  mean(b2)
  b3 = b$catchment_areasqkm[which(b$minflow>10 & b$minflow<=20)] 
  mean(b3)
  
  f = d3[which(!is.na(d3$wbmID_30sec_netsymdiff)),]
  f1 = f[which(f$minflow==0),]
  f2 = f[which(f$minflow>0),]
  mean(f1$wbmID_30sec_netsymdiff)
  mean(f2$wbmID_30sec_netsymdiff)
  hist(f1$wbmID_30sec_netsymdiff)
  hist(f2$wbmID_30sec_netsymdiff)
}

#vif analysis
bg= bckdata
for( i in 1:ncol(bckdata))
{
  bg = bg[which(!is.na(bg[,i])),]
}
library(usdm)
usdm::vif(bg)

# projection area integrity check
faulty_projarea  = c()


for ( i in 1:nrow(spnamedata))
{
  projectionareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  result = try({spdata = read.csv(projectionareafilename)})
  if(class(result) == 'try-error')
  {
    faulty_projarea = c(faulty_projarea,i)
  }
  print(i)
}
check <- function(idx)
{
  projectionareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[idx])  
  result = try({spdata = read.csv(projectionareafilename)})
  if(class(result) == 'try-error')
  {
    return(0)
  } else {
    return(1)
  }
}
library(data.table)

dd = c()
for(i in 1:nrow(spnamedata))
{
  projectionareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[idx])  
  result = try({spdata = fread(projectionareafilename)})
  proj2 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/temp/%s_projarea.csv',spnamedata$name[idx])
  spdata2 = fread(proj2)
  dd[i] = nrow(spdata)-nrow(spdata2)
}


ddd = pbapply(as.matrix(1:nrow(spnamedata)),1,check2)
beep(sound=5)
