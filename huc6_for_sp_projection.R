#for yetta, get unique huc 6s for each species' projection area (including the 2 species in per_sp/yetta)
library(dplyr)
# second round of code to get huc6s for each species' projection area using unfiltered occurrence data.
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spnamedata2 = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_allyr.csv')
spnamedata2 = spnamedata2[which(!(spnamedata2$name %in% spnamedata$name)),]
comid2huc = read.csv('G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/CrosswalkTable_NHDplus_HU12.csv')
spnamedata = spnamedata2

for(i in 115:nrow(spnamedata))
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/no_date_filter/%s_wcomid.csv',spnamedata$name[i])
  d <- try({
    read.csv(filename)
  }, silent = TRUE)
  if(class(d)!= "try-error")
  {
    if(!is.null(d$huc))
    {
      d$huc12 = d$huc
      d$huc2 = floor(d$huc12/10^10)
      d = select(d, -c(huc))
      write.csv(d,filename,row.names=FALSE)
    } else
    {
      d$huc12 = NA
      d$huc2 = NA
      write.csv(d,filename,row.names=FALSE)
    }
  }
}


for(i in 29:nrow(spnamedata))
{
  # make sure the species in the species list has occurrence data.
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/no_date_filter/%s_wcomid.csv',spnamedata$name[i])
  d <- try({
    read.csv(filename)
  }, silent = TRUE)
  if(class(d)!= "try-error")
  {
    # get huc unit for the comid of all occ pts.
    huc = apply(as.matrix(d$comid),1,function(x) comid2huc$HUC_12[which(comid2huc$FEATUREID==x)])
    if(length(huc)>0)
    {
      print('length greater than 0')
      if(class(huc)=='list')
      {
        huc = sapply(huc,function(x) if(length(x)==0){x=NA} else {x=x})
      }
      print(i)
      d$huc = huc
      
      # get unique huc6's from all the huc units of the occ pts.
      huc6 = as.data.frame(unique(floor(huc[which(!is.na(huc) & huc>0)]/10^6)))
      names(huc6) = c('huc6')
      
      ofilename0 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/huc6_allsp_b4filtering/%s_huc6.csv',spnamedata$name[i])
      write.csv(huc6,ofilename0,row.names=FALSE)
      
      # save the huc unit for the occ data.
      ofilename1 = filename
      write.csv(d,ofilename1,row.names=FALSE)
    }
  }
}













# first round of code I made to get huc6 s for each species' projection area. This was not good enough for thermal indication project
# that yetta was doing because there were lots of huc6s without any species. This is probably because a lot of the species occurrence data
# were filtered out during data processing. 
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
for( i in 1:nrow(spnamedata))
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])  
  d = read.csv(filename)
  var = unique(floor(d$huc12/10^6))
  dd = data.frame(huc6=var)
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/huc6/%s_huc6.csv',spnamedata$name[i])
  write.csv(dd,ofilename,row.names=FALSE)
}
# do it for yetta's special request species. 
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
filename = 'G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/CrosswalkTable_NHDplus_HU12.csv'
com2huc = read.csv(filename)

for( i in 1:nrow(spnamedata))
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])  
  d = read.csv(filename)
  idx = match(d$comid,com2huc$FEATUREID)
  d$huc12 = com2huc$HUC_12[idx]
  ofilename = filename
  write.csv(d,ofilename,row.names=FALSE)
}

var = unique(floor(d$huc12/10^6))
dd = data.frame(huc6=var)
ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/huc6/%s_huc6.csv','Moxostoma robustum')
write.csv(dd,ofilename,row.names=FALSE)


filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/for_yetta/%s_projarea.csv','Salvelinus confluentus')  
d = read.csv(filename)
huc2s = unique(d$huc2)
d$huc12 = NA
for(i in huc2s)
{
  com2huc2 = com2huc[which(floor(com2huc$HUC_12/10^10)==i),]
  idx = match(d$comid[which(d$huc2==i)],com2huc2$FEATUREID)
  d$huc12[which(d$huc2==i)] = com2huc2$HUC_12[idx]
}
var = unique(floor(d$huc12/10^6))
dd = data.frame(huc6=var)
ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/huc6/%s_huc6.csv','Salvelinus confluentus')
write.csv(dd,ofilename,row.names=FALSE)


