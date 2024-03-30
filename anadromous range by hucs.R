# get a shp file for species
library(ggthemes)
library(ggplot2)
theme_set(theme_bw())
library(pbapply)
library(data.table)
library(sf)
library(raster)
library(sp)

rm(list=ls())
filename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spdata = read.csv(filename)
anadsp = spdata$name[which(spdata$diadromous.Yettainput=='1' | spdata$diadromous.Yettainput=='partial')]
huc8conus_og = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus = huc8conus_og
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)

# load in spcount data for 3 different scenario given the gcm
{
  currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
  gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
  scenario = 'pristine_gcm_reservoir'
  threshold = 0.4 # netsymdiff threshold
  if(scenario=='pristine w gcm')
  {
    scenariostr = 'pri'  
  } else
  {
    scenariostr = 'res'
  }
  spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
  spcount_currentres = read.csv(spcountfilename1)
  
  currentofuture = 'future' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
  gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
  scenario = 'pristine_gcm_reservoir'
  threshold = 0.4 # netsymdiff threshold
  if(scenario=='pristine w gcm')
  {
    scenariostr = 'pri'  
  } else
  {
    scenariostr = 'res'
  }
  spcountfilename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
  spcount_futureres = read.csv(spcountfilename2)
  
  currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
  gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
  scenario = 'pristine w gcm'
  threshold = 0.4 # netsymdiff threshold
  if(scenario=='pristine w gcm')
  {
    scenariostr = 'pri'  
  } else
  {
    scenariostr = 'res'
  }
  spcountfilename3 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
  spcount_currentpri = read.csv(spcountfilename3)
}

subject_scenario = spcount_currentres


for(i in 1:length(anadsp))
{
  # get species distribution predicted by SDM (current reservori scenario)
  spname = anadsp[i]
  spnamefile = gsub('\\.',' ',spname)
  
  spcount_lite = as.data.table(cbind(subject_scenario$HUC8,subject_scenario[,i]))  
  
  # get species study range 
  studyrange_wd = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors'
  rangefilename = sprintf('%s/%s_projarea_non-tempflowpredictors.csv',studyrange_wd,gsub('\\.',' ',spname))
  projarea = fread(rangefilename)
  studyarea_huc8 = unique(floor(projarea$huc12/10^4))
  studyarea_01 = rep(0,length(huc8conus$HUC8))
  studyarea_01[which(!is.na(match(huc8conus$HUC8,studyarea_huc8)))] = 1
  huc8conus = cbind(huc8conus,studyarea_01)
  spname_filename = strsplit(spname,' ')
  spname_filename[[1]][1] = substring(spname_filename[[1]][1], 1, 1)
  spname_filename[[1]][2] = substring(spname_filename[[1]][2], 1, 1)
  spname_filename = paste(spname_filename[[1]],collapse='.')
  names(huc8conus)[ncol(huc8conus)-1] = spname_filename
}
huc8conus
ofilename = 'G:/My Drive/research/sdm_modeling/spdata/sp_studyareaNoccurrencearea_maps/anadromous_sp_shapefile/studyarea.shp'
st_write(huc8conus, ofilename, delete_layer = TRUE)

## go through the study area of each anadromous species that are only partially anadromous.
## Pick out huc8s that you think are where the species would be non-anadromous.

# 

templatefilename = 'G:/My Drive/research/sdm_modeling/spdata/non-indigenous_byhuc8_studyspecies_(from NS data).csv'
template = read.csv(templatefilename)


d = template[,c(1,match(gsub(' ','.',anadsp),colnames(template)))]
for(i in 1:length(anadsp))
{
  spidx = which(spdata$name==anadsp[i])
  if(spdata$diadromous.Yettainput[spidx]=='1')
  {
    d[,i+1] = 1
  } else if (spdata$diadromous.Yettainput[spidx]=='partial')
  {
    result <- try({
      # select
      partialfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/sp_studyareaNoccurrencearea_maps/anadromous_sp_shapefile/%s_select.csv',anadsp[i])
      partialhuc8s = read.csv(partialfilename)    
      d[which(!is.na(match(d$HUC8,partialhuc8s$HUC8))),(i+1)] = 1
      d[which(is.na(match(d$HUC8,partialhuc8s$HUC8))),(i+1)] = 0
    }, silent = TRUE)
    if(class(result) == "try-error")
    {
      # deselect
      partialfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/sp_studyareaNoccurrencearea_maps/anadromous_sp_shapefile/%s_deselect.csv',anadsp[i])
      partialhuc8s = read.csv(partialfilename)
      d[which(!is.na(match(d$HUC8,partialhuc8s$HUC8))),(i+1)] = 0
      d[which(is.na(match(d$HUC8,partialhuc8s$HUC8))),(i+1)] = 1
      studyrange_wd = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors'
      rangefilename = sprintf('%s/%s_projarea_non-tempflowpredictors.csv',studyrange_wd,gsub('\\.',' ',anadsp[i]))
      projarea = fread(rangefilename)
      studyarea_huc8 = unique(floor(projarea$huc12/10^4))
      d[which(is.na(match(d$HUC8,studyarea_huc8))),i+1] = 0
    }
  }
}
head(d)
ofilename = 'G:/My Drive/research/sdm_modeling/spdata/diadromy_byhuc8_studyspecies.csv'
write.csv(d,ofilename,row.names=FALSE)
