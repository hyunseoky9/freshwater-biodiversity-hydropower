# counts the number of species predicted to be present for each huc8 units in the CONUS (1) and plots them (2). 
# (3) calculates the species count differences between scenarios.

library(ggplot2)
theme_set(theme_bw())
library(pbapply)
library(data.table)
library(sf)
library(raster)
library(sp)
library(ggthemes)
library(viridis)

rm(list=ls())
#1 get species count per huc8 unit. (ALL DONE DON'T RUN)
currentofuture = 'future' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
scenario = 'pristine w gcm'
threshold = 0.4 # netsymdiff threshold
{
# get all 8digit hucs
#crosswalk = read.csv('G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/CrosswalkTable_NHDplus_HU12.csv')
#huc8 = unique(ceiling(crosswalk$HUC_12/10^4))
huc8 =read.csv('G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/huc8 derived from the crosswalk table2.csv')
huc8 = as.array(huc8$huc8)

spcount = huc8 # first column spcount dataframe is the huc8 units.

# get list of species in the study with sufficient data
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
spnamedata_onlystudysp = spnamedata[idx0,]

#for every sp.
not_all_matched = c()
for(i in 1:nrow(spnamedata_onlystudysp))
{
  # get modeled predicted occ data per sp
  if(gcmver!= 'GCM_average')
  {
    fname = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_binary_predictions.csv',scenario,gcmver,currentofuture,spnamedata_onlystudysp$name[i],spnamedata_onlystudysp$name[i])    
  } else {
    if(currentofuture=='current')
    {
      if(scenario=='pristine w gcm')
      {
        scenario2 = 'currentpri'        
      } else {
        scenario2 = 'baseline'
      }
    } else {
      if(scenario=='pristine w gcm')
      {
        scenario2 = 'futurepri'
      } else {
        scenario2 = 'futureres'
      }
    }
    fname = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_%s.csv',gcmver,spnamedata_onlystudysp$name[i],spnamedata_onlystudysp$name[i],scenario2)
  }

  occpred = read.csv(fname)
  # match the modeled distribution data to huc units.
  projectionareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata_onlystudysp$name[i])
  proj = fread(projectionareafilename)
  proj = proj[which(proj$wbmID_30sec_netsymdiff<threshold),]
  
  matchidx = match(occpred$comid,proj$comid)
  if(any(is.na(matchidx)))
  {
    not_all_matched = c(not_all_matched,i)
  }
  
  occpred$huc8 = ceiling(proj$huc12[matchidx]/10^4)
  #occpred$huc8 = crosswalk$HUC_12[apply(as.matrix(occpred$comid),1,function(x) which(crosswalk$FEATUREID==x))
  
  # match the above to the list of 8digit hucs  
  presenthuc8 = unique(occpred$huc8[which(occpred$maxsss==1)])
  idx = apply(as.matrix(presenthuc8),1,function(x) which(huc8==x))
  if(class(idx)=='list')
  {
    idx = unlist(idx)
  }
  spoccperhuc8 = rep(0,length(huc8))
  spoccperhuc8[idx] = 1
  spcount = cbind(spcount,spoccperhuc8)
  print(i)
}

# add up the presences of species for all 8digit hucs
spcount = cbind(spcount,rowSums(spcount[,2:ncol(spcount)]))
spcount = as.data.frame(spcount)
names(spcount) = c('HUC8',spnamedata_onlystudysp$name,'total number of species')

if(scenario=='pristine w gcm')
{
  scenariostr = 'pri'  
} else
{
  scenariostr = 'res'
}
ofilename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
#write.csv(spcount,ofilename,row.names=FALSE)
}

#1.2 get turnover rate (beta divesity) per huc8 unit (from Bulsson et al. 2010)
{
# call in 
gcmver = 'GCM_average'
threshold = 0.4
filenamepricur = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,'current','pri')
filenameprifut = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,'future','pri')
filenamerescur = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,'current','res')
filenameresfut = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,'future','res')
pricur = fread(filenamepricur)
prifut = fread(filenameprifut)
rescur = fread(filenamerescur)
resfut = fread(filenameresfut)

onlyfish = 0
onlymussel = 1
if(onlyfish | onlymussel)
{
  spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
  spnames = names(rescur)[2:(length(rescur)-1)]
  spnames = sub('\\.',' ',spnames)
  spnames[127] = "Erimystax x-punctatus" # has hyphen(-) in the name that's turned into '.'
  a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
  if(onlyfish)
  {
    spidx = which(a==1)+1 # listed fish species idx  
  }
  if(onlymussel)
  {
    spidx = which(a==0)+1 # listed mussel species idx      
  }
  # calculate turnover between rescur to resfut
  ogsp = as.data.frame(rescur)[,spidx]
  sr = rowSums(as.data.frame(rescur)[,spidx])
  
  newsp = as.data.frame(resfut)[,spidx]
  spdiff = ogsp - newsp
  sl = apply(spdiff,1,function(x) length(which(x==1))) # species lost
  sg = apply(spdiff,1,function(x) length(which(x==-1))) # species gain
  turnover_rescur2resfut = 100*(sg+sl)/(sr+sg) # NaN are areas where there were no species to begin with and there won't be any new species in a new scenario (resfut)  
  
  # calculate turnover between rescur to pricur
  newsp = as.data.frame(pricur)[,spidx] # new species composition
  spdiff = ogsp - newsp
  sl = apply(spdiff,1,function(x) length(which(x==1))) # species lost
  sg = apply(spdiff,1,function(x) length(which(x==-1))) # species gain
  turnover_rescur2pricur = 100*(sg+sl)/(sr+sg) # NaN are areas where there were no species to begin with and there won't be any new species in a new scenario (resfut)
  
  # calculate turnover between pricur to prifut
  ogsp = as.data.frame(pricur)[,spidx] # original species composition
  sr = rowSums(as.data.frame(pricur)[,spidx])
  newsp = as.data.frame(prifut)[,spidx] # new species composition
  spdiff = ogsp - newsp
  sl = apply(spdiff,1,function(x) length(which(x==1))) # species lost
  sg = apply(spdiff,1,function(x) length(which(x==-1))) # species gain
  turnover_pricur2prifut = 100*(sg+sl)/(sr+sg) # NaN are areas where there were no species to begin with and there won't be any new species in a new scenario (resfut)
  
  turnover = data.frame(pricur$HUC8,
                        rescur2resfut=turnover_rescur2resfut,
                        rescur2pricur=turnover_rescur2pricur,
                        pricur2prifut=turnover_pricur2prifut)
  if(onlyfish)
  {
    ofilename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/turnover_onlyfish.csv',gcmver)    
  }
  if(onlymussel)
  {
    ofilename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/turnover_onlymussel.csv',gcmver)    
  }
} else
{
  # calculate turnover between rescur to resfut
  ogsp = rescur[,2:(ncol(rescur)-1)] # original species composition
  sr = rescur$`total number of species`
  newsp = resfut[,2:(ncol(rescur)-1)] # new species composition
  spdiff = ogsp - newsp
  sl = apply(spdiff,1,function(x) length(which(x==1))) # species lost
  sg = apply(spdiff,1,function(x) length(which(x==-1))) # species gain
  turnover_rescur2resfut = 100*(sg+sl)/(sr+sg) # NaN are areas where there were no species to begin with and there won't be any new species in a new scenario (resfut)
  
  # calculate turnover between rescur to pricur
  newsp = pricur[,2:(ncol(pricur)-1)] # new species composition
  spdiff = ogsp - newsp
  sl = apply(spdiff,1,function(x) length(which(x==1))) # species lost
  sg = apply(spdiff,1,function(x) length(which(x==-1))) # species gain
  turnover_rescur2pricur = 100*(sg+sl)/(sr+sg) # NaN are areas where there were no species to begin with and there won't be any new species in a new scenario (resfut)
  
  # calculate turnover between pricur to prifut
  ogsp = pricur[,2:(ncol(pricur)-1)] # original species composition
  sr = pricur$`total number of species`
  newsp = prifut[,2:(ncol(prifut)-1)] # new species composition
  spdiff = ogsp - newsp
  sl = apply(spdiff,1,function(x) length(which(x==1))) # species lost
  sg = apply(spdiff,1,function(x) length(which(x==-1))) # species gain
  turnover_pricur2prifut = 100*(sg+sl)/(sr+sg) # NaN are areas where there were no species to begin with and there won't be any new species in a new scenario (resfut)
  
  turnover = data.frame(pricur$HUC8,
                        rescur2resfut=turnover_rescur2resfut,
                        rescur2pricur=turnover_rescur2pricur,
                        pricur2prifut=turnover_pricur2prifut)
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/turnover.csv',gcmver)
}
write.csv(turnover,ofilename,row.names=FALSE)
}
#2.  plotting spcount in the map (figure a and b)
rm(list=ls())
onlyfish = 1
onlymussel =0
{
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

# read in 8digit huc shapefile
huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)
great_lakes = c(4020300,4060200,4080300,4150200,4120200)
huc8conus = huc8conus[-which(huc8conus$HUC8 %in% great_lakes),]

# read in 2digit huc shapefile
huc2conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc2.shp')
huc2conus$HUC2 


#make a new field in the vector file of huc8 with the total count of species.
#spcount = spcount_currentpri$total.number.of.species
subject_scenario = spcount_currentres
spcount_lite = as.data.table(cbind(subject_scenario$HUC8,subject_scenario$total.number.of.species))
names(spcount_lite) = c('HUC8','spcnt')

if(onlyfish | onlymussel)
{
  spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
  spnames = names(spcount_futureres)[2:(length(spcount_futureres)-1)]
  spnames = sub('\\.',' ',spnames)
  spnames[127] = "Erimystax x-punctatus" # has hyphen(-) in the name that's turned into '.'
  a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
  if(onlyfish)
  {
    spidx = which(a==1)+1 # listed fish species idx  
  }
  if(onlymussel)
  {
    spidx = which(a==0)+1 # listed mussel species idx      
  }
  spcount = rowSums(subject_scenario[,spidx])
  spcount_lite = as.data.table(cbind(subject_scenario$HUC8,spcount))
  names(spcount_lite) = c('HUC8','spcnt')
}

d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)

#plot(d['total_sum'])
library(ggthemes)

ggplot(data = d) +
  geom_sf() +
  geom_sf(data = d, aes(fill = spcnt)) +
  geom_sf(data = huc2conus, color = "red", fill = NA, size = 0.6) +  # This adds the HUC2 boundaries
  scale_fill_viridis_c(option='D',direction=1) + 
  labs(fill = "number of species") +
  theme_map()
}

# 3. calculates the species count differences between scenarios. (figure c and d)
# load species count data for 3 scenarios (rescurrent,resfuture, pricurrent)
rm(list=ls())
onlyfish = 1
onlymussel = 0
{
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
gcmver = 'GCM_average'
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
gcmver = 'GCM_average'
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

resdiff = spcount_currentpri$total.number.of.species-spcount_currentres$total.number.of.species
climdiff = spcount_futureres$total.number.of.species-spcount_currentres$total.number.of.species

if(onlyfish | onlymussel)
{
  spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
  spnames = names(spcount_futureres)[2:(length(spcount_futureres)-1)]
  spnames = sub('\\.',' ',spnames)
  spnames[127] = "Erimystax x-punctatus" # has hyphen(-) in the name that's turned into '.'
  a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
  if(onlyfish)
  {
    spidx = which(a==1)+1 # listed fish species idx  
  }
  if(onlymussel)
  {
    spidx = which(a==0)+1 # listed mussel species idx      
  }
  resdiff = rowSums(spcount_currentpri[,spidx]) - rowSums(spcount_currentres[,spidx])
  climdiff = rowSums(spcount_futureres[,spidx]) - rowSums(spcount_currentres[,spidx])
 }



# percentage of huc8s that have change in species numbers
# due to reservoir effect
1-length(which(resdiff==0))/length(resdiff)
# due to climate change effect
1-length(which(climdiff==0))/length(climdiff)
mean(climdiff)
sd(climdiff)/sqrt(length(climdiff))
t.test(climdiff)

# 3.1  plotting the spcount difference btw scenarios (you have to run 3. first)
# read in 8digit huc shapefile
huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)
great_lakes = c(4020300,4060200,4080300,4150200,4120200)
huc8conus = huc8conus[-which(huc8conus$HUC8 %in% great_lakes),]
# read in 2digit huc shapefile
huc2conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc2.shp')
huc2conus$HUC2 


#make a new field in the vector file of huc8 with the total count of species.
diff = climdiff
spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,diff))
names(spcount_lite) = c('HUC8','dtsn')

d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)


#plot(d['total_sum'])

scaled_values = d$dtsn
scaled_values[which(is.na(scaled_values))] = 0
scaled_values[which(scaled_values<0)] = scaled_values[which(scaled_values<0)]/abs(min(scaled_values[which(scaled_values<0)]))
scaled_values[which(scaled_values>0)] = scaled_values[which(scaled_values>0)]/max(scaled_values[which(scaled_values>0)])
get_color <- function(val) {
  if (val < 0) {
    # Scale between light red and red for negative values
    return(colorRampPalette(c("#ffbaba", "#a70000"))(100)[as.integer(-val * 99) + 1])
  } else {
    # Scale between light blue and blue for positive values
    return(colorRampPalette(c("#b3cde0", "#011f4b"))(100)[as.integer(val * 99) + 1])
  }
}
fillcolor <- sapply(scaled_values, get_color)
fillcolor[which(d$dtsn==0)] = '#a1a4a2'

# Create the gradient bar
gradient_data <- data.frame(x = 1, y = seq(0, 1, length.out = 200))
# negative gradient bar
ggplot(gradient_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = y)) +
  scale_fill_gradient(low = "#ffbaba", high = "#a70000") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), # Removes grid lines
    panel.border = element_blank(), # Removes panel border
    axis.line = element_blank() # Removes axis lines
  )
# positive gradient bar
ggplot(gradient_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = y)) +
  scale_fill_gradient(low = "#b3cde0", high = "#011f4b") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), # Removes grid lines
    panel.border = element_blank(), # Removes panel border
    axis.line = element_blank() # Removes axis lines
  )

d$color = fillcolor
ggplot(data = d) +
  geom_sf() +
  geom_sf(data = d, aes(fill = color),color='black') +
  scale_fill_identity() +
  geom_sf(data = huc2conus, color = "#39FF14", fill = NA, size = 0.6) +  # This adds the HUC2 boundaries
  labs(fill = "future sp # - 
       baseline sp #") +
  theme_map()
}

# 4. plotting the species turnover btw scenarios. (figure e and f)
rm(list=ls())
onlyfish = 1
onlymussel = 0
{
gcmver = 'GCM_average'
spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/turnover.csv',gcmver)
spturnover = read.csv(spcountfilename1)
if(onlyfish)
{
  spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/turnover_onlyfish.csv',gcmver)
  spturnover = read.csv(spcountfilename1)
}
if(onlymussel)
{
  spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/turnover_onlymussel.csv',gcmver)
  spturnover = read.csv(spcountfilename1)
}
#choose which two scenarios you're going to get the turnover between them for

# read in 8digit huc shapefile
huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)
great_lakes = c(4020300,4060200,4080300,4150200,4120200)
huc8conus = huc8conus[-which(huc8conus$HUC8 %in% great_lakes),]

# read in 2digit huc shapefile
huc2conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc2.shp')
huc2conus$HUC2 

names(spturnover)[1] = 'HUC8'
d = merge(huc8conus,spturnover,by='HUC8',all.x=TRUE,sort=FALSE)

mean(na.omit(spturnover$rescur2resfut))
sd(na.omit(spturnover$rescur2resfut))/sqrt(length(na.omit(spturnover$rescur2resfut)))
library(ggthemes)

#ggplot(data = d) +
#  geom_sf() +
#  geom_sf(data = d, aes(fill = rescur2resfut)) +
#  scale_fill_viridis_c(option='D',direction=1) + 
#  labs(fill = "turnover rate from
#       baseline to future") +
#  theme_map()

# plot with different scaling where turnover rate higher than 50
# has same color. (used for fish, use the upper one for mussels) 
d$rescur2resfut_50 = d$rescur2resfut
d$rescur2resfut_50[which(d$rescur2resfut_50>50)] = 50

if(onlyfish)
{
  ggplot(data = d) +
    geom_sf() +
    geom_sf(data = d, aes(fill = rescur2resfut_50)) +
    geom_sf(data = huc2conus, color = "red", fill = NA, size = 0.6) +  # This adds the HUC2 boundaries
    scale_fill_viridis_c(option='D',direction=1) + 
    labs(fill = "turnover rate from
       current to future") +
    theme_map()
} else
{
  ggplot(data = d) +
    geom_sf() +
    geom_sf(data = d, aes(fill = rescur2resfut)) +
    geom_sf(data = huc2conus, color = "red", fill = NA, size = 0.6) +  # This adds the HUC2 boundaries
    scale_fill_viridis_c(option='D',direction=1) + 
    labs(fill = "turnover rate from
       current to future") +
    theme_map()
}
}

# test plot
# data for test plotting
#testd = st_read('G:/My Drive/research/sdm_modeling/gis/tl_2017_us_state.shp')
#testd$idx = 1:nrow(testd)
#testd$idx[1:10] = NA
#
#ggplot(data = testd) +
#  geom_sf() +
#  geom_sf(data = testd, aes(fill = idx)) +
#  scale_fill_viridis_c(option='D',direction=1) +
#  theme_map()
