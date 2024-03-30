# calculating risk metric by huc 8 for current reservoir effect and future reservoir effect scenario
# and looking at the difference between the two scenarios.
rm(list=ls())
library(sf)
library(data.table)
library(ggthemes)
library(ggplot2)
# get cbar data (we will use cbar value defined by ferc project scale)
cbard = read.csv('G:/My Drive/research/sdm_modeling/environmental mitigation data/average cost of mitigation (cbar) value.csv',row.names = 1)
cbard = cbard[1,] # use by_project values.


# get spcount data
{
# first, current reservoir spcount data
currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
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
spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
spcount_currentres = read.csv(spcountfilename1)


# second, future reservoir spcount data
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
}
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spnames = names(spcount_futureres)[2:(length(spcount_futureres)-1)]
spnames = sub('\\.',' ',spnames)
spnames[127] = "Erimystax x-punctatus" # has hyphen(-) in the name that's turned into '.'
# huc8 geospatial file
huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)


# 1.calculate difference in average mitigation cost for listed species btw currentres and futureres
# (future res - current res)
{
# find which species in the spcount data are listed
a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
b = apply(as.matrix(spnames),1,function(x) spnamedata$threatened[which(spnamedata$name==x)])
listedfishspidx = which((a+b)==2)+1 # listed fish species idx

# calculate listed spcount per huc8

listedspcount_futureres = rowSums(spcount_futureres[,listedfishspidx])
listedspcount_currentres = rowSums(spcount_currentres[,listedfishspidx])

# risk metric (R) calculation for both scenarios
R_currentres = cbard$avgcost_listed*listedspcount_currentres
R_futureres = cbard$avgcost_listed*listedspcount_futureres
# risk metric difference between the two scenarios (future - current reservoir effect scenario) 
Rdiff = R_futureres - R_currentres

# plot Rdiff


#make a new field in the vector file of huc8 with the total count of species.
spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,Rdiff))
names(spcount_lite) = c('HUC8','dtsn')

d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)

#plot(d['total_sum'])

ggplot(data = d) +
  geom_sf() +
  geom_sf(data = d, aes(fill = dtsn)) +
  scale_fill_viridis_c(option='D',direction=1) + 
  theme_map()
}

# 2. plot game species mitigation cost change
{
a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
b = apply(as.matrix(spnames),1,function(x) spnamedata$game[which(spnamedata$name==x)])
gamespidx = which((a+b)==2)+1 # listed fish species idx
gamespcount_futureres = rowSums(spcount_futureres[,gamespidx])
gamespcount_currentres = rowSums(spcount_currentres[,gamespidx])

R_currentres = cbard$avgcost_game*gamespcount_currentres
R_futureres = cbard$avgcost_game*gamespcount_futureres
Rdiff = R_futureres - R_currentres
spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,Rdiff))
names(spcount_lite) = c('HUC8','dtsn')
d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)

ggplot(data = d) +
  geom_sf() +
  geom_sf(data = d, aes(fill = dtsn)) +
  scale_fill_viridis_c(option='D',direction=1) + 
  labs(fill = "change in cost in the future") +
  theme_map()
}
# 3. plot invasive species mitigation cost change
dir = 'G:/My Drive/research/sdm_modeling/spdata'
filename = sprintf('%s/non-indigenous_byhuc8_studyspecies.csv',dir)
nonindi = read.csv(filename)

idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
nonindi = nonindi[,idx0+1]
dim(nonindi)
dim(spcount_currentres[,2:(ncol(spcount_currentres)-1)])
sp_presentNinvasive_currentres = spcount_currentres[,2:(ncol(spcount_currentres)-1)]*nonindi
sp_presentNinvasive_futureres = spcount_futureres[,2:(ncol(spcount_futureres)-1)]*nonindi
invasivespcount_futureres = rowSums(sp_presentNinvasive_futureres)
invasivespcount_currentres = rowSums(sp_presentNinvasive_currentres)

R_currentres = cbard$avgcost_invasive*invasivespcount_currentres
R_futureres = cbard$avgcost_invasive*invasivespcount_futureres
Rdiff = R_futureres - R_currentres
spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,Rdiff))
names(spcount_lite) = c('HUC8','dtsn')
d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)



# old way of calculating estimated cost regarding non-indigenous species.
# doesn't account for the fact that species may be non-indigenous in only some
# regions of CONUS
{
b = apply(as.matrix(spnames),1,function(x) spnamedata$nonindigenous[which(spnamedata$name==x)])
invasivespidx = which(b==1)+1 # listed fish species idx
invasivespcount_futureres = rowSums(spcount_futureres[,invasivespidx])
invasivespcount_currentres = rowSums(spcount_currentres[,invasivespidx])

R_currentres = cbard$avgcost_invasive*invasivespcount_currentres
R_futureres = cbard$avgcost_invasive*invasivespcount_futureres
Rdiff = R_futureres - R_currentres
spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,Rdiff))
names(spcount_lite) = c('HUC8','dtsn')
d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)

ggplot(data = d) +
  geom_sf() +
  geom_sf(data = d, aes(fill = dtsn)) +
  scale_fill_viridis_c(option='D',direction=1) + 
  labs(fill = "change in cost in the future") +
  theme_map()
}

# 4. plot mussel related mitigation cost change
{
  a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
  musselspidx = which(a==0)+1 # listed mussel species idx
  musselspcount_futureres = rowSums(spcount_futureres[,musselspidx])
  musselspcount_currentres = rowSums(spcount_currentres[,musselspidx])
  
  R_currentres = cbard$avgcost_mussel*musselspcount_currentres
  R_futureres = cbard$avgcost_mussel*musselspcount_futureres
  Rdiff = R_futureres - R_currentres
  spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,Rdiff))
  names(spcount_lite) = c('HUC8','dtsn')
  d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)
  ggplot(data = d) +
    geom_sf() +
    geom_sf(data = d, aes(fill = dtsn)) +
    scale_fill_viridis_c(option='D',direction=1) + 
    labs(fill = "change in cost in the future") +
    theme_map()
}

# 5. plot total mitigation cost change 
# including invasive species, listed species, and game species mitigation.
{
  a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
  b = apply(as.matrix(spnames),1,function(x) spnamedata$threatened[which(spnamedata$name==x)])
  listedfishspidx = which((a+b)==2)+1 # listed fish species idx

  a = apply(as.matrix(spnames),1,function(x) spnamedata$mussel_or_fish[which(spnamedata$name==x)])
  b = apply(as.matrix(spnames),1,function(x) spnamedata$game[which(spnamedata$name==x)])
  gamespidx = which((a+b)==2)+1 # listed fish species idx
  
  b = apply(as.matrix(spnames),1,function(x) spnamedata$nonindigenous[which(spnamedata$name==x)])
  invasivespidx = which(b==1)+1 # listed fish species idx 
  
  
  # species that are both listed and game are categorized as listed
  #length(gamespidx)
  #length(invasivespidx)
  listedgame = c(listedfishspidx,gamespidx)
  #listedgame[duplicated(listedgame)]
  newlistedspidx = listedfishspidx
  newgamespidx = gamespidx[-which(gamespidx %in% listedgame[duplicated(listedgame)])]
  # species that are both listed and invasive are categorized as listed
  listedinvasive = c(listedfishspidx,invasivespidx)
  #listedinvasive[duplicated(listedinvasive)]
  newinvasivespidx = invasivespidx[-which(invasivespidx %in% listedinvasive[duplicated(listedinvasive)])]
  # species that are both game and invasive are categorized as game
  gameinvasive = c(invasivespidx,gamespidx)
  #gameinvasive[duplicated(gameinvasive)]
  newinvasivespidx = newinvasivespidx[-which(newinvasivespidx %in% gameinvasive[duplicated(gameinvasive)])]
  
  
  length(listedfishspidx)+
  length(gamespidx)+
  length(invasivespidx)
  
  length(newlistedspidx)+
  length(newgamespidx)+
  length(newinvasivespidx)
  339-266
  
  length(unique(c(listedfishspidx,gamespidx,invasivespidx)))
  
  #allspidx = c(newlistedspidx,newinvasivespidx,newgamespidx)
  newlistedspcount_futureres = rowSums(spcount_futureres[,newlistedspidx])
  newlistedspcount_currentres = rowSums(spcount_currentres[,newlistedspidx])
  newgamespcount_futureres = rowSums(spcount_futureres[,newgamespidx])
  newgamespcount_currentres = rowSums(spcount_currentres[,newgamespidx])
  newinvasivespcount_futureres = rowSums(spcount_futureres[,newinvasivespidx])
  newinvasivespcount_currentres = rowSums(spcount_currentres[,newinvasivespidx])

  R_currentres = cbard$avgcost_listed*newlistedspcount_currentres + 
    cbard$avgcost_game*newgamespcount_currentres + 
    cbard$avgcost_invasive*newinvasivespcount_currentres
  R_futureres = cbard$avgcost_listed*newlistedspcount_futureres + 
    cbard$avgcost_game*newgamespcount_futureres + 
    cbard$avgcost_invasive*newinvasivespcount_futureres
  Rdiff = R_futureres - R_currentres
  spcount_lite = as.data.table(cbind(spcount_currentres$HUC8,Rdiff))
  names(spcount_lite) = c('HUC8','dtsn')
  d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)
  
  ggplot(data = d) +
    geom_sf() +
    geom_sf(data = d, aes(fill = dtsn)) +
    scale_fill_viridis_c(option='D',direction=1) + 
    labs(fill = "change in cost in the future (2022$)") +
    theme_map()
}



# file containing cost shift data for huc8's (derived from above codes)
# and summary analysis
filename = 'G:/My Drive/research/sdm_modeling/draft/cost_shift.csv'
d = read.csv(filename)
apply(d,2,function(x) 100*length(which(!is.na(x) & x!=0))/length(which(!is.na(x)))) # average cost shift excluding huc8s with no change in cost.
apply(d,2,function(x) mean(x[which(!is.na(x) & x!=0)])) # average cost shift excluding huc8s with no change in cost.
apply(d,2,function(x) sd(x[which(!is.na(x) & x!=0)])) # sd
apply(d,2,function(x) sd(x[which(!is.na(x) & x!=0)])/sqrt(length(x))) # se



