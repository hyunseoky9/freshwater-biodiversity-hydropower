# centroid of occurrence comid coordinates and their shift between current reservoir and future 
# reservoir scenario
library(sf)
library(data.table)
library(geosphere)
library(dplyr)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

centroid_cr_sf = list()
centroid_fr_sf = list()
for(i in idx0)
{
  filename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_baseline.csv',spnamedata$name[i],spnamedata$name[i])
  currentres = fread(filename1)
  filename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_futureres.csv',spnamedata$name[i],spnamedata$name[i])
  futureres = fread(filename2)
  filename3 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_currentpri.csv',spnamedata$name[i],spnamedata$name[i])
  currentpri = fread(filename3)
  filename4 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_futurepri.csv',spnamedata$name[i],spnamedata$name[i])
  futurepri = fread(filename4)
  
  #boxplot(cbind(currentres$probability,futureres$probability))
  #boxplot(cbind(currentpri$probability,futurepri$probability))
  #
  #boxplot(cbind(currentres$probability,currentpri$probability))
  #boxplot(cbind(futureres$probability,futurepri$probability))
  
  filename5 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[i])
  proj = fread(filename5)
  
  occcomid_cr = currentres$comid[which(currentres$maxsss==1)]
  occcomid_fr = currentres$comid[which(futureres$maxsss==1)]
  length(occcomid_cr)
  coords_cr = data.frame(long=proj$decimalLongitude[which(proj$comid %in% occcomid_cr)],lat=proj$decimalLatitude[which(proj$comid %in% occcomid_cr)])
  coords_fr = data.frame(long=proj$decimalLongitude[which(proj$comid %in% occcomid_fr)],lat=proj$decimalLatitude[which(proj$comid %in% occcomid_fr)])
  
  coords_crsf = st_as_sf(as.data.frame(coords_cr),coords=c('long','lat'),crs = 4326)
  coords_frsf = st_as_sf(as.data.frame(coords_fr),coords=c('long','lat'),crs = 4326)
  
  centroid_cr_sf[[which(idx0==i)]] = coords_crsf %>% st_union %>% st_centroid
  centroid_fr_sf[[which(idx0==i)]] = coords_frsf %>% st_union %>% st_centroid
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}
unlist(centroid_cr_sf)
centroid_cr_df = cbind(unlist(centroid_cr_sf)[seq(1,length(unlist(centroid_cr_sf)),by=2)],
      unlist(centroid_cr_sf)[seq(2,length(unlist(centroid_cr_sf)),by=2)])
centroid_fr_df = cbind(unlist(centroid_fr_sf)[seq(1,length(unlist(centroid_fr_sf)),by=2)],
                       unlist(centroid_fr_sf)[seq(2,length(unlist(centroid_fr_sf)),by=2)])

centroid_cr_df = as.data.frame(centroid_cr_df)
centroid_fr_df = as.data.frame(centroid_fr_df)
names(centroid_cr_df) = c('long','lat')
names(centroid_fr_df) = c('long','lat')
ofilename_cr = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/sp_dist_centroid_currentres.csv'
ofilename_fr = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/sp_dist_centroid_futureres.csv'
#write.csv(centroid_cr_df,ofilename_cr,row.names=FALSE) 
#write.csv(centroid_fr_df,ofilename_fr,row.names=FALSE)
centroid_cr_df = read.csv(ofilename_cr)
centroid_fr_df = read.csv(ofilename_fr)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]
crlat = centroid_cr_df$lat
frlat = centroid_fr_df$lat
latdiff = frlat-crlat
naidx = which(apply(centroid_fr_df,1,function(x) any(is.na(x))))
mean(centroid_cr_df$lat[naidx]) 
mean(centroid_fr_df$lat[naidx]) # -14 because 14 has no future range.

fishlatdiff = latdiff[which(fishidx==1)]
mussellatdiff = latdiff[which(fishidx==0)]
tempidx  = spnamedata$thermal_pref[idx0]
warmlatdiff = latdiff[which(tempidx==1)]
coollatdiff = latdiff[which(tempidx==2)]
coldlatdiff = latdiff[which(tempidx==0)]

mean(na.omit(fishlatdiff))
mean(mussellatdiff)
mean(na.omit(warmlatdiff))
mean(coollatdiff)
mean(coldlatdiff)
sd(na.omit(fishlatdiff))/sqrt(length(na.omit(fishlatdiff)))
sd(mussellatdiff)/sqrt(length(mussellatdiff))
sd(na.omit(warmlatdiff))/sqrt(length(na.omit(warmlatdiff)))
sd(coollatdiff)/sqrt(length(coollatdiff))
sd(coldlatdiff)/sqrt(length(coldlatdiff))
par(mfrow=c(1,1))
boxplot(fishlatdiff,warmlatdiff,coollatdiff,coldlatdiff,mussellatdiff,
        names=c('Fish','Warm water','Cool water','Cold water','Mussel'))
