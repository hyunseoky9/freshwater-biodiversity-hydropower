library(data.table)
# get average stream order between current and future scenario.
# separate between mussels and fish.

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

sodiff = c()
for( i in 1:length(idx0))
{
  # get occurrence prediction from gcm average for current and future res scenario
  filename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_baseline.csv',spnamedata$name[idx0[i]],spnamedata$name[idx0[i]])
  filename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_futureres.csv',spnamedata$name[idx0[i]],spnamedata$name[idx0[i]])
  curpred = fread(filename1)
  futpred = fread(filename2)
  curpredcomid = curpred$comid[which(curpred$maxsss==1)]
  futpredcomid = futpred$comid[which(futpred$maxsss==1)]
  # match the streamorder of the reaches where its predicted to occur
  filename3 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[idx0[i]])
  predictord = fread(filename3)  
  so = predictord[,c('comid','streamorder')]
  curmatchidx = match(curpredcomid,so$comid)
  futmatchidx = match(futpredcomid,so$comid)
  curso = so$streamorder[curmatchidx]
  futso = so$streamorder[futmatchidx]
  meansodiff = mean(futso) - mean(curso)
  sodiff = c(sodiff,meansodiff)
  print(sprintf('%d/%d',i,length(idx0)))
}
# *run from this line to just get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/streamorder_difference.csv'
#write.csv(sodiff,ofilename,row.names=FALSE)
sodiff = read.csv(ofilename)
sodiff = sodiff$x

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

fishsodiff = sodiff[which(fishidx==1)]
musselsodiff = sodiff[which(fishidx==0)]

hist(fishsodiff)
hist(musselsodiff)
mean(na.omit(fishsodiff))
sd(na.omit(fishsodiff))
sd(na.omit(fishsodiff))/sqrt(length(na.omit(fishsodiff)))
mean(na.omit(musselsodiff))
sd(na.omit(musselsodiff))
sd(na.omit(musselsodiff))/sqrt(length(na.omit(musselsodiff)))

# stats by warm, cool, and cold water fish
tempidx  = spnamedata$thermal_pref[idx0]
warmsodiff = sodiff[which(tempidx==1)]
coolsodiff = sodiff[which(tempidx==2)]
coldsodiff = sodiff[which(tempidx==0)]

mean(na.omit(warmsodiff))
sd(na.omit(warmsodiff))
sd(na.omit(warmsodiff))/sqrt(length(na.omit(warmsodiff)))
mean(na.omit(coolsodiff))
sd(na.omit(coolsodiff))
sd(na.omit(coolsodiff))/sqrt(length(na.omit(coolsodiff)))
mean(na.omit(coldsodiff))
sd(na.omit(coldsodiff))
sd(na.omit(coldsodiff))/sqrt(length(na.omit(coldsodiff)))
