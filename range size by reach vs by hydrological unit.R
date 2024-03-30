# find out why avg spcount change per huc8 is negative for fish but the avg range size change is positive


subhuc8 = 11020010

spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/GCM_average/df2_total_spcount_nsd0.4_futureres.csv')
spcount_futureres = read.csv(spcountfilename1)
spcountfilename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/GCM_average/df2_total_spcount_nsd0.4_currentres.csv')
spcount_currentres = read.csv(spcountfilename2)
#future spcount
futurespidx = which(spcount_futureres[which(spcount_futureres$HUC8==subhuc8),]==1)
length(futurespidx)
#current spcount 
currentspidx = which(spcount_currentres[which(spcount_currentres$HUC8==subhuc8),]==1)
length(currentspidx)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
idx0[futurespidx]
idx0[currentspidx]

a = colSums(spcount_futureres)-colSums(spcount_currentres)
a = a[2:(length(a)-1)]
boxplot(a[2:(length(a)-1)])
b = a
names(b) = c()

ofilename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/analysis/netdistributionchange_percentage.csv')
netchange_percentage = read.csv(ofilename)
netchange_percentage = netchange_percentage$x
netchange_percentage[futurespidx]
netchange_percentage[currentspidx]

reachcountdiff = c()
curreachcount = c()
futreachcount = c()
for(i in 1:length(idx0))
{
  filename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_baseline.csv',spnamedata$name[idx0[i]],spnamedata$name[idx0[i]])
  filename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_futureres.csv',spnamedata$name[idx0[i]],spnamedata$name[idx0[i]])
  curpred = fread(filename1)
  futpred = fread(filename2)
  current_reach = curpred$comid[which(curpred$maxsss==1)]
  future_reach = futpred$comid[which(futpred$maxsss==1)]
  diff =  length(future_reach) - length(current_reach)
  reachcountdiff = c(reachcountdiff,diff)
  curreachcount = c(curreachcount,length(current_reach))
  futreachcount = c(futreachcount, length(future_reach))
  print(i)
}

fish = which(spnamedata$mussel_or_fish[idx0]==1)
mussel = which(spnamedata$mussel_or_fish[idx0]==0)
which((reachcountdiff[fish]/abs(reachcountdiff[fish])==b[fish]/abs(b[fish]))==TRUE)

netchange_percentage[futurespidx]/abs(netchange_percentage[futurespidx])

mean(netchange_percentage[fish])
mean(reachcountdiff[fish])
mean(b[fish])
mean(netchange_percentage[mussel])
# for a species that has less watersheds in the future, does it also have less reach count in the future?
# these plots may answer.

plot(reachcountdiff[fish],b[fish],
     xlab=sprintf('change in the number of reaches occupied in the future (mean= %.2f)',mean(reachcountdiff[fish])),
     ylab=sprintf('change in number of occupied huc 8 units in the future (mean= %.2f)',mean(b[fish]))) # this may answer.
lines(lowess(reachcountdiff[fish],b[fish]), col=2)

plot(reachcountdiff[mussel],b[mussel],
     xlab=sprintf('change in the number of reaches occupied in the future (mean= %.2f)',mean(reachcountdiff[mussel])),
     ylab=sprintf('change in number of occupied huc 8 units in the future (mean= %.2f)',mean(b[mussel]))) # this may answer.
lines(lowess(reachcountdiff[mussel],b[mussel]), col=2)


plot(curreachcount[fish],reachcountdiff[fish])
# fish species with lower reach count in the future have occupy lower number of hydrological units
# in the future, and as species reach count change in the future increases, 
# change in number of hydrological units occupied increases as well. 
# However, as the change in number of units go above 0, its marginal increase decreases.
# This is probably why the average change in number of units is negative, but
# the average change in species reach count is positive. 


# are species with positive reach count change more local species with fewer hydrological
# units?
sphuc6 = list()
for( i in 1:length(idx0))
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[idx0[i]])
  proj = fread(filename)
  sphuc6[[i]] = unique(floor(proj$huc12/10^6))
  print(i)
}

sphuc6len = sapply(sphuc6,length)
plot(sphuc6len[fish],reachcountdiff[fish],xlab='number of huc6 for the study area (proxy for study area size',
     ylab='change in the number of reaches occupied in the future')
abline(h=0,col=1)
lines(lowess(sphuc6len[fish],reachcountdiff[fish]),col=2)
plot(sphuc6len[mussel],reachcountdiff[mussel],xlab='number of huc6 in the study area')

plot(log(reachcountdiff[fish]+abs(min(reachcountdiff[fish]))+1),sphuc6len[fish])
lr = lm(log(reachcountdiff[fish]+abs(min(reachcountdiff[fish]))+1)~sphuc6len[fish])
plot(lr)
hist(b)
hist(reachcountdiff)

summary(lr)
# broadness of study area tend to have no effect on reach count difference. (p=0.000122)
