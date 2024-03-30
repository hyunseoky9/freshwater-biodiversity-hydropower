# net change in range length 
library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)
library(data.table)
rm(list=ls())
# 0. add reach length in projarea 
{
# 0.1. by_huc
looplength = 18
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area
for(i in 2:looplength)
{
  if(occ_or_proj)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
    data = read.csv(filename)
  } else {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
    data = read.csv(filename)
  }
  comid = data$comid[which(!is.na(data$comid))]
  numloops = ceiling(length(comid)/100)
  reachlength = c()
  comids = c()
  for( j in 1:numloops)
  {
    start = (j-1)*100+1
    if(j!=numloops)
    {
      end = j*100  
    } else {
      end = length(comid)
    }
    subset_file <- tempfile(fileext = ".gpkg")
    subset <- subset_nhdplus(comids = comid[start:end],
                             output_file = subset_file,
                             nhdplus_data = "download", 
                             flowline_only = TRUE,
                             return_data = TRUE, overwrite = TRUE)
    if(length(comid[start:end])!= length(subset$NHDFlowline_Network$lengthkm))
    {
      idx = match(comid[start:end],subset$NHDFlowline_Network$comid)
      rl = subset$NHDFlowline_Network$lengthkm[idx]
    } else {
      rl = subset$NHDFlowline_Network$lengthkm
    }
    comids = c(comids, comid[start:end])
    reachlength = c(reachlength,rl)
    print(sprintf('loop %d/%d',j,numloops))
  }
  print('subset no error')
  data$reachlen = reachlength #stream order
  #write.csv(data,filename,row.names=FALSE)
  print(sprintf('%d done' , i))
}


# 0.1. by_sp

rm(list=ls())
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')

for(i in 1:nrow(spdata))
{
  path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  spproj = read.csv(path1)
  hucs = unique(spproj$huc2)
  spproj$reachlen = NA
  for(j in 1:length(hucs))
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',hucs[j])
    hucd = read.csv(filename)
    idx = match(spproj$comid[which(spproj$huc2==hucs[j])], hucd$comid)
    # get rest of the predictors from by_huc data.
    spproj$reachlen[which(spproj$huc2==hucs[j])] = hucd$reachlen[idx]
  }
  # write out the data in a separate file
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spdata$name[i])
  write.csv(spproj,ofilename,row.names=FALSE)
  print(sprintf('%d done',i))
}
}

# 1.get net change in range length
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

netchange_percentage = c() # for both fish and mussel
netchange_percentage_fish = c()
netchange_percentage_mussel = c()
errorlengthdiff= c()
for( i in 1:length(idx0))
{
  filename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_baseline.csv',spnamedata$name[idx0[i]],spnamedata$name[idx0[i]])
  filename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_avg_binary_predictions_futureres.csv',spnamedata$name[idx0[i]],spnamedata$name[idx0[i]])
  curpred = fread(filename1)
  futpred = fread(filename2)
  
  
  # current reaches
  current_reach = curpred$comid[which(curpred$maxsss==1)]
  # newly occurring reaches
  addition = futpred$comid[which(curpred$maxsss==0 & futpred$maxsss==1)] # new range
  # lost reaches
  lost = curpred$comid[which(curpred$maxsss==1 & futpred$maxsss==0)] # lost range

  # sum the reaches' lengths
  filename3 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors/%s_projarea_non-tempflowpredictors.csv',spnamedata$name[idx0[i]])
  predictord = fread(filename3)
  currentlength = sum(predictord$reachlen[match(current_reach,predictord$comid)])
  additionlength = predictord$reachlen[match(addition,predictord$comid)]
  lostlength = predictord$reachlen[match(lost,predictord$comid)]
  netchange = sum(additionlength) - sum(lostlength)
  
  newval = netchange/currentlength
  
  # 2nd way of measuring net change length
  fut = futpred$comid[which(futpred$maxsss==1)]
  cur = curpred$comid[which(curpred$maxsss==1)]
  currentlength = sum(predictord$reachlen[match(current_reach,predictord$comid)])
  futurelength = sum(predictord$reachlen[match(fut,predictord$comid)])
  netchange_new = futurelength - currentlength
  
  if(netchange-netchange_new > 10^-8)
  {
    print(sprintf('length diff at %d',i))
    errorlengthdiff= c(errorlengthdiff,i)
  }
  if(is.na(newval))
  {
    print('newval is NA!')
  }
  netchange_percentage = c(netchange_percentage,newval)
  fish = spnamedata$mussel_or_fish[idx0[i]]
  if(fish)
  {
    netchange_percentage_fish = c(netchange_percentage_fish,newval)
  } else {
    netchange_percentage_mussel = c(netchange_percentage_mussel,newval)
  }  
  print(i)
}
# *run from this line to just get the result
ofilename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/analysis/netdistributionchange_percentage.csv')
#write.csv(netchange_percentage,ofilename,row.names=FALSE)

netchange_percentage = read.csv(ofilename)
netchange_percentage = netchange_percentage$x*100
mean(netchange_percentage)
sd(netchange_percentage)
sd(netchange_percentage)/sqrt(length(netchange_percentage))

# for fish
fish = which(spnamedata$mussel_or_fish[idx0]==1)
mean(netchange_percentage[fish])
sd(netchange_percentage[fish])
sd(netchange_percentage[fish])/sqrt(length(netchange_percentage[fish]))

mussel = which(spnamedata$mussel_or_fish[idx0]==0)
mean(netchange_percentage[mussel])
sd(netchange_percentage[mussel])
sd(netchange_percentage[mussel])/sqrt(length(netchange_percentage[mussel]))

hist(netchange_percentage,main='net change in distribution size')

# for non-indigenous
nonindigenous = which(spnamedata$nonindigenous[idx0]==1)
mean(netchange_percentage[nonindigenous])
sd(netchange_percentage[nonindigenous])
sd(netchange_percentage[nonindigenous])/sqrt(length(netchange_percentage[nonindigenous]))
native = which(spnamedata$nonindigenous[idx0]==0)
mean(netchange_percentage[native])
sd(netchange_percentage[native])
sd(netchange_percentage[native])/sqrt(length(netchange_percentage[native]))

