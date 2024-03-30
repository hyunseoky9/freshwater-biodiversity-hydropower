# evaluation metric average (auc,cbi,or10,ormtp) across species
# AUC, CBI, OR10

# average across species and then across gcm.
rm(list=ls())
library(rmaxent)
library(ENMeval)
library(ecospat)
library(devtools)
library(dplyr)
library(sf)
library(sp)
library(data.table)
library(tidyverse)
library(usdm)
set.seed(458)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
gcmver = c('DOE-ACCESS-CM2','DOE-CNRM-ESM2-1','DOE-BCC-CSM2-MR')

spavg = c()
bysp = list()
for(i in idx0)
{
  gcmavg = c(0,0,0,0)
  gcms = c()
  for(gcm in gcmver)
  {
    filename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/evalresult_selected_model.csv',gcm,spnamedata$name[i])
    d = read.csv(filename) 
    gcmavg = gcmavg + c(d$auc.train,d$cbi.train,d$or.10p.avg,d$or.mtp.avg)
    gcms = cbind(gcms,c(d$auc.train,d$cbi.train,d$or.10p.avg,d$or.mtp.avg))
  }
  gcmavg = gcmavg/length(gcmver)
  spavg = rbind(spavg,gcmavg)
  bysp[[which(idx0 %in% i)]] = gcms
  print(sprintf('%d/%d',which(idx0==i),length(idx0)))
}
auc = sapply(bysp,function(x) x[1,])
cbi = sapply(bysp,function(x) x[2,])
boxplot(auc[1,],auc[2,],auc[3,],names=gcmver,ylab='AUC')
boxplot(cbi[1,],cbi[2,],cbi[3,],names=gcmver,ylab='CBI')
species_average = apply(spavg,2,mean)
species_sd = apply(spavg,2,sd)
o = data.frame(rbind(species_average,species_sd))
names(o) = c('auc.train','cbi.train','OR.10','OR.mtp')
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/avg_evaluation_metric.csv'
write.csv(o,ofilename,row.names=FALSE)
