#  predictors with most contribution acoss species
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
nolownetsymdiff = 1
threshold = 0.4

idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index



currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
gcmver = 'DOE-CNRM-ESM2-1'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
doerrored_ones =0 # if 1, only run for species that errored previously. Which ones errored are marked in the errorspidx.csv
if(doerrored_ones)
{
  idx0 = read.csv(sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/errorspidx.csv',gcmver))
  idx0 = idx0$x
}

start <- proc.time()
first = c()
second  = c()
third = c()
for(i in idx0[1:length(idx0)])
{
  filename = sprintf("G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/predictor_contribution.csv",gcmver,spnamedata$name[i])
  d  = fread(filename)
  p =  names(d)[order(as.matrix(d),decreasing=TRUE)]
  first = c(first,p[1])
  second = c(second,p[2])
  third = c(third,p[3])
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}
par(mfrow=c(3,1))
barplot(sort(table(first))[1:5],cex.names=2,main=sprintf('top 5 predictors that come up as the highest contributor to the model. gcm: %s',gcmver))
barplot(sort(table(second))[1:5],cex.names=2,main=sprintf('top 5 predictors that come up as the 2nd highest contributor to the model. gcm: %s',gcmver))
barplot(sort(table(third))[1:5],cex.names=2,main=sprintf('top 5 predictors that come up as the highest contributor to the model. gcm: %s',gcmver))
