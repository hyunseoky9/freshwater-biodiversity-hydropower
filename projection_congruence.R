# calculate congruence between species distribution results between the 3 GCMs.
rm(list=ls())
library(data.table)

filename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spdata = read.csv(filename)

idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

gcmvers = c('DOE-CNRM-ESM2-1','DOE-BCC-CSM2-MR','DOE-ACCESS-CM2')

congruence <- function(x,y)
{
  sum(x*y)/sqrt(sum(x^2)*sum(y^2))
}

cr_congruence = c()
fr_congruence = c()
cp_congruence = c()
fp_congruence = c()
for( i in idx0)
{
  spname = spdata$name[i]
  #cr=current res, fr=future res, cp=current pri, fp=future pri
  cr_pred = list()
  fr_pred = list()
  cp_pred = list()
  fp_pred = list()
  
  j = 1
  for(gcmver in gcmvers)
  {
    filename_cr = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_binary_predictions_baseline.csv',gcmver,spname,spname)
    filename_fr = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_binary_predictions_futureres.csv',gcmver,spname,spname)
    filename_cp = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_binary_predictions_currentpri.csv',gcmver,spname,spname)
    filename_fp = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/%s_binary_predictions_futurepri.csv',gcmver,spname,spname)
    d_cr = fread(filename_cr)
    d_fr = fread(filename_fr)
    d_cp = fread(filename_cp)
    d_fp = fread(filename_fp)
    cr_pred[[j]] = d_cr$maxsss
    fr_pred[[j]] = d_fr$maxsss
    cp_pred[[j]] = d_cp$maxsss
    fp_pred[[j]] = d_fp$maxsss
    j = j + 1
  }
  
  cradd = c(congruence(cr_pred[[1]],cr_pred[[2]]),congruence(cr_pred[[1]],cr_pred[[3]]),congruence(cr_pred[[2]],cr_pred[[3]]))
  fradd = c(congruence(fr_pred[[1]],fr_pred[[2]]),congruence(fr_pred[[1]],fr_pred[[3]]),congruence(fr_pred[[2]],fr_pred[[3]]))
  cpadd = c(congruence(cp_pred[[1]],cp_pred[[2]]),congruence(cp_pred[[1]],cp_pred[[3]]),congruence(cp_pred[[2]],cp_pred[[3]]))
  fpadd = c(congruence(fp_pred[[1]],fp_pred[[2]]),congruence(fp_pred[[1]],fp_pred[[3]]),congruence(fp_pred[[2]],fp_pred[[3]]))
  if(all(is.nan(cradd))){cradd = rep(1,length(cradd))}
  if(all(is.nan(fradd))){fradd = rep(1,length(fradd))}
  if(all(is.nan(cpadd))){cpadd = rep(1,length(cpadd))}
  if(all(is.nan(fpadd))){fpadd = rep(1,length(fpadd))}
  cr_congruence = rbind(cr_congruence,cradd)
  fr_congruence = rbind(fr_congruence,fradd)
  cp_congruence = rbind(cp_congruence,cpadd)
  fp_congruence = rbind(fp_congruence,fpadd)
  
  print(sprintf('%d/%d',which(idx0==i),length(idx0)))
}

cr_congruence_mean = apply(cr_congruence,2,mean)
fr_congruence_mean = apply(fr_congruence,2,mean)
cp_congruence_mean = apply(cp_congruence,2,mean)
fp_congruence_mean = apply(fp_congruence,2,mean)

cr_congruence_se = apply(cr_congruence,2,function(x) sd(x)/sqrt(length(x)))
fr_congruence_se = apply(fr_congruence,2,function(x) sd(x)/sqrt(length(x)))
cp_congruence_se = apply(cp_congruence,2,function(x) sd(x)/sqrt(length(x)))
fp_congruence_se = apply(fp_congruence,2,function(x) sd(x)/sqrt(length(x)))



cr_congruence_text = paste(sprintf('%.2f',cr_congruence_mean),
                           sprintf('(%.2f)',cr_congruence_se))
fr_congruence_text = paste(sprintf('%.2f',fr_congruence_mean),
                           sprintf('(%.2f)',cr_congruence_se))
cp_congruence_text = paste(sprintf('%.2f',cp_congruence_mean),
                           sprintf('(%.2f)',cr_congruence_se))
fp_congruence_text = paste(sprintf('%.2f',fp_congruence_mean),
                           sprintf('(%.2f)',cr_congruence_se))


o = as.data.frame(rbind(cr_congruence_text, fr_congruence_text, cp_congruence_text, fp_congruence_text))

names(o) = c(sprintf('%s:%s',gcmvers[1],gcmvers[2]),
             sprintf('%s:%s',gcmvers[1],gcmvers[3]),
             sprintf('%s:%s',gcmvers[2],gcmvers[3]))

ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/projected_distribution_congruence_between_gcms.csv'
write.csv(o,ofilename,row.names=FALSE)
head(o)
max(o)
min(o)
