# difference in occurrence probability between current and future, and pristine and reservoir scenario.
rm(list=ls())
library(data.table)


#1. mean difference in ocurrence probability between pristine and reservoir scenario in tailwater
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

#gcmver = 'DOE-BCC-CSM2-MR'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
#scenario = 'pristine w gcm' #'pristine_gcm_reservoir'

pval = c()
meandiff = c()
tailwaterratio = c()
for(i in idx0[1:length(idx0)])
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
  tailcomids = proj$comid[which(!is.na(proj$udd2hilarri_umdamcomid))]
  tailwaterratio = c(tailwaterratio, length(tailcomids)/nrow(proj))
  if(length(tailcomids)>0)
  {
    tailresprob = currentres$probability[which(currentres$comid %in% tailcomids)]
    tailpriprob = currentpri$probability[which(currentpri$comid %in% tailcomids)]
    #boxplot(cbind(tailresprob,tailpriprob))
    
    result <- t.test(tailresprob, tailpriprob, paired = TRUE)
    pval = c(pval,result$p.value)
    meandiff = c(meandiff,result$estimate)
  } else {
    pval = c(pval,NA)
    meandiff = c(meandiff,NA)
  }
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}
length(meandiff)
sort(meandiff,decreasing=TRUE)[1:100]
hist(meandiff,main='histogram of mean difference in ocurrence probability between pristine and reservoir scenario in tailwater')

pri_res_probmeandiff = data.frame(spname=spnamedata$name[idx0],meandiff=meandiff,paired_ttest_pval=pval)
# *run from this line to get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/rescur-pricur_probmeandiff_tailwater.csv'
#write.csv(pri_res_probmeandiff,ofilename,row.names=FALSE)
d = read.csv(ofilename)
dim(d)
hist(na.omit(d$meandiff),main=sprintf('histogram of mean difference in ocurrence probability
(current res - current pri) in tailwater
mean=%.3f, sd=%.2f, sd=%.4f',mean(na.omit(d$meandiff)),sd(na.omit(d$meandiff)),
                             sd(na.omit(d$meandiff))/sqrt(length(na.omit(d$meandiff)))))
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

fishd = d[which(fishidx==1),]
musseld = d[which(fishidx==0),]
mean(na.omit(fishd$meandiff))
sd(na.omit(fishd$meandiff))
sd(na.omit(fishd$meandiff))/sqrt(length(na.omit(fishd$meandiff)))
mean(abs(na.omit(musseld$meandiff)))
sd(abs(na.omit(musseld$meandiff)))
sd(na.omit(musseld$meandiff))/sqrt(length(na.omit(musseld$meandiff)))
boxplot(na.omit(fishd$meandiff),na.omit(musseld$meandiff))

# 2. mean difference in occurrence probability between future and current scenario. 
rm(list=ls())
library(data.table)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

#gcmver = 'DOE-BCC-CSM2-MR'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
#scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
pval = c()
meandiff = c()
tailwaterratio = c()
for(i in idx0[1:length(idx0)])
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
  
  result <- t.test(futureres$probability,currentres$probability, paired = TRUE)
  pval = c(pval,result$p.value)
  meandiff = c(meandiff,result$estimate)
  print(result$estimate)
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}

#length(meandiff)
#sort(meandiff,decreasing=TRUE)[1:100]

curr_future_probmeandiff = data.frame(spname=spnamedata$name[idx0],meandiff=meandiff,paired_ttest_pval=pval)
# *run from this line to get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/futureres-currentres_probmeandiff.csv'
#write.csv(curr_future_probmeandiff,ofilename,row.names=FALSE)
d = read.csv(ofilename)
dim(d)
hist(d$meandiff,main=sprintf('histogram of mean difference in ocurrence probability 
     (futureres - currentres)
     mean=%.3f, sd=%.2f, se=%.4f',mean(d$meandiff),sd(d$meandiff),
                             sd(d$meandiff)/sqrt(length(d$meandiff))))
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

fishd = d[which(fishidx==1),]
musseld = d[which(fishidx==0),]
mean(abs(na.omit(fishd$meandiff)))
sd(abs(na.omit(fishd$meandiff)))
sd(na.omit(fishd$meandiff))/sqrt(length(na.omit(fishd$meandiff)))
mean(abs(na.omit(musseld$meandiff)))
sd(abs(na.omit(musseld$meandiff)))
sd(na.omit(musseld$meandiff))/sqrt(length(na.omit(musseld$meandiff)))
boxplot(na.omit(fishd$meandiff),na.omit(musseld$meandiff))

# 2.2 mean difference in occurrence probability between future and current scenario 
# under pristine scenario. 
rm(list=ls())
library(data.table)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

#gcmver = 'DOE-BCC-CSM2-MR'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
#scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
pval = c()
meandiff = c()
tailwaterratio = c()
for(i in idx0[1:length(idx0)])
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
  
  result <- t.test(futurepri$probability,currentpri$probability, paired = TRUE)
  pval = c(pval,result$p.value)
  meandiff = c(meandiff,result$estimate)
  print(result$estimate)
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}

#length(meandiff)
#sort(meandiff,decreasing=TRUE)[1:100]

curr_future_probmeandiff = data.frame(spname=spnamedata$name[idx0],meandiff=meandiff,paired_ttest_pval=pval)
# *run from this line to get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/futurepri-currentpri_probmeandiff.csv'
write.csv(curr_future_probmeandiff,ofilename,row.names=FALSE)




# 3. mean difference in ocurrence probability between pristine and reservoir scenario
rm(list=ls())
library(data.table)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

gcmver = 'DOE-BCC-CSM2-MR'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
pval = c()
meandiff = c()
tailwaterratio = c()
for(i in idx0[1:length(idx0)])
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
  
  result <- t.test(currentres$probability, currentpri$probability, paired = TRUE)
  pval = c(pval,result$p.value)
  meandiff = c(meandiff,result$estimate)
  print(result$estimate)
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}

#length(meandiff)
#sort(meandiff,decreasing=TRUE)[1:100]
hist(meandiff,main='histogram of mean difference in ocurrence probability between pristine and reservoir scenario under current timeline')
pri_res_probmeandiff = data.frame(spname=spnamedata$name[idx0],meandiff=meandiff,paired_ttest_pval=pval)
# *run from this line to get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/pri_res_probmeandiff.csv'
#write.csv(pri_res_probmeandiff,ofilename,row.names=FALSE)
d = read.csv(ofilename)
dim(d)
hist(abs(d$meandiff),main=sprintf('histogram of mean difference in ocurrence probability
                             between pristine and reservoir scenario under current timeline
                             mean=%.3f, sd=%.2f',mean(abs(d$meandiff)),sd(abs(d$meandiff))))
