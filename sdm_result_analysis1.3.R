# plot Effect of hydropower on the effect of climate change in the future. 
# get difference in occurrence probability between current res and future res in tailwater
# + get difference in occurrence probability between current pri and future pri in tailwater
# Then, get the difference between the two differences.
# 1. get difference in occurrence probability between current res and future res in tailwater
rm(list=ls())

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

#gcmver = 'DOE-BCC-CSM2-MR'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
#scenario = 'pristine w gcm' #'pristine_gcm_reservoir'

pvalres = c()
pvalpri = c()
meandiffres = c()
meandiffpri = c()
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
    tailcurresprob = currentres$probability[which(currentres$comid %in% tailcomids)]
    tailfutresprob = futureres$probability[which(futureres$comid %in% tailcomids)]
    #boxplot(cbind(tailresprob,tailpriprob))
    resultres <- t.test(tailfutresprob, tailcurresprob, paired = TRUE)
    
    tailcurpriprob = currentpri$probability[which(currentpri$comid %in% tailcomids)]
    tailfutpriprob = futurepri$probability[which(futurepri$comid %in% tailcomids)]
    resultpri <- t.test(tailfutpriprob, tailcurpriprob, paired = TRUE)
    
    pvalres = c(pvalres,resultres$p.value)
    pvalpri = c(pvalpri,resultpri$p.value)
    
    meandiffres = c(meandiffres,resultres$estimate)
    meandiffpri = c(meandiffpri,resultpri$estimate)
  } else {
    pvalres = c(pvalres,NA)
    pvalpri = c(pvalpri,NA)
    meandiffres = c(meandiffres,NA)
    meandiffpri = c(meandiffpri,NA)
  }
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}

fut_cur_probmeandiff = data.frame(spname=spnamedata$name[idx0],
                                  meandiff_futureresMcurrentres=meandiffres,
                                  meandiff_futurepriMcurrentpri=meandiffpri,
                                  paired_ttest_pval_res=pvalres,
                                  paired_ttest_pval_res=pvalpri)
# *run from this line to get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/fut-cur_probmeandiff_tailwater.csv'
#write.csv(fut_cur_probmeandiff,ofilename,row.names=FALSE)


d = read.csv(ofilename)
dim(d)
hist(na.omit(d$meandiff_futureresMcurrentres),main=sprintf('histogram of mean difference in ocurrence probability
(current res - current pri) in tailwater
mean=%.3f, sd=%.2f, sd=%.4f',mean(na.omit(d$meandiff_futureresMcurrentres)),sd(na.omit(d$meandiff_futureresMcurrentres)),
                             sd(na.omit(d$meandiff_futureresMcurrentres))/sqrt(length(na.omit(d$meandiff_futureresMcurrentres)))))
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

fishd = d[which(fishidx==1),]
musseld = d[which(fishidx==0),]

# future res - current res
mean(na.omit(fishd$meandiff_futureresMcurrentres))
hist(na.omit(fishd$meandiff_futureresMcurrentres))
sd(na.omit(fishd$meandiff_futureresMcurrentres))
sd(na.omit(fishd$meandiff_futureresMcurrentres))/sqrt(length(na.omit(fishd$meandiff_futureresMcurrentres)))
t.test(na.omit(fishd$meandiff_futureresMcurrentres))

mean(na.omit(musseld$meandiff_futureresMcurrentres))
sd(na.omit(musseld$meandiff_futureresMcurrentres))
sd(na.omit(musseld$meandiff_futureresMcurrentres))/sqrt(length(na.omit(musseld$meandiff_futureresMcurrentres)))
boxplot(na.omit(fishd$meandiff_futureresMcurrentres),na.omit(musseld$meandiff_futureresMcurrentres))
t.test(na.omit(musseld$meandiff_futureresMcurrentres))

# future pri - current pri
mean(na.omit(fishd$meandiff_futurepriMcurrentpri))
hist(na.omit(fishd$meandiff_futurepriMcurrentpri))
sd(na.omit(fishd$meandiff_futurepriMcurrentpri))
sd(na.omit(fishd$meandiff_futurepriMcurrentpri))/sqrt(length(na.omit(fishd$meandiff_futurepriMcurrentpri)))
t.test(na.omit(fishd$meandiff_futurepriMcurrentpri))

mean(na.omit(musseld$meandiff_futurepriMcurrentpri))
sd(na.omit(musseld$meandiff_futurepriMcurrentpri))
sd(na.omit(musseld$meandiff_futurepriMcurrentpri))/sqrt(length(na.omit(musseld$meandiff_futurepriMcurrentpri)))
boxplot(na.omit(fishd$meandiff_futurepriMcurrentpri),na.omit(musseld$meandiff_futurepriMcurrentpri))
t.test(na.omit(musseld$meandiff_futurepriMcurrentpri))

# (future res - current res) - (future pri - current pri)
fish_diffdiff = na.omit(fishd$meandiff_futureresMcurrentres) - na.omit(fishd$meandiff_futurepriMcurrentpri)
hist(fish_diffdiff)
mean(fish_diffdiff)
sd(fish_diffdiff)/sqrt(length(fish_diffdiff))
t.test(fish_diffdiff)

mussel_diffdiff = na.omit(musseld$meandiff_futureresMcurrentres) - na.omit(musseld$meandiff_futurepriMcurrentpri)
mean(mussel_diffdiff)
hist(mussel_diffdiff)
sd(mussel_diffdiff)/sqrt(length(mussel_diffdiff))
t.test(mussel_diffdiff)


# plotting (future res - current res) - (future pri - current pri) with a dynamite plot
# Turn summary into a data frame that ggplot2 can work with
summary_df <- data.frame(
  Group = c('Fish','Mussels'),
  Mean = c(mean(fish_diffdiff),mean(mussel_diffdiff)),
  SE = c(sd(fish_diffdiff)/sqrt(length(fish_diffdiff)),sd(mussel_diffdiff)/sqrt(length(mussel_diffdiff)))
)

# Dynamite plot
ggplot(summary_df, aes(x = Group, y = Mean)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.25, position = position_dodge(.9)) +
  theme_minimal() +
  theme(text=element_text(size=13))+
  labs(x="",y = "Effect of climate change accounting for the reservoir effect - 
  Effect of climate change not accounting for the reservoir effect")
