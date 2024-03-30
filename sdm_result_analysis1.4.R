rm(list=ls())
library(data.table)
library(ggplot2)
library(data.table)
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)


#1. mean difference in reach length of species distribution
# between pristine and reservoir scenario only in tailwater (reservoir effect)
# and between current and future pristine scenario in 
# all reaches within study area (climate effect)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

#gcmver = 'DOE-BCC-CSM2-MR'   #'DOE-ACCESS-CM2' # DOE-CNRM-ESM2-1 #'DOE-BCC-CSM2-MR' 
#scenario = 'pristine w gcm' #'pristine_gcm_reservoir'
reservoir_effect = c()
climate_effect = c()
tailwaterratio = c()
futcurres = c()
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
  tailtotlen = sum(proj$reachlen[which(!is.na(proj$udd2hilarri_umdamcomid))]) #tailwater total length
  if(length(tailcomids)>0)
  {
    # for current scenario
    tailresocccomid = currentres$comid[which(currentres$comid %in% tailcomids & currentres$maxsss==1)]
    tailpriocccomid = currentpri$comid[which(currentpri$comid %in% tailcomids & currentpri$maxsss==1)]
    # total length of occurring reaches in the tailwater in reservoir sc.
    tailresocclen = sum(proj$reachlen[match(tailresocccomid,proj$comid)])
    tailpriocclen = sum(proj$reachlen[match(tailpriocccomid,proj$comid)])
    
    # for future scenario (later used for interaction in #2.)
    futtailresocccomid = futureres$comid[which(futureres$comid %in% tailcomids & futureres$maxsss==1)]
    futtailpriocccomid = futurepri$comid[which(futurepri$comid %in% tailcomids & futurepri$maxsss==1)]
    # total length of occurring reaches in the tailwater in reservoir sc.
    futtailresocclen = sum(proj$reachlen[match(futtailresocccomid,proj$comid)])
    futtailpriocclen = sum(proj$reachlen[match(futtailpriocccomid,proj$comid)])
    
        
    reservoir_effect = rbind(reservoir_effect,
                             c(tailresocclen,tailpriocclen,
                               futtailresocclen,futtailpriocclen,
                               tailresocclen-tailpriocclen))
  } else {
    reservoir_effect = rbind(reservoir_effect, NA)
  }
  
  curocccomid = currentpri$comid[which(currentpri$maxsss==1)]
  futocccomid = futurepri$comid[which(futurepri$maxsss==1)]
  curocclen = sum(proj$reachlen[match(curocccomid,proj$comid)])
  futocclen = sum(proj$reachlen[match(futocccomid,proj$comid)])
  climate_effect = rbind(climate_effect,
                         c(futocclen,curocclen,futocclen-curocclen))
  curresocccomid = currentres$comid[which(currentres$maxsss==1)]
  futresocccomid = futureres$comid[which(futureres$maxsss==1)]  
  
  curresocclen = sum(proj$reachlen[match(curresocccomid,proj$comid)])
  futresocclen = sum(proj$reachlen[match(futresocccomid,proj$comid)])
  futcurres = rbind(futcurres,c(curresocclen,futresocclen))
  print(sprintf('%d/%d done',which(idx0==i),length(idx0)))
}

effects = as.data.frame(cbind(reservoir_effect,climate_effect,futcurres))
names(effects) = c('cr_occ_kmlen_intail',
                   'cp_occ_kmlen_intail',
                   'fr_occ_kmlen_intail',
                   'fp_occ_kmlen_intail',
                   'reservoir_effect',
                   'fp_occ_kmlen',
                   'cp_occ_kmlen',
                   'climate_effect',
                   'cr_occ_kmlen',
                   'fr_occ_kmlen')

ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/climateNreservoir_effects_by_kmreachlength.csv'
#write.csv(effects,ofilename,row.names=FALSE)
effects = read.csv(ofilename)
# percentage
effects$reservoir_effect_p = effects$reservoir_effect/abs(effects$cp_occ_kmlen_intail+1)*100
effects$climate_effect_p = effects$climate_effect/abs(effects$cp_occ_kmlen+1)*100
hist(effects$reservoir_effect_p)
hist(effects$climate_effect_p)
mean(na.omit(effects$reservoir_effect_p))
mean(effects$climate_effect_p)
t.test(effects$reservoir_effect_p)
t.test(effects$climate_effect_p)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

climate_effect_fish = na.omit(effects$climate_effect[which(fishidx==1)])
reservoir_effect_fish = na.omit(effects$reservoir_effect[which(fishidx==1)])
climate_effect_mussel = na.omit(effects$climate_effect[which(fishidx==0)])
reservoir_effect_mussel = na.omit(effects$reservoir_effect[which(fishidx==0)])

climate_effect_fish_p = na.omit(effects$climate_effect_p[which(fishidx==1)])
reservoir_effect_fish_p = na.omit(effects$reservoir_effect_p[which(fishidx==1)])
climate_effect_mussel_p = na.omit(effects$climate_effect_p[which(fishidx==0)])
reservoir_effect_mussel_p = na.omit(effects$reservoir_effect_p[which(fishidx==0)])
quantile(climate_effect_fish_p,na.rm = T,probs = c(0.93))
quantile(climate_effect_mussel_p,na.rm = T,probs = c(0.90))

log_diff=function(x)
{
  logged = log10(abs(x)+1)
  logged[which(x<0)] = logged[which(x<0)]*(-1)
  return(logged)
}
climate_effect_fish_log10 = log_diff(na.omit(effects$climate_effect[which(fishidx==1)]))
reservoir_effect_fish_log10 = log_diff(na.omit(effects$reservoir_effect[which(fishidx==1)]))
climate_effect_mussel_log10 = log_diff(na.omit(effects$climate_effect[which(fishidx==0)]))
reservoir_effect_mussel_log10 = log_diff(na.omit(effects$reservoir_effect[which(fishidx==0)]))

climate_effect_fish_logp = log_diff(na.omit(effects$climate_effect_p[which(fishidx==1)]))
reservoir_effect_fish_logp = log_diff(na.omit(effects$reservoir_effect_p[which(fishidx==1)]))
climate_effect_mussel_logp = log_diff(na.omit(effects$climate_effect_p[which(fishidx==0)]))
reservoir_effect_mussel_logp = log_diff(na.omit(effects$reservoir_effect_p[which(fishidx==0)]))


# mean & SE
mean(climate_effect_fish)
mean(reservoir_effect_fish)
mean(climate_effect_mussel)
mean(reservoir_effect_mussel)
sd(climate_effect_fish)/sqrt(length(climate_effect_fish))
sd(reservoir_effect_fish)/sqrt(length(reservoir_effect_fish))
sd(climate_effect_mussel)/sqrt(length(climate_effect_mussel))
sd(reservoir_effect_mussel)/sqrt(length(reservoir_effect_mussel))
# mean & SE for percentage change
mean(climate_effect_fish_p)
mean(reservoir_effect_fish_p)
mean(climate_effect_mussel_p)
mean(reservoir_effect_mussel_p)
sd(climate_effect_fish_p)/sqrt(length(climate_effect_fish_p))
sd(reservoir_effect_fish_p)/sqrt(length(reservoir_effect_fish_p))
sd(climate_effect_mussel_p)/sqrt(length(climate_effect_mussel_p))
sd(reservoir_effect_mussel_p)/sqrt(length(reservoir_effect_mussel_p))


# test the hypothesis for non-zero climate and reservoir effect (in manuscript)
t.test(climate_effect_fish)
t.test(climate_effect_mussel)
t.test(reservoir_effect_fish)
t.test(reservoir_effect_mussel)

t.test(climate_effect_fish_p)
t.test(climate_effect_mussel_p)
t.test(reservoir_effect_fish_p)
t.test(reservoir_effect_mussel_p)


par(mfrow=c(2,2))
hist(climate_effect_fish)
hist(reservoir_effect_fish)
hist(climate_effect_mussel)
hist(reservoir_effect_mussel)
boxplot(climate_effect_fish)
boxplot(reservoir_effect_fish)
boxplot(climate_effect_mussel)
boxplot(reservoir_effect_mussel)


# make a separate object for plotting halfeye plot
#log10 version
combined_data1 <- rbind(
  data.frame(effect = 'reservoir effect on\n fish species\n(in tailwaters)', meandiff = reservoir_effect_fish_log10),
  data.frame(effect = 'climate effect on\n fish species', meandiff = climate_effect_fish_log10),
  data.frame(effect = 'reservoir effect on\n mussel species\n(in tailwaters)', meandiff = reservoir_effect_mussel_log10),
  data.frame(effect = 'climate effect on\n mussel species', meandiff = climate_effect_mussel_log10)
)

# percentage
combined_data2 <- rbind(
  data.frame(effect = 'reservoir effect on\n fish species\n(in tailwaters)', meandiff = reservoir_effect_fish_p),
  data.frame(effect = 'climate effect on\n fish species', meandiff = climate_effect_fish_p),
  data.frame(effect = 'reservoir effect on\n mussel species\n(in tailwaters)', meandiff = reservoir_effect_mussel_p),
  data.frame(effect = 'climate effect on\n mussel species', meandiff = climate_effect_mussel_p)
)

# log10 percentage
combined_data3 <- rbind(
  data.frame(effect = 'reservoir effect on\n fish species\n(in tailwaters)', meandiff = reservoir_effect_fish_logp),
  data.frame(effect = 'climate effect on\n fish species', meandiff = climate_effect_fish_logp),
  data.frame(effect = 'reservoir effect on\n mussel species\n(in tailwaters)', meandiff = reservoir_effect_mussel_logp),
  data.frame(effect = 'climate effect on\n mussel species', meandiff = climate_effect_mussel_logp)
)


# raw
combined_data4 <- rbind(
  data.frame(effect = 'reservoir effect on\n fish species\n(in tailwaters)', meandiff = reservoir_effect_fish),
  data.frame(effect = 'climate effect on\n fish species', meandiff = climate_effect_fish),
  data.frame(effect = 'reservoir effect on\n mussel species\n(in tailwaters)', meandiff = reservoir_effect_mussel),
  data.frame(effect = 'climate effect on\n mussel species', meandiff = climate_effect_mussel)
)


combined_data_chosen = combined_data2
combined_data_chosen$effect <- factor(combined_data_chosen$effect, levels = c(
  'climate effect on\n fish species',
  'reservoir effect on\n fish species\n(in tailwaters)',
  'climate effect on\n mussel species',
  'reservoir effect on\n mussel species\n(in tailwaters)'
))
# halfeye plot

'log(mean difference in distribution length) (km)'
ylab_text = expression('sign('*Delta*'range)log'[10]*'('*Delta*'range)')
ggplot(combined_data_chosen, aes(x = effect, y = meandiff)) + 
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) + 
  geom_boxplot(
    width = 0.25,
    # removing outliers
    alpha = 0.5,
    outlier.color = NA
  ) +   
  ylim(-100,250)+
  ylab('Percent change in range') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),) +
  theme(axis.text.x = element_text(size=14.5),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17))

# half-eye with logscale
ylab_text = expression('sign('*Delta*'range)log'[10]*'('*Delta*'range)')
ggplot(combined_data, aes(x = effect, y = meandiff)) + 
  geom_boxplot(
    width = 0.12,
  ) +   
  ylab('') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),) +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17))



# 2. looking at the interaction between the climate and the reservoir effect:
# difference between future_pristine-current_preistine and future_reservoir-current_reservoir in tail water
# (reach length)
filename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/climateNreservoir_effects_by_kmreachlength.csv'
effects = read.csv(filename)
climate_effect_reservoir = effects$fr_occ_kmlen_intail - effects$cr_occ_kmlen_intail
climate_effect_pristine = effects$fp_occ_kmlen_intail - effects$cp_occ_kmlen_intail
climate_effect_reservoir_p = (effects$fr_occ_kmlen_intail - effects$cr_occ_kmlen_intail)/abs(effects$cr_occ_kmlen_intail)*100
climate_effect_reservoir_p[which(climate_effect_reservoir_p==Inf)] = NA
climate_effect_pristine_p = (effects$fp_occ_kmlen_intail - effects$cp_occ_kmlen_intail)/abs(effects$cp_occ_kmlen_intail)*100
climate_effect_pristine_p[which(climate_effect_pristine_p==Inf)] = NA
par(mfrow=c(1,2))
hist(climate_effect_reservoir)
hist(climate_effect_pristine)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

climate_effect_reservoir_fish = na.omit(climate_effect_reservoir[which(fishidx==1)])
climate_effect_pristine_fish = na.omit(climate_effect_pristine[which(fishidx==1)])
climate_effect_reservoir_mussel = na.omit(climate_effect_reservoir[which(fishidx==0)])
climate_effect_pristine_mussel = na.omit(climate_effect_pristine[which(fishidx==0)])
climate_effect_reservoir_p_fish = na.omit(climate_effect_reservoir_p[which(fishidx==1)])
climate_effect_pristine_p_fish = na.omit(climate_effect_pristine_p[which(fishidx==1)])
climate_effect_reservoir_p_mussel = na.omit(climate_effect_reservoir_p[which(fishidx==0)])
climate_effect_pristine_p_mussel = na.omit(climate_effect_pristine_p[which(fishidx==0)])


par(mfrow=c(1,2))
boxplot(climate_effect_reservoir_fish,climate_effect_pristine_fish)
boxplot(climate_effect_reservoir_mussel,climate_effect_pristine_mussel)
interaction_fish <- t.test(climate_effect_reservoir_fish, climate_effect_pristine_fish, paired = TRUE)
interaction_mussel <- t.test(climate_effect_reservoir_mussel,climate_effect_pristine_mussel, paired = TRUE)
interaction_fish_p <- t.test(climate_effect_reservoir_p_fish, climate_effect_pristine_p_fish, paired = TRUE)
interaction_mussel_p <- t.test(climate_effect_reservoir_p_mussel,climate_effect_pristine_p_mussel, paired = TRUE)
diff_fish = climate_effect_reservoir_fish-climate_effect_pristine_fish
diff_mussel = climate_effect_reservoir_mussel-climate_effect_pristine_mussel
sd(diff_fish)/sqrt(length(diff_fish))
sd(diff_mussel)/sqrt(length(diff_mussel))
mean(climate_effect_reservoir_fish)
mean(climate_effect_pristine_fish)
mean(climate_effect_reservoir_mussel)
mean(climate_effect_pristine_mussel)

median(climate_effect_reservoir_p_fish)
median(climate_effect_pristine_p_fish)
median(climate_effect_reservoir_p_mussel)
median(climate_effect_pristine_p_mussel)




