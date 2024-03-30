# makes the boxplot with sdm_result analysis1.R for mussel and fish separately
rm(list=ls())
library(ggplot2)
library(data.table)
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

#1. mean difference in ocurrence probability between pristine and reservoir scenario in tailwater
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

# *run from this line to get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/rescur-pricur_probmeandiff_tailwater.csv'
#write.csv(pri_res_probmeandiff,ofilename,row.names=FALSE)
d = read.csv(ofilename)
dim(d)
hist(d$meandiff,main=sprintf('histogram of mean difference in ocurrence probability
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
fishd0 = fishd
musseld0 = musseld

#2. mean difference in occurrence probability between future and current scenario. 
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

par(mar=c(5.1, 5, 4.1, 2.1))
boxplot(na.omit(fishd0$meandiff),na.omit(musseld0$meandiff),
        na.omit(fishd$meandiff),na.omit(musseld$meandiff)
        ,names=c('fish; reservoir','mussel; reservoir','fish; climate','mussel; climate')
        ,ylab='mean difference in probability of occurrence \n due to the effect'
        ,main='effect of reservoir and climate change on
        occurrence probability of species')

combined_data <- rbind(
  data.frame(effect = 'reservoir effect on\n fish species\n(in tailwaters)', meandiff = fishd0$meandiff),
  data.frame(effect = 'climate effect on\n fish species', meandiff = fishd$meandiff),
  data.frame(effect = 'reservoir effect on\n mussel species\n(in tailwaters)', meandiff = musseld0$meandiff),
  data.frame(effect = 'climate effect on\n mussel species', meandiff = musseld$meandiff)
)
combined_data$effect <- factor(combined_data$effect, levels = c(
  'climate effect on\n fish species',
  'reservoir effect on\n fish species\n(in tailwaters)',
  'climate effect on\n mussel species',
  'reservoir effect on\n mussel species\n(in tailwaters)'
))
ggplot(combined_data, aes(x = effect, y = meandiff)) + 
  geom_boxplot() +
  ylab('mean difference in probability of occurrence \n due to the effect') + 
  xlab('') + 
  ggtitle('effect of reservoir and climate change on occurrence probability of species') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(combined_data, aes(x = effect, y = meandiff)) + 
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
  width = 0.12,
  # removing outliers
  alpha = 0.5,
  outlier.color = NA
) +   
ylab('mean difference in occurrence probability of species') + 
xlab('') + 
theme(plot.title = element_text(hjust = 0.5),) +
theme(axis.text.x = element_text(size=14),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17))

# ttest fishd0 and musseld0 is looking at reservoir effect
# fishd and musseld is looking at the climate change effect
t.test(fishd0$meandiff) 
t.test(fishd$meandiff)
t.test(musseld0$meandiff)
t.test(musseld$meandiff)
