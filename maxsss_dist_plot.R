# make boxplots of average temp of projection are for each species
rm(list=ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
gcm = 'DOE-ACCESS-CM2'
curreswd = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/pristine_gcm_reservoir/%s/current',gcm)
futreswd = sprintf('D:/sdm_modeling/by_sp predictor data temporary storage/pristine_gcm_reservoir/%s/future',gcm)
maxsss = c()
for(i in idx0)
{
  spname = spnamedata$name[i]
  filename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/GCM_average/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/avg_prediction_thresholds.csv',spname)
  d = fread(filename)
  maxsss = c(maxsss,d$maxsss)
}

maxsss
combined_data <- rbind(
  data.frame(effect = 'MaxSSS Threshold', maxsss = maxsss)
)

ggplot(combined_data, aes(x = effect, y = maxsss)) + 
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
  ylab('') + 
  xlab('') + 
  scale_x_discrete(labels = '') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17))

