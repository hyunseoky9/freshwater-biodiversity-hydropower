# net change in range length 
library(nhdplusTools)
library(sf)
library(dplyr)
library(pbapply)
library(data.table)
library(ggplot2)
library(tidyquant)
library(ggdist)
library(ggthemes)

rm(list=ls())
# 1.get net change in range length
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

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

combined_data <- rbind(
  data.frame(effect = 'fish', netchange_percentage = netchange_percentage[fish]),
  data.frame(effect = 'mussel', netchange_percentage = netchange_percentage[mussel]),
  data.frame(effect = 'native', netchange_percentage = netchange_percentage[native]),
  data.frame(effect = 'non-\nindigenous', netchange_percentage = netchange_percentage[nonindigenous])
)

combined_data2 <- rbind(
  data.frame(effect = 'fish', netchange_percentage = netchange_percentage[fish]),
  data.frame(effect = 'mussel', netchange_percentage = netchange_percentage[mussel])
)

ggplot(combined_data, aes(netchange_percentage)) +
  geom_histogram(binwidth = 25) +
  facet_wrap(~ effect, ncol = 1) +
  scale_x_continuous(limits = c(-100, NA), breaks = seq(-100, 1000, by = 200))
# violin + boxplot
ggplot(combined_data2, aes(x = effect, y = netchange_percentage)) + 
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
  ylab('percent change in total length of the projected
       species distribution in the future') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-100, NA),breaks = c(-100,0,100,250,500,750,1000))  +
theme(plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17))




  #ggtitle('effect of reservoir and climate change on
#          occurrence probability of species') +
# ttest  
t.test(netchange_percentage[fish])
t.test(netchange_percentage[mussel])
t.test(netchange_percentage[native])
t.test(netchange_percentage[nonindigenous])
