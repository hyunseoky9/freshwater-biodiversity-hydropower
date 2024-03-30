library(gridExtra)
library(data.table)
library(ggplot2)
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)
# get average stream order between current and future scenario.
# separate between mussels and fish.
# 3.2 plotting the results
rm(list=ls())
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

# *run from this line to just get the result
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/streamorder_difference.csv'
#write.csv(sodiff,ofilename,row.names=FALSE)
sodiff = read.csv(ofilename)
sodiff = sodiff$x

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

fishsodiff = sodiff[which(fishidx==1)]
musselsodiff = sodiff[which(fishidx==0)]

hist(fishsodiff)
hist(musselsodiff)
mean(na.omit(fishsodiff))
sd(na.omit(fishsodiff))
sd(na.omit(fishsodiff))/sqrt(length(na.omit(fishsodiff)))
mean(na.omit(musselsodiff))
sd(na.omit(musselsodiff))
sd(na.omit(musselsodiff))/sqrt(length(na.omit(musselsodiff)))

# stats by warm, cool, and cold water fish
tempidx  = spnamedata$thermal_pref[idx0]
warmsodiff = sodiff[which(tempidx==1)]
coolsodiff = sodiff[which(tempidx==2)]
coldsodiff = sodiff[which(tempidx==0)]

mean(na.omit(warmsodiff))
sd(na.omit(warmsodiff))
sd(na.omit(warmsodiff))/sqrt(length(na.omit(warmsodiff)))
mean(na.omit(coolsodiff))
sd(na.omit(coolsodiff))
sd(na.omit(coolsodiff))/sqrt(length(na.omit(coolsodiff)))
mean(na.omit(coldsodiff))
sd(na.omit(coldsodiff))
sd(na.omit(coldsodiff))/sqrt(length(na.omit(coldsodiff)))


combined_data <- rbind(
  data.frame(effect = 'fish', stream_order_diff = fishsodiff),
  data.frame(effect = 'mussel', stream_order_diff = musselsodiff),
  data.frame(effect = 'warm', stream_order_diff = warmsodiff),
  data.frame(effect = 'cool', stream_order_diff = coolsodiff),
  data.frame(effect = 'cold', stream_order_diff = coldsodiff)
)

combined_data$effect <- factor(combined_data$effect, levels = c(
  'fish',
  'warm',
  'cool',
  'cold',
  'mussel'

))

# figure B draft1
ggplot(combined_data, aes(x = effect, y = stream_order_diff)) + 
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
  ylim(-2,2)+
  ylab('mean shift in stream order') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=22),
        axis.title.y = element_text(size=20))


#ggtitle('effect of reservoir and climate change on
#          occurrence probability of species') +
  
# ttest
t.test(fishsodiff)
t.test(musselsodiff)
t.test(warmsodiff)
t.test(coolsodiff)
t.test(coldsodiff)



# 2. make a double violin plot with left side as percent length change (in km)
# and right side as stream order change 
# for fish warm cool cold and mussel and 
# template in sdm_result_analysis1.4.R

# a) first, make a violin plot for each (stream order change & percent length change)
# a1) so diff (read the data from above.)
fishsodiff
musselsodiff
warmsodiff
coolsodiff
coldsodiff

min(fishsodiff,na.rm=TRUE)
min(musselsodiff,na.rm=TRUE)
min(warmsodiff,na.rm=TRUE)
min(coolsodiff,na.rm=TRUE)
min(coldsodiff,na.rm=TRUE)

# a2) length change or percent length change.
# first investigate the data with box-violin plot. 
filename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/climateNreservoir_effects_by_kmreachlength.csv'
d = read.csv(filename)

lenchange = d$fr_occ_kmlen - d$cr_occ_kmlen 
lenpchange = (d$fr_occ_kmlen - d$cr_occ_kmlen)/d$cr_occ_kmlen * 100 #percent change

fishlenchange = lenchange[which(fishidx==1)]
mussellenchange = lenchange[which(fishidx==0)]
warmlenchange = lenchange[which(tempidx==1)]
coollenchange = lenchange[which(tempidx==2)]
coldlenchange = lenchange[which(tempidx==0)]

fishlenpchange = lenpchange[which(fishidx==1)]
mussellenpchange = lenpchange[which(fishidx==0)]
warmlenpchange = lenpchange[which(tempidx==1)]
coollenpchange = lenpchange[which(tempidx==2)]
coldlenpchange = lenpchange[which(tempidx==0)]

max(fishlenpchange,na.rm = T)
max(mussellenpchange,na.rm = T)
max(warmlenpchange,na.rm = T)
max(coollenpchange,na.rm = T)
max(coldlenpchange,na.rm = T)


quantile(fishlenpchange,na.rm = T,probs = c(0.99))
quantile(mussellenpchange,na.rm = T,probs = c(0.99))
quantile(warmlenpchange,na.rm = T,probs = c(0.99))
quantile(coollenpchange,na.rm = T,probs = c(0.97))
quantile(coldlenpchange,na.rm = T,probs = c(1))

# raw value change
combined_data2 <- rbind(
  data.frame(effect = 'fish', lenchange = fishlenchange),
  data.frame(effect = 'mussel', lenchange = mussellenchange),
  data.frame(effect = 'warm', lenchange = warmlenchange),
  data.frame(effect = 'cool', lenchange = coollenchange),
  data.frame(effect = 'cold', lenchange = coldlenchange)
)

combined_data2$effect <- factor(combined_data2$effect, levels = c(
  'fish',
  'warm',
  'cool',
  'cold',
  'mussel'
  
))
# percent change
combined_data3 <- rbind(
  data.frame(effect = 'fish', lenpchange = fishlenpchange),
  data.frame(effect = 'mussel', lenpchange = mussellenpchange),
  data.frame(effect = 'warm', lenpchange = warmlenpchange),
  data.frame(effect = 'cool', lenpchange = coollenpchange),
  data.frame(effect = 'cold', lenpchange = coldlenpchange)
)

combined_data3$effect <- factor(combined_data2$effect, levels = c(
  'fish',
  'warm',
  'cool',
  'cold',
  'mussel'
))


ggplot(combined_data2, aes(x = effect, y = lenchange)) + 
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
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17))

# figure B draft3
ggplot(combined_data3, aes(x = effect, y = lenpchange)) + 
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
  ylim(-100,600)+
  ylab('percent change in range length') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17))

# mean and se
mean(fishlenpchange)
mean(mussellenpchange)
mean(warmlenpchange)
mean(coollenpchange)
mean(coldlenpchange)

sd(fishlenpchange)/sqrt(length(fishlenpchange))
sd(mussellenpchange)/sqrt(length(mussellenpchange))
sd(warmlenpchange)/sqrt(length(warmlenpchange))
sd(coollenpchange)/sqrt(length(coollenpchange))
sd(coldlenpchange)/sqrt(length(coldlenpchange))











# Figure B (two figures in 1. Just run the codes below to get the figure B)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

# *run from this line to just get the result
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
fishidx = spnamedata$mussel_or_fish[idx0]

ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/streamorder_difference.csv'
sodiff = read.csv(ofilename)
sodiff = sodiff$x

fishsodiff = sodiff[which(fishidx==1)]
musselsodiff = sodiff[which(fishidx==0)]

# stats by warm, cool, and cold water fish
tempidx  = spnamedata$thermal_pref[idx0]
warmsodiff = sodiff[which(tempidx==1)]
coolsodiff = sodiff[which(tempidx==2)]
coldsodiff = sodiff[which(tempidx==0)]

combined_data <- rbind(
  data.frame(effect = 'fish', stream_order_diff = fishsodiff),
  data.frame(effect = 'mussel', stream_order_diff = musselsodiff),
  data.frame(effect = 'warm', stream_order_diff = warmsodiff),
  data.frame(effect = 'cool', stream_order_diff = coolsodiff),
  data.frame(effect = 'cold', stream_order_diff = coldsodiff)
)

combined_data$effect <- factor(combined_data$effect, levels = c(
  'fish',
  'warm',
  'cool',
  'cold',
  'mussel'
  
))

axistextxsize=25
axistextysize=22
axistitleysize=20

# figure B draft1
streamorder_fig = ggplot(combined_data, aes(x = effect, y = stream_order_diff)) + 
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
  ylim(-2,2)+
  ylab('mean shift in stream order') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=axistextxsize),
        axis.text.y = element_text(size=axistextysize),
        axis.title.y = element_text(size=axistitleysize))


filename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/climateNreservoir_effects_by_kmreachlength.csv'
d = read.csv(filename)
lenpchange = (d$fr_occ_kmlen - d$cr_occ_kmlen)/d$cr_occ_kmlen * 100 #percent change
fishlenpchange = lenpchange[which(fishidx==1)]
mussellenpchange = lenpchange[which(fishidx==0)]
warmlenpchange = lenpchange[which(tempidx==1)]
coollenpchange = lenpchange[which(tempidx==2)]
coldlenpchange = lenpchange[which(tempidx==0)]

# percent change
combined_data3 <- rbind(
  data.frame(effect = 'fish', lenpchange = fishlenpchange),
  data.frame(effect = 'mussel', lenpchange = mussellenpchange),
  data.frame(effect = 'warm', lenpchange = warmlenpchange),
  data.frame(effect = 'cool', lenpchange = coollenpchange),
  data.frame(effect = 'cold', lenpchange = coldlenpchange)
)

combined_data3$effect <- factor(combined_data3$effect, levels = c(
  'fish',
  'warm',
  'cool',
  'cold',
  'mussel'
))


# figure B draft3
lenpch_fig = ggplot(combined_data3, aes(x = effect, y = lenpchange)) + 
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
  ylim(-100,600)+
  ylab('percent change in range length') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=axistextxsize),
        axis.text.y = element_text(size=axistextysize),
        axis.title.y = element_text(size=axistitleysize))



grid.arrange(streamorder_fig, lenpch_fig, ncol = 2)
