spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
tmax = spnamedata$tmax
fishtmax = tmax[which(spnamedata$mussel_or_fish==1)]
musseltmax = tmax[which(spnamedata$mussel_or_fish==0)]
warmtmax = tmax[which(spnamedata$thermal_pref==1)]
coldtmax = tmax[which(spnamedata$thermal_pref==0)]
cooltmax = tmax[which(spnamedata$thermal_pref==2)]
combined_data <- rbind(
  data.frame(effect = 'fish', tmax = fishtmax),
  data.frame(effect = 'mussel', tmax = musseltmax),
  data.frame(effect = 'warm', tmax = warmtmax),
  data.frame(effect = 'cool', tmax = cooltmax),
  data.frame(effect = 'cold', tmax = coldtmax)
)


combined_data$effect_string = combined_data$effect
combined_data$effect <- factor(combined_data$effect, levels = c(
  'fish',
  'warm',
  'cool',
  'cold',
  'mussel'
))

head(combined_data)
ggplot(combined_data, aes(x = effect, y = tmax)) + 
  geom_violin() +   
  ylab('Upper Thermal Tolerance (Celsius)') + 
  xlab('') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17))

