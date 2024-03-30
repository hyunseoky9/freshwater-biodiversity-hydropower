# 3.3
# what's driving warm water species to be the only type to have a significantincrease in stream order?

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')


#what is the sample size of warm cold cool species.
# 0=cold (n=35), 1=warm (n=381), 2=cool (n=55)
length(which(spnamedata$thermal_pref==0))
length(which(spnamedata$thermal_pref==1))
length(which(spnamedata$thermal_pref==2))
#what is the critical temperature threshold for warm cool and cold species.

tempidx  = spnamedata$thermal_pref[idx0]
tmax = spnamedata$tmax[idx0]
warmtmax = tmax[which(tempidx==1)]
cooltmax = tmax[which(tempidx==2)]
coldtmax = tmax[which(tempidx==0)]
mean(warmtmax)
mean(cooltmax)
mean(coldtmax)
warmsodiff = sodiff[which(tempidx==1)]
coolsodiff = sodiff[which(tempidx==2)]
coldsodiff = sodiff[which(tempidx==0)]
length(warmsodiff) #334
length(coolsodiff) #52
length(coldsodiff) #34
# ANSWER: It's the sample size difference. There's just sufficient number of warmwater 
# species in my dataset for the stream order change to come out significant for the warmwater
# but not for cold or cool water species where the number of species belonging to that 
# category is small.


hist(warmsodiff)
hist(coolsodiff)
hist(coldsodiff)
