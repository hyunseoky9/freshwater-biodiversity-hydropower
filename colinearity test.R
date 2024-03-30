# multicolinearity test of all the sdm predictors 
library(data.table)
library(usdm)
# get pristine current predictors for all hucs and rbind them.
i=2
#for( i in 1:18)
{
  ntffilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
  tffilename = sprintf('G:/My Drive/research/sdm_modeling/wbmdata/pristine w gcm/DOE-ACCESS-CM2/temp_and_flow_predictors/current/by_huc/huc%d_projarea_tempNflow_predictors.csv',i)
  ntfd = read.csv(ntffilename)
  tfd = read.csv(tffilename)
  idx = match(ntfd$wbmID_1min,tfd$wbmID_1min)
  d = cbind(ntfd[,c('BFI','waterbody','streamorder')],tfd[idx,2:ncol(tfd)])
  #if(i==1)
  #{
  #  d = cbind(ntfd[,c('BFI','waterbody','streamorder')],tfd[idx,2:ncol(tfd)])
  #} else {
  #  d = rbind(d,cbind(ntfd[,c('BFI','waterbody','streamorder')],tfd[idx,2:ncol(tfd)]))
  #}
  # make na bfi values 0 
  d$BFI[which(is.na(d$BFI))] = 0
  # exclude rows with streamorder value of -9
  d = d[-which(d$streamorder==-9),]
  # get rid of rows with NA tempflow related predictors 
  idx= which(is.na(d$numday_above_tmax_12.200))
  d = d[-idx,]
}

usdm::vif(d)
custom = d[,c('BFI','waterbody','streamorder','numday_above_tmax_12.200','numday_below_tmin_3.400','dd90_10c','minflow','maxflowdate','minflowdate','avgflow')]
usdm::vif(custom)

# compare vif for every pair of numday above tmax and numday below tmin predictor. 
aboveidx = which(grepl('numday_above',names(d)))
belowidx = which(grepl('numday_below',names(d)))
record= c()
for(i in 1:length(aboveidx))
{
  for(j in 1:length(belowidx))
  {
    a = usdm::vif(d[,c(aboveidx[i],belowidx[j])])    
    record = rbind(record,c(a[1,2],a[2,2]))
  }
  print(i)
}
tmaxrecord = record[which(!is.nan(record[,1])),1]
tminrecord = record[which(!is.nan(record[,2])),2]
min(tmaxrecord)
min(tminrecord)
head(record)
cor(d[,c('numday_above_tmax_12.200','numday_below_tmin_3.400','dd90_10c','avgtemp')])




aboveidx = which(grepl('numday_above',names(d)))
ddidx = which(grepl('dd',names(d)))
record= c()
for(i in 1:length(aboveidx))
{
  for(j in 1:length(ddidx))
  {
    a = usdm::vif(d[,c(aboveidx[i],ddidx[j])])    
    record = rbind(record,c(a[1,2],a[2,2]))
  }
  print(i)
}
tmaxrecord = record[which(!is.nan(record[,1])),1]
ddrecord = record[which(!is.nan(record[,2])),2]
sort(tmaxrecord)
sort(ddrecord)




# 3. calculate vif of dd predictors for every numdayabove pred.

aboveidx = which(grepl('numday_above',names(d)))
ddidx = which(grepl('dd',names(d)))
record= matrix(rep(NA,length(ddidx)*length(aboveidx)),nrow=length(ddidx))

for(i in 1:length(aboveidx))
{
  for(j in 1:length(ddidx))
  {
    a = usdm::vif(d[,c(aboveidx[i],ddidx[j])])    
    record[j,i] = a[2,2]
  }
  print(i)
}
for(j in 1:length(ddidx))
{
  print(sprintf('%s vif with numday aboves',names(d)[ddidx[j]]))
  r = record[j,]
  print(sprintf('number of vifs above 10 = %d',length(which(r>10))))
  #print(r[which(r>10)])
}
tmaxrecord = record[which(!is.nan(record[,1])),1]
ddrecord = record[which(!is.nan(record[,2])),2]
sort(tmaxrecord)
sort(ddrecord)




# avengers
'BFI'
'waterbody'
'streamorder'
'numday_above_tmax'
'dd90_10c' (?)
'minflow'
'minflowdate'
'maxflowdate'
'avgflow'