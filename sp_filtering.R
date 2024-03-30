# figuring out which species should be included in the study by year collected and sample size (only take species with >30 occurrence pts)
setwd('F:/sdm_modeling/spdata/')
sp_list = read.csv('sp_list2.csv') #complete list that concatenates all the lists from above.
sp_list = sp_list$species_name
setwd('F:/sdm_modeling/spdata/per_sp')
# make a df with sp. name, occ point with earliest yr, occ point with latest yr, and occ point amount
df = data.frame(name=character(), start.yr=integer(),end.yr=integer(),occnum=integer()) 
cutoff = 0
# fill out the df
for(i in 1:length(sp_list))
{
  filename = sprintf('%s.csv',sp_list[i])
  spocc = read.csv(filename)
  if(ncol(spocc)==1)
  {
    spocc = read.csv(filename,sep = "\t")
  }
  spocc = spocc[!(is.na(spocc$year)),]
  spocc = spocc[spocc$year>=cutoff,]
  df = rbind(df,c(spocc$species[1],min(spocc$year),max(spocc$year),nrow(spocc)))
}
names(df) = c('name','start.yr','end.yr','occnum')
df$start.yr = as.numeric(df$start.yr)
df$end.yr = as.numeric(df$end.yr)
df$occnum = as.numeric(df$occnum)

min(df$start.yr)
max(df$end.yr)
hist(df$occnum)
minsize = 30 # min number of occ points for the sp to be considered for conducting SDM
sort(df$occnum[df$occnum>=minsize])
length(df$occnum[df$occnum>=minsize]) # number of species that are included in the study after filtering for time span and sample size (>30)
median(df$occnum[df$occnum>=minsize]) # median number of occurrence point data for species
sp_idx_included = which(df$occnum>=minsize) # sp idx for those who are included in the study
sp_name_included = sp_list[which(df$occnum>=minsize)] # sp name of those who are included in the study

write.csv(data.frame(species_name=sp_name_included),'F:/sdm_modeling/spdata/sp_list2_occ_over30.csv')
