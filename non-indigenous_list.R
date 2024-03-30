# make a datatable of which species in my comprehensive species list is 
# non-indigenous in which huc8s.

# combine all the non-indigenous species list per huc8s (already done)
{
dir = 'G:/My Drive/research/sdm_modeling/spdata'

huc8sref= read.csv('G:/My Drive/research/sdm_modeling/sdm_results/spcount/GCM_average/df2_total_spcount_nsd0.4_currentpri.csv')
huc8sref = huc8sref$HUC8

start = seq(1,2100,100)
finish = start + 99
filenames = sprintf('%s/non-indigenous_byhuc8_%d-%d.csv',dir,start,finish)
filenames = c(filenames,
             sprintf('%s/non-indigenous_byhuc8_2101-2114.csv',dir))

maxncol = 0
for(filename in filenames)
{
  d = read.csv(filename)
  print(ncol(d))
  if(maxncol < ncol(d))
  {
    maxncol = ncol(d)
  }
}
dd = c()
for(filename in filenames)
{
  d = read.csv(filename)
  d = as.matrix(d)
  if(ncol(d)<maxncol)
  {
    d = cbind(d,matrix(rep("",(maxncol-ncol(d))*nrow(d)),ncol=(maxncol-ncol(d))))
  }
  dd = rbind(dd,d)
}
dim(dd)
ofilename = sprintf('%s/non-indigenous_byhuc8.csv',dir)
write.csv(dd,ofilename,row.names=FALSE)
}

# make a new datatable of which species in my comprehensive species list is 
# non-indigenous in which huc8 unit.
# each row is a huc8 unit and each column are the species in the comprehensive species 
# list 
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
dir = 'G:/My Drive/research/sdm_modeling/spdata'
filename = sprintf('%s/non-indigenous_byhuc8.csv',dir)
d = read.csv(filename)
check_presence <- function(splist,name)
{
  splist = splist[-which(splist=="" | grepl('sp.',splist))]
  if(length(splist)==1)
  {
    return(0)
  }
  splist = splist[2:length(splist)]
  idx = which(sapply(strsplit(as.character(splist),' '),length)!=2)
  if(length(idx)>0)
  {
    splist = splist[-idx]
  }
  check = which(splist==name)
  if(length(check)>0)
  {
    return(1)
  } else {
    return(0)
  }
}
dd = c()
for( i in 1:nrow(spnamedata))
{
  nameval = spnamedata$name[i]
  nonindsp_presence = apply(as.matrix(1:length(d$huc8)),1,function(x) check_presence(d[x,],nameval))
  dd = cbind(dd,nonindsp_presence)
}
dd = cbind(d$huc8,dd)
colnames(dd) = c('HUC8',spnamedata$name)
head(dd)
ofilename = sprintf('%s/non-indigenous_byhuc8_studyspecies.csv',dir)
write.csv(dd,ofilename,row.names=FALSE)
