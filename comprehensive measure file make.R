# pull all the measures and their associated cost from each project document csv files and
# combine them to make one single csv file.
rm(list=ls())
comprehensive = data.frame(measure=c(),adopted.by.staff=c(),
                           annualized.cost=c(),dollaryr=c(),annualized.cost.2022=c(),
                           subheader=c(),fercid=c(),documentid=c())
projd = read.csv('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv')

wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc_docs/table extracts/convert2excel/processed'
files = list.files(wd)
files = files[which(grepl('\\.csv',files))]

# inflation data
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/inflation data.csv'
infld = read.csv(filename2) 
infld$dollar = as.numeric(gsub('\\$','',infld$dollar))

for(filename in files)
{
  ifilename = sprintf('%s/%s',wd,filename)
  d = read.csv(ifilename)
  subheader_exist = any(d$subheader==1)
  if(subheader_exist)
  {
    s_idx = which(d$subheader==1)
  }
  f_prep = gsub('P-[0-9]+_','',filename)
  f_prep = gsub('-processed.csv','',f_prep)
  f = gsub('-processed.csv','',f_prep)
  id = gsub('_P-[0-9]+-processed.csv+','',filename)
  yr = unique(projd$dollar_yr[which(projd$documentID==id)])
  if(filename=="P-935_P-935&2071&2111-processed.csv")
  {
    yr = 2006
    id = 'P-935'
  }
  d$annualized.cost.2022 = 
    d$annualized.cost*(infld$dollar[which(infld$year==2022)]/
                         infld$dollar[which(infld$year==yr)])
  
  for(i in 1:nrow(d))
  {
    if(d$subheader[i]==1)
    {
      next
    }
    if(subheader_exist)
    {
      idxdiff = i - s_idx
      if(any(idxdiff>0))
      {
        s=d$measure[s_idx[which(idxdiff>0 & idxdiff==min(idxdiff[which(idxdiff>0)]))]]        
      } else {
        s= NA
      }

    } else
    {
      s = NA
    }
    m=d$measure[i]
    a=d$adopted.by.staff[i]
    ac=d$annualized.cost[i]
    ac22=d$annualized.cost.2022[i]
    line = data.frame(measure=m,adopted.by.staff=a,annualized.cost=ac,dollaryr=yr,annualized.cost.2022=ac22,
                      subheader=s,fercid=f,documentid=id)     
    comprehensive = rbind(comprehensive,line)
  }
  print(sprintf('%s done (%d/%d)',filename,which(files==filename),length(files)))
}
ofilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/measure_compilation.csv'
write.csv(comprehensive,ofilename, row.names=FALSE)
