# get list of species
library(data.table)
filename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spdata = read.csv(filename)
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/listed sp in ea eis.csv'
listedsp = read.csv(filename2)
filename3 = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_allyr.csv'
spdataold = read.csv(filename3)
filename4 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename4)
filename5 = 'G:/My Drive/research/sdm_modeling/sdm_results/spcount/GCM_average/df2_total_spcount_nsd0.4_currentres.csv'
spcount = read.csv(filename5)

head(listedsp)
matches = match(listedsp$scientific.name,spdata$name)
matches2 = match(listedsp$scientific.name,spdataold$name)
listedsp$scientific.name[which(!is.na(matches))]
listedsp$scientific.name[which(!is.na(matches2))]


commonsp = listedsp$scientific.name[which(!is.na(matches))]
commonspidx = which(!is.na(matches))
prop0 = c()
for( i in commonspidx[1:(length(commonspidx)-8)])
{
  wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
  filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
  d = read.csv(filename)
  dd = d
  keyword = listedsp$keywords[i]
  exclude = c()
  idx = which(grepl(keyword,dd$measure))      
  if(length(exclude)>0)
  {idx = idx[-exclude]}
  projectids = unique(dd$fercid[idx])
  projhuc8s = unique(fercd$huc8[which(fercd$projectID %in% projectids)])
  
  spname_edit = gsub(' ' ,'\\.',commonsp[i])
  sp_presence = apply(as.matrix(projhuc8s),1,function(x) spcount[which(spcount$HUC8==x),which(names(spcount)==spname_edit)])
  #print(spname_edit)
  #print(rbind(projhuc8s,sp_presence))
  sp_presence = as.numeric(sp_presence)
  prop0 = c(prop0,length(which(sp_presence==0))/length(sp_presence))
}
# proportion of species presence predictions that says the species is absent when it shouldn't be 
# (for each species in my study list that's also metioned in the plants' Environmental Assessment)
rbind(prop0,commonsp[1:(length(commonspidx)-8)])


