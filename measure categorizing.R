# cateogrizing measure data
wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
d = read.csv(filename)
length(which(d$freshWsp.cons.related==''))
keyword = '[Bb]ull [Tt]rout'
exkeyword = ''
exclude = c()
if(exkeyword!='')
{
  idx = which(grepl(keyword,d$measure)& !grepl(exkeyword,d$measure) & d$freshWsp.cons.related=="")  
} else {
  idx = which(grepl(keyword,d$measure) & d$freshWsp.cons.related=="")  
}
if(length(exclude)>0)
{idx = idx[-exclude]}
length(idx)
d$measure[idx]
d$freshWsp.cons.related[idx] = 0

ofilename = filename 
#write.csv(d,ofilename,row.names=FALSE)


# put in fishing/fishery related as separate category.
wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
d = read.csv(filename)
keyword = 
  exkeyword = ''
exclude = c()
conservation.related = 0
if(exkeyword!='')
{
  idx = which(grepl(keyword,d$measure)& !grepl(exkeyword,d$measure) & d$freshWsp.cons.related==conservation.related)  
} else {
  idx = which(grepl(keyword,d$measure) & d$freshWsp.cons.related==conservation.related)  
}
if(length(exclude)>0)
{idx = idx[-exclude]}
length(idx)
d$measure[idx]
d$freshWsp.cons.related[idx] = 1
#d$game.related[idx] = 1

ofilename = filename 
write.csv(d,ofilename,row.names=FALSE)


# categorize conservation related measures into 
# species-targeted conservation vs non-species-targeted conservation

{
  wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
  filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
  d = read.csv(filename)
  dd = d[which(d$freshWsp.cons.related==1),]
  length(dd$measure[which(is.na(dd$sp.spcific))])
  keyword = '[Mm]igrant' 
  exkeyword = ''
  exclude = c()
  if(exkeyword!='')
  {
    idx = which(grepl(keyword,dd$measure)& !grepl(exkeyword,dd$measure) & is.na(dd$sp.spcific))  
  } else {
    idx = which(grepl(keyword,dd$measure) & is.na(dd$sp.spcific))  
  }
  if(length(exclude)>0)
  {idx = idx[-exclude]}
  length(idx)
  dd$measure[idx]
}


d$sp.spcific[which(d$ID %in% dd$ID[idx])] = 0

ofilename = filename 
write.csv(d,ofilename,row.names=FALSE)



# listed/game/migrating species related categorizing using species names. 
{wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
  studyspd = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
  spfilename = sprintf('%s/listed sp in ea eis.csv',wd)
  spd = read.csv(spfilename)
  docd = read.csv('G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv')
  listedhuc8 = read.csv(sprintf('G:/My Drive/research/sdm_modeling/spdata/listed_byhuc8_studyspecies.csv'))
  spd$keyword
  listed= 1
  anadromous = 0
  game = 0
}
{if(listed)
{
  iis = which(spd$listed!=0)
  print(which(spd$listed!=0))
} else if(anadromous)
{
  iis = which(spd$migrating==1)
  print(which(spd$migrating==1))
} else if(game)
{
  iis = which(spd$game==1)
  print(which(spd$game==1))
}}
ii = 15
print(spd$full.name[ii])
{
  if(spd$keyword.duplicated[ii]=='1')
  {
    print('already done')
  }
  filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
  d = read.csv(filename)
  dd = d[which(d$freshWsp.cons.related==1 & d$sp.spcific==1),]
  #keywords = c('[sS]turgeon','[tT]rout','[eE]el','[Cc]hinook','[sS]turgeon','[pP]ikeminnow','[lL]amprey','[sS]ockeye','[sS]teelhead','[sS]almon','[lL]ogperch','[cC]utthroat','[wW]alleye','[sS]icklefin','[rR]edhorse','[tT]rout','[sS]melt','[rR]ainbow','[bB]ass','[cC]oho','[sS]had','[Pp]addlefish','[hH]erring','[sS]turgeon','[mM]ykiss','[cC]hum','[Rr]esident','[Aa]nadromous')
  keyword = spd$keywords[ii]
  exclude = c(1,2,3,4,6,7)
  if(listed)
  {
    idx = which(grepl(keyword,dd$measure) & dd$sp.spcific==1 & is.na(dd$listed.related)) 
  } else if (anadromous)
  {
    idx = which(grepl(keyword,dd$measure) & dd$sp.spcific==1 & is.na(dd$migratingsp.related))      
  } else if (game)
  {
    idx = which(grepl(keyword,dd$measure) & dd$sp.spcific==1 & is.na(dd$game.related))      
  }
  if(length(exclude)>0)
  {idx = idx[-exclude]}
  print(length(idx))
  
  if(listed & spd$listed[ii]=='partial')
  {
    fercid = dd$fercid[idx]
    projhuc8_w_measures = as.numeric(docd$huc8[match(fercid,docd$projectID)])
    spidx = which(studyspd$name==spd$scientific.name[ii])
    listed_status = listedhuc8[match(projhuc8_w_measures,listedhuc8$HUC8),spidx+1]
    idx = idx[which(listed_status==1)]
  }
  if(game & spd$listed[ii]=='partial')
  {
    fercid = dd$fercid[idx]
    projhuc8_w_measures = as.numeric(docd$huc8[match(fercid,docd$projectID)])
    
    spidx = which(studyspd$name==spd$scientific.name[ii])
    listed_status = listedhuc8[match(projhuc8_w_measures,listedhuc8$HUC8),spidx+1]
    idx = idx[which(listed_status==0 | is.na(listed_status))]
  }
  print(dd$measure[idx])
  print(spd$full.name[ii])
}
if(length(idx)>0)
{
  if(listed)
  {
    d$listed.related[which(d$ID %in% dd$ID[idx])] = 1
  } else if (anadromous)
  {
    d$migratingsp.related[which(d$ID %in% dd$ID[idx])] = 1
  } else if (game)
  {
    d$game.related[which(d$ID %in% dd$ID[idx])] = 1
  }
}

ofilename = filename 
write.csv(d,ofilename,row.names=FALSE)
time = Sys.time()
time = gsub(':',';',time)
ofilename2 = sprintf('%scopies/%s%s%s',wd,'measure_compilation_with_categorization_',time,'.csv')
write.csv(d,ofilename2,row.names=FALSE)

# further categorizing migrating species related measures
{
  wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
  filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
  d = read.csv(filename)
  dd = d[which(d$freshWsp.cons.related==1 & d$sp.spcific==1),]
  keyword = '[Bb]ull Ttrout'
  exclude = c()
  idx = which(grepl(keyword,dd$measure) & dd$sp.spcific==1 & is.na(dd$migratingsp.related))      
  if(length(exclude)>0)
  {idx = idx[-exclude]}
  print(dd$measure[idx])
  length(dd$measure[idx])
}
d$migratingsp.related[which(d$ID %in% dd$ID[idx])] = 1

ofilename = filename 
write.csv(d,ofilename,row.names=FALSE)
time = Sys.time()
time = gsub(':',';',time)
ofilename2 = sprintf('%scopies/%s%s%s',wd,'measure_compilation_with_categorization_',time,'.csv')
write.csv(d,ofilename2,row.names=FALSE)

# categorize non-species targeted measures that directly affect the persistence of species.
# keyword suggestion: 'monitoring', 'study','survey','assessment'
{
  wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
  filename = sprintf('%s/%s',wd,'measure_compilation_with_categorization.csv')
  d = read.csv(filename)
  dd = d[which(d$freshWsp.cons.related==1 & d$sp.spcific==0),]
  keyword = '[Bb]ull'
  exclude = c()
  idx = which(grepl(keyword,dd$measure) & dd$sp.spcific==0 & is.na(dd$direct.nonspecific))      
  if(length(exclude)>0)
  {idx = idx[-exclude]}
  print(dd$measure[idx])
  length(dd$measure[idx])
}
d$migratingsp.related[which(d$ID %in% dd$ID[idx])] = 1

ofilename = filename 
write.csv(d,ofilename,row.names=FALSE)
time = Sys.time()
time = gsub(':',';',time)
ofilename2 = sprintf('%scopies/%s%s%s',wd,'measure_compilation_with_categorization_',time,'.csv')
write.csv(d,ofilename2,row.names=FALSE)
