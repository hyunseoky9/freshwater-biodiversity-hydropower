#script getting the thermal tolerance (tmin, tmax, and warm/cold water species classification) info of species.

#1.  get the tmin and tmax data from frimpong and angemeier 2009 data for the species list. 
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
frim = read.csv('G:/My Drive/research/sdm_modeling/spdata/FishTraits_allSP_formatted.csv')
frim= frim[,c('X','mintemp','maxtemp')]
names(frim)[1] = 'name'
fish = spnamedata[which(spnamedata$mussel_or_fish==1),]
for ( i in 1:nrow(spnamedata))
{
  idx = which(frim$name == spnamedata$name[i])
  if(length(idx)==1)
  {
    if(is.na(spnamedata$tmin[i]))
    {
      spnamedata$tmin[i] = frim$mintemp[idx]    
    }
    if(is.na(spnamedata$t))
    {
      spnamedata$tmax[i] = frim$maxtemp[idx]      
    }
  }
  if(length(idx)>=2)
  {
    spnamedata$tmin[i] = frim$mintemp[idx[1]]
    spnamedata$tmax[i] = frim$maxtemp[idx[1]]
  }
}

#write.csv(spnamedata,'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv',,row.names=FALSE)

# for tmin and tmax values that are classified as not applicable on frimpong data, assign 10000 or -10000 
# based on whether its tmax or tmin, respectively.
temps = cbind(spnamedata$tmax, spnamedata$tmin)
names(temps) = c('tmax','tmin')
spnamedata$tmax[which(spnamedata$tmax==-1)] = 10000
spnamedata$tmin[which(spnamedata$tmin==-1)] = -10000
#write.csv(spnamedata,'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv',row.names=FALSE)



# 1.2 get the tmin and tmax data from Jess' thermal tolerance data data for the species list. 
library(stringi)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
jess = read.csv('G:/My Drive/research/sdm_modeling/spdata/thermal data/fish_tolerance_compiled_data_v2.csv')
# consolidate all the data for same species 
jessSp = unique(jess$Species)
jessSpcommon = unique(jess$Common)
newjess = data.frame(name=c(),tmin=c(),tmax=c())
for( i in 1:length(jessSp))
{
  idx = which(jess$Species == jessSp[i])
  tmaxidx = idx[which(!is.na(jess$Tmax[idx]))]
  tminidx = idx[which(!is.na(jess$Tmin[idx]))]
  avgtmax = round(mean(jess$Tmax[tmaxidx]),1)
  avgtmin = round(mean(jess$Tmin[tminidx]),1)
  newjess = rbind(newjess,c(jessSp[i],jessSpcommon[i],avgtmin,avgtmax))
}
names(newjess) = c('name','common','tmin','tmax')
# get tmin tmax info from jess to spnamedata
newjess$name = trimws(newjess$name)
newjess$tmin = as.numeric(newjess$tmin)
newjess$tmax = as.numeric(newjess$tmax)
newjess$tmin[which(is.nan(newjess$tmin))] = NA
newjess$tmax[which(is.nan(newjess$tmax))] = NA

matchnum = 0
matches = c()
for ( i in 1:nrow(spnamedata))
{
  idx = which(newjess$name == spnamedata$name[i])
  if(length(idx)==1)
  {
    if(is.na(spnamedata$tmin[i]))
    {
      spnamedata$tmin[i] = newjess$tmin[idx]
      spnamedata$tminref[i] = 'jess'
    }
    if(is.na(spnamedata$tmax[i]))
    {
      spnamedata$tmax[i] = newjess$tmax[idx]
      spnamedata$tmaxref[i] = 'jess'
    }
    matchnum = matchnum + 1
    matches = rbind(matches,newjess[idx,])
  }
  if(length(idx)>=2)
  {
    print('two matches!')
    #spnamedata$tmin[i] = newjess$tmin[idx[1]]
    #spnamedata$tmax[i] = newjess$tmax[idx[1]]
  }
}

# moxostoma, ictiobus genus have average tmin and tmax data in Jess' data.
moxorange = c(newjess$tmin[which(newjess$name=='Moxostoma sp.')],newjess$tmax[which(newjess$name=='Moxostoma sp.')])
ictirange = c(newjess$tmin[which(newjess$name=='Ictiobus sp.')],newjess$tmax[which(newjess$name=='Ictiobus sp.')])
for ( i in 1:nrow(spnamedata))
{
  if(spnamedata$genus[i]=='Moxostoma')
  {
    spnamedata$tmin[i] = moxorange[1]
    spnamedata$tminref[i] = 'jess'
    spnamedata$tmax[i] = moxorange[2]
    spnamedata$tmaxref[i] = 'jess'
  }
  if(spnamedata$genus[i]=='Ictiobus')
  {
    spnamedata$tmin[i] = ictirange[1]
    spnamedata$tminref[i] = 'jess'
    spnamedata$tmax[i] = ictirange[2]
    spnamedata$tmaxref[i] = 'jess'
  }
}





# 1.3 get the tmin and tmax data from globaltherm data data for the species list.
#spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
globtherm = read.csv('G:/My Drive/research/sdm_modeling/spdata/thermal data/GlobalTherm_upload_02_11_17.csv')
globtherm = globtherm[which(globtherm$Phylum == 'Chordata' | globtherm$Phylum == 'Mollusca'),] # only chordates and mollusca 
globtherm$name = paste(globtherm$Genus,globtherm$Species)
globtherm$tmax = as.numeric(globtherm$tmax)
globtherm$tmin = as.numeric(globtherm$tmin)
matchnum = 0
matches = c()
for ( i in 1:nrow(spnamedata))
{
  idx = which(globtherm$name == spnamedata$name[i])
  
  if(length(idx)==1)
  {
    if(is.na(spnamedata$tmin[i]))
    {
      spnamedata$tmin[i] = globtherm$tmin[idx]    
      spnamedata$tminref[i] = 'globtherm'
    }
    if(is.na(spnamedata$tmax[i]))
    {
      spnamedata$tmax[i] = globtherm$tmax[idx]      
      spnamedata$tmaxref[i] = 'globtherm'
    }
    matchnum = matchnum + 1
    matches = rbind(matches,globtherm[idx,])
  }
  if(length(idx)>=2)
  {
    print('two matches!')
  }
}

#write.csv(spnamedata,'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv',row.names=FALSE)
# 1.4 get the tmin and tmax and thermal preference data from lyons data data for the species list.
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
lyons = read.csv('G:/My Drive/research/sdm_modeling/spdata/thermal data/Lyons et al 2009 data.csv')
names(lyons)[5:7] = c('Tpreference','tmax','thpref')
names(lyons)[2] = 'name'
for( i in 1:length(lyons$thpref))
{
  if(!is.na(lyons$thpref[i]))
  {
    if(lyons$thpref[i] == 'Warm')
    {
      lyons$thpref[i] = 1
    } else if(lyons$thpref[i] == 'Trans')
    {
      lyons$thpref[i] = 2
    } else if(lyons$thpref[i] == 'Cold')
    {
      lyons$thpref[i] = 0
    }
  }
}
lyons$thpref = as.numeric(lyons$thpref)

matchnum = 0
matches = c()
for ( i in 1:nrow(spnamedata))
{
  idx = which(lyons$name == spnamedata$name[i])
  
  if(length(idx)==1)
  {
    if(is.na(spnamedata$tmax[i]))
    {
      spnamedata$tmax[i] = lyons$tmax[idx]      
      spnamedata$tmaxref[i] = 'lyons'
    }
    if(is.na(spnamedata$thermal_pref[i]))
    {
      spnamedata$thermal_pref[i] = lyons$thpref[idx]      
      spnamedata$thermal_prefref[i] = 'lyons'
    }
    matchnum = matchnum + 1
    matches = rbind(matches,lyons[idx,])
  }
  if(length(idx)>=2)
  {
    print('two matches!')
  }
}

# assign all Salmonidae and Cottidae species whose thermal preference has been classified as cold
# assign all Cyprinidae, Catostomidae, Ictaluridae, Centrarchidae, and Percidae
# species whose thermal preference has been classified as warm
idx = which(spnamedata$family %in% c('Salmonidae','Cottidae') & is.na(spnamedata$thermal_pref))
spnamedata$thermal_pref[idx] = 0
spnamedata$thermal_prefref[idx] = 'lyons'

idx = which(spnamedata$family %in% c('Cyprinidae','Catostomidae','Ictaluridae','Centrarchidae','Percidae') & is.na(spnamedata$thermal_pref))
spnamedata$thermal_pref[idx] = 0
spnamedata$thermal_prefref[idx] = 'lyons'





#1.5 assign all the species thermal preference in the Coker et al. to the comprehensive_sp_info.

# get scientific names for all the common names in coker (note: ALREADY DONE. NO NEED TO RUN AGAIN)
{
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
cokersp = read.csv('G:/My Drive/research/sdm_modeling/spdata/thermal data/Canadian Manuscript Report table2-speciesnamesonly.csv')
cokerdata = read.csv('G:/My Drive/research/sdm_modeling/spdata/Canadian Manuscript Report table2-temppref.csv')

result = list()
intervalength = 100
intervalnum = ceiling(length(cokersp)/intervalength)
for (i in 1:intervalnum)
{
  startidx = (i-1)*intervalength + 1
  if(i!=intervalnum)
  {
    finishidx = i*intervalength
  } else {
    finishidx = length(cokersp)
  }
  result  = c(result,comm2sci(cokersp[startidx:finishidx], db = "ncbi", simplify = TRUE, commnames = NULL))
}
result$`tessellated darter`  = 'Etheostoma olmstedi'
result$`white crappie` = 'Pomoxis annularis'
result$warmouth = 'Lepomis gulosus'
result$`rock bass` = 'Ambloplites rupestris'
result$`fourhorn sculpin` = 'Myoxocephalus quadricornis'
result$`threespine stickleback` = 'Gasterosteus aculeatus'
result$`trout-perch` = 'Percopsis omiscomaycus'
result$`Aurora trout` = 'Salvelinus fontinalis'
result$`brown trout` = 'Salmo trutta'
result$`shortnose cisco` = 'Coregonus reighardi'
result$`pygmy smelt` = 'Osmerus spectrum'
result$`surf smelt` = 'Hypomesus pretiosus'
result$`chestnut lamprey` = 'Ichthyomyzon castaneus'
result$`northern brook lamprey` = 'Ichthyomyzon fossor'
result$`Vancouver Island lamprey` = 'Entosphenus macrostomus'
result$`darktail lamprey` = 'Lethenteron alaskense'
result$`blueback herring` = 'Alosa aestivalis'
result$`redside dace` = 'Clinostomus elongatus'
result$`bridle shiner` = 'Notropis bifrenatus'
result$`finescale dace` = 'Chrosomus neogaeus'
result$`flathead chub` = 'Platygobio gracilis'
result$`northern squawfish` = 'Ptychocheilus oregonensis'
result$`pond smelt` = 'Hypomesus olidus'
result = unlist(result)

idx = apply(as.matrix(cokersp),1,function(x) which(x==names(result))[1])
length(unique(idx)) 
output = cbind(cokersp,result[idx])
row.names(output) = c()
write.csv(output,'G:/My Drive/research/sdm_modeling/spdata/Canadian Manuscript Report table2-speciesnamesonly.csv',row.names=FALSE)
}

# match all the species in the comprehensive list to the coker species.
cokerdata = read.csv('G:/My Drive/research/sdm_modeling/spdata/thermal data/Canadian Manuscript Report table2-speciesnamesonly.csv')
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
for( i in 1:length(cokerdata$temp_pref)) # change thermal preference into 0/1/2 codes
{
  if(cokerdata$temp_pref[i] == 'warm')
  {
    cokerdata$temp_pref[i] = 1
  } else if(cokerdata$temp_pref[i] == 'cold/cool')
  {
    cokerdata$temp_pref[i] = 2
  } else if(cokerdata$temp_pref[i] == 'cool/warm')
  {
    cokerdata$temp_pref[i] = 2
  } else if(cokerdata$temp_pref[i] == 'cool')
  {
    cokerdata$temp_pref[i] = 2
  } else if(cokerdata$temp_pref[i] == 'cold')
  {
    cokerdata$temp_pref[i] = 0
  }
}
cokerdata$temp_pref= as.numeric(cokerdata$temp_pref)

matchnum = 0
matches = c()
for ( i in 1:nrow(spnamedata))
{
  idx = which(cokerdata$sci == spnamedata$name[i])
  
  if(length(idx)==1)
  {
    if(is.na(spnamedata$thermal_pref[i]))
    {
      spnamedata$thermal_pref[i] = cokerdata$temp_pref[idx]      
      spnamedata$thermal_prefref[i] = 'coker'
    }
    matchnum = matchnum + 1
    matches = rbind(matches,cokerdata[idx,])
  }
  if(length(idx)>=2)
  {
    print('two matches!')
  }
}

#write.csv(spnamedata, 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv',row.names=FALSE)















#2. figure out which species doesn't have thermal pref and no tmin&tmax info.
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
nothpreftminmax = spnamedata$name[which(is.na(spnamedata$tmin)& is.na(spnamedata$tmax) & is.na(spnamedata$thermal_pref) & spnamedata$mussel_or_fish==1)]
nothpref = spnamedata$name[which(is.na(spnamedata$thermal_pref) & spnamedata$mussel_or_fish==1)]
notminmax = spnamedata$name[which(is.na(spnamedata$tmin)& is.na(spnamedata$tmax) & spnamedata$mussel_or_fish==1)]
length(nothpreftminmax)
length(nothpref)
length(notminmax)

fams = unique(spnamedata$family[which(spnamedata$mussel_or_fish==1)])
probfam  = c() # families where no species have thermal preference info.
for(i in 1:length(fams))
{
  th_pref_perfam = spnamedata$thermal_pref[which(spnamedata$family==fams[i] & spnamedata$mussel_or_fish==1)]
  if(length(th_pref_perfam)==sum(is.na(th_pref_perfam)))
  {
    probfam = c(probfam,fams[i])
  }
}
probsp = c() # species without tmin/tmax/thermal pref nor thermal pref of species within the same family
for(i in 1:length(nothpreftminmax))
{
  idx = which(spnamedata$name==nothpreftminmax[i])
  if(spnamedata$family[idx] %in% probfam)
  {
    probsp = rbind(probsp,spnamedata[idx,c('name','family','thermal_pref','thermal_prefref','tmin','tminref','tmax','tmaxref')])
  }
}
probsp$thermal_prefref
write.csv(probsp,'G:/My Drive/research/sdm_modeling/spdata/thermal data/species thermal classification.csv')








# 3. get all the temperature information from my own lit search in 'species thermal classification.csv'
# and estimate the thermal information of all the species using existing data. 
# Species that have thermal preference info, but not tmin or tmax will have average tmin or tmax 
# of its thermal prefernce class.
indi = read.csv('G:/My Drive/research/sdm_modeling/spdata/thermal data/species thermal classification.csv')
names(indi)
indi = indi[,c('name','family','thermal_pref','tmin','tmax','tmaxref')]
# get all the temperature information from my own lit search
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
for( i in 1:nrow(indi))
{
  sp = indi$name[i]
  idx = which(spnamedata$name == sp)
  if(!is.na(indi$tmin[i]))
  {
    spnamedata$tmin[idx] = indi$tmin[i]
  }
  if(!is.na(indi$tmax[i]))
  {
    spnamedata$tmax[idx] = indi$tmax[i]
  }
  if(!is.na(indi$tmax[i]))
  {
    spnamedata$tmax[idx] = indi$tmax[i]
  }
}
# get avg tmin and tmax for each thermal class
avgtmincold = spnamedata$tmin[which(spnamedata$thermal_pref==0)]
avgtminwarm = spnamedata$tmin[which(spnamedata$thermal_pref==1)]
avgtmincool = spnamedata$tmin[which(spnamedata$thermal_pref==2)]

avgtmaxcold = spnamedata$tmax[which(spnamedata$thermal_pref==0)]
avgtmaxwarm = spnamedata$tmax[which(spnamedata$thermal_pref==1)]
avgtmaxcool = spnamedata$tmax[which(spnamedata$thermal_pref==2)]

# assign thermal preference to species that lack thermal preference, tmin, and tmax data.

for( i in 1:nrow(spnamedata))
{
  if(is.na(spnamedata$thermal_pref))
  {
    if(is.na(tmin$))
  }
}




d



































#GARBAGE

# get their scientific name 
library(taxize)
sci = probsp
result  = sci2comm(sci, db = "ncbi", simplify = TRUE, scinames = NULL)

#result = cbind(names(unlist(result)),unlist(result))
result$`Percopsis omiscomaycus` = "Trout-perch"
result$`Ichthyomyzon unicuspis` = "Silver Lamprey"
result$`Herichthys cyanoguttatus` = "Texas Cichlid"
result$`Elassoma gilberti` = 'Gulf Coast pygmy sunfish' 
result$`Dormitator maculatus` = 'Fat sleeper'
result$`Ctenogobius boleosoma` = 'Darter Goby'
result$`Oreochromis aureus` = 'blue tilapia'
result$`Ichthyomyzon greeleyi`
result$`Ichthyomyzon bdellium`
result$`Herichthys cyanoguttatus`
result$`Cyprinodon salinus`
result$`Cyprinodon pecosensis`
result$`Cyprinodon macularius`
result$`Ctenogobius boleosoma`
result$`Cichlasoma bimaculatum`



result = unlist(result)
write.csv(cbind(sci,result),'G:/My Drive/research/sdm_modeling/spdata/thermal data/species thermal classification.csv')
d = read.csv('G:/My Drive/research/sdm_modeling/spdata/species thermal classifiction.csv')
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
for(i in 1:nrow(d))
{
  if(!is.na(d$thermal.pref[i]))
  {
    if(d$thermal.pref[i]=='cold')
    {
      val = 0 
    } else if (d$thermal.pref[i]=='cool')
    {
      val = 2
    } else 
    {
      val = 1
    }
    idx = which(spnamedata$name==d$sci.name[i])
    spnamedata$thermal_pref[idx] = val
    print(spnamedata$thermal_pref[idx])
  }
}
write.csv(spnamedata,'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv',row.names=FALSE)

library(rfishbase)
library(dplyr)
d = read.csv('G:/My Drive/research/sdm_modeling/spdata/species thermal classifiction.csv')
probsp2 = d$sci.name[which(is.na(d$thermal.pref))]
result = fb_tbl("species") %>% 
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% probsp2[1])






