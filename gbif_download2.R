# download gbif occ pt data from natureserve sp. list. Duplicate species from gbif_download.R are omitted.
rm(list = ls())
library(rgbif)
library(scrubr)
library(maps)

# retrieve sp name list data
{
  setwd('F:/sdm_modeling/spdata')
  fish = read.csv('./natureserve/freshwater_fish_US.csv')
  mussel = read.csv('./natureserve/freshwater_mussel_US.csv')
  fishmussel = c(fish$Scientific.Name, mussel$Scientific.Name)
}

#omit sp whose occ data are already downloaded
splist = read.csv("sp_list.csv")
splist = splist$species_name
splist_split = strsplit(splist,' ')
splist_2words = lapply(splist_split,function(x) paste(x[1], x[2], sep=" ")) # species list into 2 words (gets rid of subsp and other bs)
already_downloaded_idx = which(fishmussel %in% splist_2words) # indexes of fish species whose occ data are already downloaded


dir = 'F:/sdm_modeling/spdata/per_sp'
myspecies_list = fishmussel[-already_downloaded_idx] # list of species whose occ pts should be downloaded. Omits already downloaded sp.
#myspecies_list = c('Dreissena rostriformis','Dreissena bugensis', 'Notropis simus') # used for downloading species for subsp without sp data. (refer to ridding_subspeciesNgenus.R)

startfrom = 1 # if you want to start downloading from the middle of the list
lifestage =c()
for (i in startfrom:length(myspecies_list))
{
  s = myspecies_list[i]
  gotdata = 0
  # gbif calling
  while(!gotdata){ 
    result <- try(gbif_data <- occ_data(scientificName = s, country='US', hasCoordinate = TRUE, limit = 100000))  
    if(class(result)!="try-error"){
      gotdata = 1      
    } else {
      sprintf('i=%d, error occurred',i)
    }
  }
  print(length(which(!is.na(gbif_data$data$lifeStage))))
  lifestage[i] = length(which(!is.na(gbif_data$data$lifeStage)))
  #if(gbif_data$meta$endOfRecords==FALSE)
  #{
  #  print('not all records are retrieved')
  #}
  #if(gbif_data$meta$count>0 && !is.null(gbif_data$data$year) && !is.null(gbif_data$data$month) && !is.null(gbif_data$data$day) &&!is.null(gbif_data$data$gbifID)) # if a sp has >0 count, make the data into csv.
  #{
  #  coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "occurrenceStatus","year","month","day","gbifID")]
  #  myspecies_coords = data.frame(species = s, coords)
  #  filename = sprintf('%s/%s.csv',dir,s)
  #  write.csv(myspecies_coords, filename)
  #}
  
  # print out finished species
  msg = sprintf('%s done', s)
  print(msg)
}

# make a new species list
library('tidyverse')
setwd('F:/sdm_modeling/spdata/per_sp')
spfiles = list.files(path=".") # species files
newsplist = str_replace(spfiles, '.csv',"")
write.csv(data.frame(species_name=newsplist),'../sp_list2.csv')


splist2 = read.csv('../sp_list2.csv')
splist2 = splist2$species
length(splist2)


i=18
d[1:5,(7*i+1):(7*(i+1))]
