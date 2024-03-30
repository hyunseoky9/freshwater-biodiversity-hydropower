# download fish and mussel species occurrence data in US from GBIF.
# the code uses species list from 4 different sourcesa: fish sp. list from thermal tolerance csv,
# Mussel species list from UMN, usgs fish list, nonindigenous species list
# directory paths have to be changed. this project is no longer in google drive, 
# but in my external hard drive.

# source:https://modtools.wordpress.com/2021/03/22/import-and-clean-gbif-data/
rm(list = ls())
library(rgbif)
library(scrubr)
library(maps)

# retrieve sp name list data
{
thermal_tol_list = unique(c("Acipenser fulvescens","Alosa chrysochloris","Ameiurus melas","Ameiurus natalis","Ameiurus nebulosus","Amia calva","Aplodinotus grunniens","Campostoma anomalum","Campostoma oligolepis","Carpiodes carpio","Carpiodes cyprinus","Carpiodes velifer","Catostomus catostomus","Catostomus commersoni","Catostomus platyrhynchus","Cottus bairdii","Couseius plumbeus","Cottus carolinae","Culea inconstans","Cycleptus elongatus","Cyprinella lutrensis","Cyprinella spiloptera","Dorosoma cepedianum","Dorosoma petenese","Erimystax x-punctatus","Esox americanus","Etheostoma blennioides","Etheostoma caeruleum","Etheostoma chlorosoma","Etheostoma exile","Etheostoma flabellare","Etheostoma microperca","Etheostoma nianguae","Etheostoma nigrum","Etheostoma punctulatum","Etheostoma spectabile","Etheostoma zonale","Fundulus sciadicus","Gambusia affinis","Hiodon alosoides","Hybognathus argyritis","Hybognathus hankinsoni","Hybognathus nuchalis","Hybognathus placitus","Hypentelium nigricans","Ichthyomyzon castaneus","Ichthyomyzon fossor","Ictalurus furcatus","Ictalurus punctatus","Ictiobus bubalus","Ictiobus cyprinellus","Ictiobus niger","Labidesthes sicculus","Lepisosteus oculatus","Lepisosteus osseus","Lepisosteus platostomus","Lepomis cyanellus","Lepomis gibbosus","Lepomis gulosus","Lepomis humilis","Lepomis macrochirus","Lepomis megalotis","Lota lota","Luxilus chrysocephalus","Luxilus cornutus","Lythrurus umbratilis","Macrhybopsis aestivalis","Macrhybopsis gelida","Macrhybopsis meeki","Macrhybopsis storeriana","Margariscus margarita","Micropterus dolomieu","Micropterus punctulatus","Micropterus salmoides","Morone chrysops","Morone mississippiensis","Moxostoma carinatum","Moxostoma duquesnei","Moxostoma erythrurum","Moxostoma macrolepidotum","Nocomis biguttatus","Notemigonus crysoleucas","Notropis atherinoides","Notropis blennius","Notropis buchanani","Notropis dorsalis","Notropis greenei","Notropis nubilus","Notropis rubellus","Notropis shumardi","Notropis stramineus","Notropis topeka","Noturus exilis","Noturus flavus","Noturus gyrinus","Noturus nocturnus","Perca flavescens","Percina caprodes","Percina cymatotaenia","Percina evides","Percina maculata","Percina phoxocephala","Percopsis omiscomaycus","Phenacobius mirabilis","Phoxinus erythrogaster","Pimephales notatus","Pimephales promelas","Pimephales vigilax","Polyodon spathula","Pomoxis annularis","Pomoxis nigromaculatus","Prosopium williamsoni","Pylodictis olivaris","Rhinichthys atratulus","Richardsonius balteatus","Sander canadensis","Sander vitreum","Scaphirhynchus albus","Scaphirhynchus platorynchus","Semolitus atromaculatus","Thymallus arcticus"))

setwd('F:/sdm_modeling/spdata/umn mussel')
umn = read.csv('Moore_et_al_2021_Mussel_Lifehistory.csv')
umn_mussel_list = unique(paste(umn$genus,umn$species,sep=" "))

setwd('F:/sdm_modeling/spdata/usgs_fish')
usgs_fish = read.csv('species-listings-by-tax-group-report.csv')
usgs_fish_list = unique(usgs_fish$Scientific_name)


setwd('F:/sdm_modeling/spdata/nonindigenous')
nonind = read.csv('2021-07-15 Jager fishes and bivalves established list.csv')
nonind_fish_biv_list = unique(nonind$sci_name)

setwd('F:/sdm_modeling/spdata/game_sp')
gamesp = read.delim('game_species_list.csv', sep=',', comment.char = '#',header=T)
gamesp_list = gamesp$scientific_name[gamesp$nodata==1] # only get species that hasn't been downloaded from above lists.

}

datasetID = 5 #1=thermal 2=umn 3=usgs 4=nonindigenous 5=game_sp
if(datasetID==1)
{
  myspecies_list = thermal_tol_list
  dir = 'F:/sdm_modeling/spdata/thermal_tol'
} else if(datasetID==2){
  myspecies_list = umn_mussel_list
  dir = 'F:/sdm_modeling/spdata/umn mussel'
} else if(datasetID==3){
  dir = 'F:/sdm_modeling/spdata/usgs_fish'
  myspecies_list = usgs_fish_list
  
} else if(datasetID==4){
    dir = 'F:/sdm_modeling/spdata/nonindigenous'
    myspecies_list = nonind_fish_biv_list
} else if(datasetID==5){
    dir = 'F:/sdm_modeling/spdata/game_sp'
    myspecies_list = gamesp_list
}

# split list of sp into smaller bites to call from gbif more reliably
numintervals = floor(length(myspecies_list)/12) # divide species list such that each segment has about 12 - 23 sp. 
intervals = ceiling(c(0:numintervals)*length(myspecies_list)/numintervals)
intervals[1] = 1
no_occ_found = 0
noUncertaintydata = 0

if (datasetID==5)
{
  i=1
  myspecies = myspecies_list
  gotdata = 0
  while(!gotdata){ 
    result <- try(gbif_data <- occ_data(scientificName = myspecies, country='US', hasCoordinate = TRUE, limit = 100000))  
    if(class(result)!="try-error"){
      gotdata = 1      
    } else {
      sprintf('i=%d, error occurred',i)
    }
  }
  myspecies_pcount = c() # species with only positive number of occurrence points
  for(s in myspecies)
  {
    if(gbif_data[[s]]$meta$endOfRecords==FALSE)
    {
      print('not all records are retrieved')
    }
    if(gbif_data[[s]]$meta$count>0)
    {
      myspecies_pcount = c(myspecies_pcount,s)
    } else {
      no_occ_found = no_occ_found + 1
    }
  }
  sprintf('i=%d',i)
  #print(gbif_data)
  # create and fill a list with only the 'data' section for each species:
  
  myspecies_coords_list <- vector("list", length(myspecies_pcount))
  names(myspecies_coords_list) <- myspecies_pcount
  for (s in myspecies_pcount) {
    if(gbif_data[[s]]$meta$count>0){
      print(s)
      coords <- gbif_data[[s]]$data[ , c("decimalLongitude", "decimalLatitude", "occurrenceStatus","year","month","day","gbifID")]
      if(length(which(names(gbif_data[[s]]$data)=='coordinateUncertaintyInMeters'))==0)
      {
        noUncertaintydata = noUncertaintydata + 1
      }
      #coords <- gbif_data[[s]]$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references","gbifID")]
      myspecies_coords_list[[s]] <- data.frame(species = s, coords)
    }
  }
  lapply(myspecies_coords_list, head)
  
  # collapse the list into a data frame:
  nrownum = 0
  myspecies_coords = myspecies_coords_list[[1]]
  for(j in 2:length(myspecies_pcount))
  {
    nrownum = nrownum + nrow(myspecies_coords_list[[s]])
    myspecies_coords = rbind(myspecies_coords,myspecies_coords_list[[j]])
  }
  
  myspecies_coords <- as.data.frame(do.call(rbind, myspecies_coords_list))
  rownames(myspecies_coords) = c()
  head(myspecies_coords)
  tail(myspecies_coords)
  
  # let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
  nrow(myspecies_coords)
  myspecies_coords <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(myspecies_coords))))
  nrow(myspecies_coords)
  
  # also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 5 km (5000 m):
  #nrow(myspecies_coords)
  #unc = myspecies_coords$coordinateUncertaintyInMeters
  #myspecies_coords = myspecies_coords[which(is.na(unc) | unc<5000),]
  #nrow(myspecies_coords)
  
  setwd(dir)
  filename = sprintf("sp_occurrence_%d.csv",i)
  write.table(myspecies_coords, file = filename, sep = "\t",
              row.names = FALSE, col.names=TRUE)
}

if (datasetID!=5)
{
  for (i in 1:(length(intervals)-1))
  {
    if (i<(length(intervals)-1))
    {
      myspecies = myspecies_list[intervals[i]:(intervals[i+1]-1)]  
    } else
    {
      myspecies = myspecies_list[intervals[i]:(intervals[i+1])]
    }
    gotdata = 0
    # gbif calling
    while(!gotdata){ 
      result <- try(gbif_data <- occ_data(scientificName = myspecies, country='US', hasCoordinate = TRUE, limit = 100000))  
      if(class(result)!="try-error"){
        gotdata = 1      
      } else {
        sprintf('i=%d, error occurred',i)
      }
    }
    
    myspecies_pcount = c() # species with only positive number of occurrence points
    for(s in myspecies)
    {
      if(gbif_data[[s]]$meta$endOfRecords==FALSE)
      {
        print('not all records are retrieved')
      }
      if(gbif_data[[s]]$meta$count>0)
      {
        myspecies_pcount = c(myspecies_pcount,s)
      } else {
        no_occ_found = no_occ_found + 1
      }
    }
    sprintf('i=%d',i)
    #print(gbif_data)
    # create and fill a list with only the 'data' section for each species:
    
    myspecies_coords_list <- vector("list", length(myspecies_pcount))
    names(myspecies_coords_list) <- myspecies_pcount
    for (s in myspecies_pcount) {
      if(gbif_data[[s]]$meta$count>0){
        print(s)
        coords <- gbif_data[[s]]$data[ , c("decimalLongitude", "decimalLatitude", "occurrenceStatus","year","month","day","gbifID")]
        if(length(which(names(gbif_data[[s]]$data)=='coordinateUncertaintyInMeters'))==0)
        {
          noUncertaintydata = noUncertaintydata + 1
        }
        #coords <- gbif_data[[s]]$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references","gbifID")]
        myspecies_coords_list[[s]] <- data.frame(species = s, coords)
      }
    }
    lapply(myspecies_coords_list, head)
    
    # collapse the list into a data frame:
    nrownum = 0
    myspecies_coords = myspecies_coords_list[[1]]
    for(j in 2:length(myspecies_pcount))
    {
      nrownum = nrownum + nrow(myspecies_coords_list[[s]])
      myspecies_coords = rbind(myspecies_coords,myspecies_coords_list[[j]])
    }
    
    myspecies_coords <- as.data.frame(do.call(rbind, myspecies_coords_list))
    rownames(myspecies_coords) = c()
    head(myspecies_coords)
    tail(myspecies_coords)
    
    # let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
    nrow(myspecies_coords)
    myspecies_coords <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(myspecies_coords))))
    nrow(myspecies_coords)
    
    # also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 5 km (5000 m):
    #nrow(myspecies_coords)
    #unc = myspecies_coords$coordinateUncertaintyInMeters
    #myspecies_coords = myspecies_coords[which(is.na(unc) | unc<5000),]
    #nrow(myspecies_coords)
    
    setwd(dir)
    filename = sprintf("sp_occurrence_%d.csv",i)
    #write.table(myspecies_coords, file = filename, sep = "\t",
    #            row.names = FALSE, col.names=TRUE)
    sprintf('i=%d done',i)
  }
  sprintf('no occurrences found for %d species',no_occ_found)
}


 
 
 
# merge sp_occurrence_#.csv files
library(data.table)
if(datasetID==1){
  dir = 'F:/sdm_modeling/spdata/game_sp/thermal_tol'
  filename = 'sp_occurrence_thermal_tol.csv'
} else if(datasetID==2){
  dir = 'F:/sdm_modeling/spdata/game_sp/umn mussel'
  filename = 'sp_occurrence_umn_mussel.csv'
} else if(datasetID==3){
  dir = 'F:/sdm_modeling/spdata/game_sp/usgs_fish'
  filename = 'sp_occurrence_usgs_fish.csv'
} else if(datasetID==4){
  dir = 'F:/sdm_modeling/spdata/game_sp/nonindigenous'
  filename = 'sp_occurrence_nonind.csv'
} else if(datasetID==5){
  dir = 'F:/sdm_modeling/spdata/game_sp'
  filename = 'sp_occurrence_gamsp.csv'
}
setwd(dir)
data = fread('sp_occurrence_1.csv', sep = "\t", header = TRUE, na.strings = "\\N")
rownum = nrow(data)
for(i in 2:(length(intervals)-1))
{
  next_filename = sprintf('sp_occurrence_%d.csv',i)
  next_data = fread(next_filename, sep = "\t", header = TRUE, na.strings = "\\N")
  rownum = rownum + nrow(next_data)
  data = rbind(data,next_data)    
}
if (rownum != nrow(data))
{
  print('somethings wrong')
}

write.table(data, file = filename, sep = "\t",
            row.names = FALSE, col.names=TRUE)


#gbif_citation(gbif_data) # for citation

#names(gbif_data$data)
#names(gbif_data[[myspecies[1]]])
#names(gbif_data[[myspecies[1]]]$meta)
#names(gbif_data[[myspecies[1]]]$data)




#---- cleaning ----
# here we'll first remove records of absence or zero-abundance (if any):
#names(myspecies_coords)
#sort(unique(myspecies_coords$individualCount))  # notice if some points correspond to zero abundance
#sort(unique(myspecies_coords$occurrenceStatus))  # check for different indications of "absent", which could be in different languages! and remember that R is case-sensitive
#absence_rows <- which(myspecies_coords$individualCount == 0 | myspecies_coords$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
#length(absence_rows)
#if (length(absence_rows) > 0) {
#  myspecies_coords <- myspecies_coords[-absence_rows, ]
#}

