# get rid of all subspecies and genus data. (Already done on 12/31)
# genus data was rid 
# subspecies data with the comprising species data already were deleted
# subspecies data with no comprising species data were deleted. Then, I 
# downloaded the species data for that subspecies and changed the subspeceis name 
# in the source that contained that name into species name.
library('tidyverse')
setwd('F:/sdm_modeling/spdata/per_sp')
spfiles = list.files(path=".") # species files
newsplist = str_replace(spfiles, '.csv',"")
wordsplit = strsplit(newsplist,' ')
splist
newsplist[which(lapply(wordsplit,length)==3)][1:10]
newsplist[which(lapply(wordsplit,length)>3)]


#download species data for subspecies with no comprising species data. (done in gbif_download2.R)

#Dreissena rostriformis bugensis: do both rostriformis and bugensis (listed in nonind) (note: all bugensis data within rostriformis)
#Notropis simus pecosensis: (listed in usgs_fish)

# make species list per sources (usgs_fish, umn, thermal_tol, & nas
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

#thermal tol
thermal_tol_split = strsplit(thermal_tol_list,' ')
which(unlist(lapply(thermal_tol_split,length))!=2) #only genus? (A:none)
write.csv(data.frame(species_name=thermal_tol_list),'F:/sdm_modeling/spdata/thermal_tol/sp_list_thermal_tol.csv')

#usgs_fish
usgs_fish_split = strsplit(usgs_fish_list,' ' )
idx = usgs_fish_split[which(lapply(usgs_fish_split,length)!=2)]
usgs_fish = unique(unlist(lapply(usgs_fish_split,function(x) paste(x[1],x[2],sep=' '))))
write.csv(data.frame(species_name=usgs_fish),'F:/sdm_modeling/spdata/usgs_fish/sp_list_usgs_fish.csv')

#umn mussel
umn_mussel_split = strsplit(umn_mussel_list,' ')
which(unlist(lapply(umn_mussel_split,length))!=2)
write.csv(data.frame(species_name=umn_mussel_list),'F:/sdm_modeling/spdata/umn mussel/sp_list_umn_mussel.csv')

#nonind
nonind_fish_biv_split = strsplit(nonind_fish_biv_list,' ')
nonind_fish_biv_split[which(unlist(lapply(nonind_fish_biv_split,length))!=2)]
nonind = unique(unlist(lapply(nonind_fish_biv_split,function(x) paste(x[1],x[2],sep=' '))))
write.csv(data.frame(species_name=nonind),'F:/sdm_modeling/spdata/nonindigenous/sp_list_nonind.csv')

#gamesp
setwd('F:/sdm_modeling/spdata/game_sp')
gamesp = read.delim('game_species_list.csv', sep=',', comment.char = '#',header=T)
gamesp_split = strsplit(gamesp$scientific_name,' ')
which(unlist(lapply(gamesp_split,length))!=2)
write.csv(data.frame(species_name=gamesp$scientific_name),'F:/sdm_modeling/spdata/game_sp/sp_list_gamesp.csv')
