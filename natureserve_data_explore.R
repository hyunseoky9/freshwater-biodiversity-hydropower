# cleaning natureserve fish and mussel species list data (remove weird ones and check which sp.' occ points are already downloaded)
setwd('F:/sdm_modeling/spdata')
fishD = read.csv('./natureserve/freshwater_fish_US.csv')
musselD = read.csv('./natureserve/freshwater_mussel_US.csv')

#delete all the species in the csv with status GX and GH (also in the species occ data)
fishmussel = read.csv('./natureserve/freshwater_fish_mussel - Copy.csv')
occfilenames =  fishmussel$Scientific.Name[which(fishmussel$NatureServe.Rounded.Global.Rank %in% c('GX','GH'))]
occfilenames2delete = paste(occfilenames,'.csv',sep='')
spfiles = list.files(path="F:/sdm_modeling/spdata/per_sp") # species files
setwd('F:/sdm_modeling/spdata/per_sp')
unlink(occfilenames2delete)
fishmussel_out = fishmussel[-which(fishmussel$NatureServe.Rounded.Global.Rank %in% c('GX','GH')),]
write.csv(fishmussel_out,'./natureserve/freshwater_fish_mussel.csv')

# check if all the species names are 2 words (no  subsp. bs)
fish = fishD$Scientific.Name
fishsplit = strsplit(fish,' ')
fish[which(unlist(lapply(fishsplit,length))!=2)] # which species have more than 2 words? (A: there's 1 and its a hybrid sp)
## get rid of it in the csv file (dido)
mussel = musselD$Scientific.Name
musselsplit = strsplit(mussel,' ')
mussel[which(unlist(lapply(musselsplit,length))!=2)] # which species have more than 2 words? (A: none)

# check which species are already downloaded
splist = read.csv("sp_list.csv")
splist = splist$species_name
splist_split = strsplit(splist,' ')
splist_2words = lapply(splist_split,function(x) paste(x[1], x[2], sep=" ")) # species list into 2 words (gets rid of subsp and other bs)
fish_downloaded_idx = which(fish %in% splist_2words) # indexes of fish species whose occ data are already downloaded
mussel_downloaded_idx = which(mussel %in% splist_2words) # same thing for mussel sp.

# check if all the sp from other sources (umn,usgs, etc.) are in the natureserve list
fishmussel = c(fish,mussel)
length(which(splist_2words %in% fishmussel)) == length(splist_2words) # #sp in splist in NS != #sp in splist
notinNS = splist_2words[-which(splist_2words %in% fishmussel)] # list of sp. not in Natureserve (length: 48)
# how many sp. that are not in NS are from umn? (A: 4)
setwd('F:/sdm_modeling/spdata/umn mussel')
umn = read.csv('Moore_et_al_2021_Mussel_Lifehistory.csv')
{umn_mussel_list = unique(paste(umn$genus,umn$species,sep=" "))
Reduce('+', lapply(strsplit(umn_mussel_list,' '),length))/2
} # check all the species name in umn only has 2 words (A: all sp name has only 2 words)
length(which(notinNS %in% umn_mussel_list)) # #umn sp in notinNS: 4
umn_mussel_list[which(notinNS %in% umn_mussel_list)]
# how many sp. that are not in NS are from thermal tol? (A: )
thermal_tol_list = unique(c("Acipenser fulvescens","Alosa chrysochloris","Ameiurus melas","Ameiurus natalis","Ameiurus nebulosus","Amia calva","Aplodinotus grunniens","Campostoma anomalum","Campostoma oligolepis","Carpiodes carpio","Carpiodes cyprinus","Carpiodes velifer","Catostomus catostomus","Catostomus commersoni","Catostomus platyrhynchus","Cottus bairdii","Couseius plumbeus","Cottus carolinae","Culea inconstans","Cycleptus elongatus","Cyprinella lutrensis","Cyprinella spiloptera","Dorosoma cepedianum","Dorosoma petenese","Erimystax x-punctatus","Esox americanus","Etheostoma blennioides","Etheostoma caeruleum","Etheostoma chlorosoma","Etheostoma exile","Etheostoma flabellare","Etheostoma microperca","Etheostoma nianguae","Etheostoma nigrum","Etheostoma punctulatum","Etheostoma spectabile","Etheostoma zonale","Fundulus sciadicus","Gambusia affinis","Hiodon alosoides","Hybognathus argyritis","Hybognathus hankinsoni","Hybognathus nuchalis","Hybognathus placitus","Hypentelium nigricans","Ichthyomyzon castaneus","Ichthyomyzon fossor","Ictalurus furcatus","Ictalurus punctatus","Ictiobus bubalus","Ictiobus cyprinellus","Ictiobus niger","Labidesthes sicculus","Lepisosteus oculatus","Lepisosteus osseus","Lepisosteus platostomus","Lepomis cyanellus","Lepomis gibbosus","Lepomis gulosus","Lepomis humilis","Lepomis macrochirus","Lepomis megalotis","Lota lota","Luxilus chrysocephalus","Luxilus cornutus","Lythrurus umbratilis","Macrhybopsis aestivalis","Macrhybopsis gelida","Macrhybopsis meeki","Macrhybopsis storeriana","Margariscus margarita","Micropterus dolomieu","Micropterus punctulatus","Micropterus salmoides","Morone chrysops","Morone mississippiensis","Moxostoma carinatum","Moxostoma duquesnei","Moxostoma erythrurum","Moxostoma macrolepidotum","Nocomis biguttatus","Notemigonus crysoleucas","Notropis atherinoides","Notropis blennius","Notropis buchanani","Notropis dorsalis","Notropis greenei","Notropis nubilus","Notropis rubellus","Notropis shumardi","Notropis stramineus","Notropis topeka","Noturus exilis","Noturus flavus","Noturus gyrinus","Noturus nocturnus","Perca flavescens","Percina caprodes","Percina cymatotaenia","Percina evides","Percina maculata","Percina phoxocephala","Percopsis omiscomaycus","Phenacobius mirabilis","Phoxinus erythrogaster","Pimephales notatus","Pimephales promelas","Pimephales vigilax","Polyodon spathula","Pomoxis annularis","Pomoxis nigromaculatus","Prosopium williamsoni","Pylodictis olivaris","Rhinichthys atratulus","Richardsonius balteatus","Sander canadensis","Sander vitreum","Scaphirhynchus albus","Scaphirhynchus platorynchus","Semolitus atromaculatus","Thymallus arcticus"))  
thermal_tol_list[which(unlist(lapply(strsplit(thermal_tol_list,' '),length))!=2)] # all the sp in thermal tol list has 2 words
length(which(notinNS %in% thermal_tol_list)) # #thermal_tol_list sp in notinNS: 1
# how many sp. that are not in NS are from fws listed sp?
setwd('F:/sdm_modeling/spdata/usgs_fish')
usgs_fish = read.csv('species-listings-by-tax-group-report.csv')
usgs_fish_list = unique(usgs_fish$Scientific_name)
usgs_fish_list[which(unlist(lapply(strsplit(usgs_fish_list,' '),length))!=2)] # check all the sp. has 2 words (A: 21 have more than 2 words)
usgs_fish_list = lapply(strsplit(usgs_fish_list,' '), function(x) paste(x[1],x[2],sep=' '))
length(which(notinNS %in% usgs_fish_list)) # #fws_listed sp. in notinNS: 5
unique(notinNS[which(notinNS %in% usgs_fish_list)])
# how many sp. that are not in NS are from usgs nas list fish&bivalve list?
setwd('F:/sdm_modeling/spdata/nonindigenous')
nonind = read.csv('2021-07-15 Jager fishes and bivalves established list.csv')
nonind_fish_biv_list = unique(nonind$sci_name)s
nonind_fish_biv_list[which(unlist(lapply(strsplit(nonind_fish_biv_list,' '),length))!=2)]
nonind_fish_biv_list[which(unlist(lapply(strsplit(nonind_fish_biv_list,' '),length))!=2)[1]] = 'Lepomis xxx'
nonind_fish_biv_list = lapply(strsplit(nonind_fish_biv_list,' '), function(x) paste(x[1],x[2],sep=' '))
length(which(notinNS %in% nonind_fish_biv_list)) # #fws_listed sp. in notinNS: 5
unlist(notinNS[which(notinNS %in% nonind_fish_biv_list)])
