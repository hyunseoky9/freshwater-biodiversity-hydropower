#- see which species that are said to be threatened are actually in the listed species list.
#- see which of the species in my list that are listed have all of their populations protected
#- find a good way to get state-listed species for all states in CONUS.
ofilename = 'G:/My Drive/research/sdm_modeling/spdata/Listed Animals.csv'
d=read.csv(ofilename)
head(d)
d = d[which(d$Species.Group %in% c('Fishes','Clams')),]
d = d[which(d$Federal.Listing.Status %in% c('Endangered','Threatened')),]
# get rid of parenthesis stuff in a species name (already done)
#d$Scientific.Name[which(grepl('\\(',d$Scientific.Name))] 
#d$Scientific.Name[which(grepl('\\(',d$Scientific.Name))] = trimws(gsub('\\([^)]+\\)','',d$Scientific.Name[which(grepl('\\(',d$Scientific.Name))]))
#d$Scientific.Name[which(grepl('  ',d$Scientific.Name))] = gsub('  ',' ',d$Scientific.Name[which(grepl('  ',d$Scientific.Name))])

d$subspecies = rep(0,nrow(d))
d$subspecies[which(sapply(strsplit(d$Scientific.Name,' '),length)>2)] = 1

#find number of species (n=238)
#d$Scientific.Name[which(sapply(strsplit(d$Scientific.Name,' '),length)>2)]
#d$Scientific.Name.Species = sapply(strsplit(d$Scientific.Name,' '),function(x) paste(x[1],x[2]))
listedsp = unique(d$Scientific.Name.Species)
length(unique(d$Scientific.Name.Species))
length(d$Scientific.Name)

# find number of species with partial listing (endangered OR threatened)
# species that have wherever in a non-subspecies level.
everywheresp = d$Scientific.Name[which(d$subspecies==0)][grepl('Wherever found',d$Where.Listed[which(d$subspecies==0)])]
partialsp = unique(d$Scientific.Name.Species[which(!d$Scientific.Name.Species %in% everywheresp)])
length(partialsp) # 37 species that are only partially protected.
# 3/37 are clams (34/37 are fish)
length(unique(d$Scientific.Name.Species[which(d$Scientific.Name.Species %in% partialsp & d$Species.Group=='Clams')])) # 3/37 are clams (34/37 are fish)
length(unique(d$Scientific.Name.Species[which(d$Scientific.Name.Species %in% partialsp & d$Species.Group=='Fishes')])) 


spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
spnamedata2 = spnamedata[idx0,]
dim(spnamedata2)

# write.csv(d,ofilename,row.names=FALSE)

# species idx that are listed in my study sp. (n=21)
listed_studysp = spnamedata2$name[which(apply(as.matrix(spnamedata2$name),1,function(x) x %in% listedsp))]
# species idx that are partially listed in my study sp. (n=14)
partial_studysp = spnamedata2$name[which(apply(as.matrix(spnamedata2$name),1,function(x) x %in% partialsp))]


ofilename = 'G:/My Drive/research/sdm_modeling/spdata/partially_listed_sp.csv'
write.csv(d[which(d$Scientific.Name.Species %in% partial_studysp),c('Scientific.Name','Where.Listed')],ofilename,row.names=FALSE)


sum(spnamedata2$threatened)

