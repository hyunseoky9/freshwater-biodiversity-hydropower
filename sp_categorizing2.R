# categorize species into threatened and non-indigenous  
library(data.table)
setwd('F:/sdm_modeling/spdata')
sp_list = as.matrix(read.csv("sp_list2_occ_over30.csv"))[,2]
filestr = c('./umn mussel/sp_occurrence_umn_mussel.csv','./usgs_fish/sp_occurrence_usgs_fish.csv','./thermal_tol/sp_occurrence_thermal_tol.csv','./nonindigenous/sp_occurrence_nonind.csv')
spnum_perdat = c()
gamesp_list = read.delim('./game_sp/game_species_list.csv', sep=',', comment.char = '#',header=T)
NS = read.csv('./natureserve/freshwater_fish_mussel.csv')
NSsp_list = NS$Scientific.Name
sp  = list() # sp_list from each source
#data frame that puts species into threatened, invasive and game categories.
df = data.frame(species=sp_list,game=rep(0,length(sp_list)), threatened=rep(0,length(sp_list)), nonindigenous=rep(0,length(sp_list)))
# pull in species list from each source
for(i in 1:length(filestr))
{
  data = fread(filestr[i], sep = "\t", header = TRUE, na.strings = "\\N")
  sp[[i]] = unique(data$species)
  spnum_perdat[i] = length(unique(data$species))
}

#all fws listed sp are labeled threatened.
#turn all the species name into 2 words 
fwssplit = strsplit(sp[[2]],' ')
df$threatened[which(df$species %in% sp[[2]])] = 1
#all sp with >G? are labeled threatened.
#all nas listed sp are labeled non-indigenous
df$nonindigenous[which(df$species %in% sp[[4]])] = 1
#categorize sp into game sp.
sp2words = c() # get rid of subspecies name in the sp list
for (i in 1:length(df$species))
{
  splits = strsplit(df$species[i]," ")[[1]][1:2]
  sp2words[i] = paste(splits[1],splits[2])
}
df$game[which(sp2words %in% gamesp_list$scientific_name)] = 1

write.csv(df,'over30occpts_sp_categorized.csv')

