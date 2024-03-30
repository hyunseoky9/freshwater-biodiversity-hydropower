# categorize species into threatened and non-indigenous  
library(data.table)
setwd('F:/sdm_modeling/spdata')
sp_list = as.matrix(read.csv("sp_list2_occ_over30.csv"))[,2]
sp_list_full = read.csv("sp_list2.csv")[,2]
umn = read.csv('./umn mussel/sp_list_umn_mussel.csv')[,2]
fws = read.csv('./usgs_fish/sp_list_usgs_fish.csv')[,2]
thermal = read.csv('./thermal_tol/sp_list_thermal_tol.csv')[,2]
nonind = read.csv('./nonindigenous/sp_list_nonind.csv')[,2]
game = read.csv('./game_sp/sp_list_gamesp.csv')[,2]
nsD = read.csv('./natureserve/freshwater_fish_mussel.csv')
sort(unique(nsD$NatureServe.Rounded.Global.Rank))
underG4 = nsD$Scientific.Name[which(nsD$NatureServe.Rounded.Global.Rank %in% c('G3','G2','G1'))]
#data frame that puts species into threatened, invasive and game categories.
df = data.frame(species=sp_list,game=rep(0,length(sp_list)), threatened=rep(0,length(sp_list)), nonindigenous=rep(0,length(sp_list)))

#all fws listed sp are labeled threatened.
df$threatened[which(df$species %in% fws)] = 1
#all natureserve sp from G1-G3 are labeled threatened.
df$threatened[which(df$species %in% underG4)] = 1
#all nas listed sp are labeled non-indigenous
df$nonindigenous[which(df$species %in% nonind)] = 1
#all sp in game sp are labeled game
df$game[which(df$species %in% game)] = 1

write.csv(df,'over30occpts_sp_categorized2.csv')

