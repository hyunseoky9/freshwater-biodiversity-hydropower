# source: https://www.r-bloggers.com/2012/12/one-r-package-for-all-your-taxonomic-needs/
library(devtools)
#install.packages("ritis") # uncomment if not already installed
#install_github('taxize_', 'ropensci') # uncomment if not already installed ## not working
#install.packages("taxize", type="source") # uncomment if not already installed
library(ritis)
library(taxize)

# For one species
#tax_name(query = "Poa annua", get = "family")


# For many species
sp_list = read.csv('F:/sdm_modeling/spdata/sp_list2.csv')[,2]
famnames <- sapply(sp_list, tax_name, get = "family", USE.NAMES = F)

sp_list30 = read.csv('F:/sdm_modeling/spdata/sp_list2_occ_over30.csv')[,2]
idx = which(sp_list %in% sp_list30)
famnames30 = famnames[,idx]
df = data.frame(species_name=sp_list, family=unlist(famnames[3,]))
df30 = data.frame(species_name=sp_list30, family=unlist(famnames30[3,]))

write.csv(df,'F:/sdm_modeling/spdata/family_names.csv')
write.csv(df30,'F:/sdm_modeling/spdata/family_names_occ30.csv')

# getting genus
genus = unlist(lapply(strsplit(sp_list30,' '),function(x) x[1]))
