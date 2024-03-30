# make a new game species categorization based on Elman 1977
filename = 'G:/My Drive/research/sdm_modeling/spdata/Elman 1977 species list.csv'
d = read.csv(filename)

spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')

# new game species categorization.
spnamedata$game_Elman = 0
spnamedata$game_Elman[which(spnamedata$name %in% d$species)] = 1
sum(spnamedata$game_Elman)
# 50 species in my study species list categorized as game species from Elman 1977

ofilename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
write.csv(spnamedata,ofilename,row.names=FALSE)
