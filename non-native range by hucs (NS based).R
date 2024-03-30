# make non-indigenous_byhuc8_studyspecies_(from NS data).csv
# where each row is a huc8 code, and column is study species 
# and 1 means the species is non-native to that huc.

chrisd = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info_crd.csv')
exotic = gsub(' ','_',chrisd$name[which(is.na(chrisd$NSgName))])# 31 species no range data (includes zebra mussels)
chrisd2 = read.csv('G:/My Drive/research/sdm_modeling/spdata/NS_subsetFrom9505_pivoted.csv')
nasd_filename  = 'G:/My Drive/research/sdm_modeling/spdata/non-indigenous_byhuc8_studyspecies_(from NAS data).csv'
nasd = read.csv(nasd_filename)
d = nasd
d[,2:ncol(d)] = 0
sp = gsub('\\.','_',names(d))
sp = sp[2:length(sp)]
length(sp)
for( i in 1:length(sp))
{
  if(sp[i] %in% exotic)
  {
    d[,(i+1)] = 1
  } else {
    rowidx = 1:nrow(d)
    nativehucs = chrisd2$HUC8_WBD[which(!is.na(chrisd2[,which(colnames(chrisd2)==sp[i])]))]
    non_native_idx = which(is.na(match(d$HUC8,nativehucs)))
    d[rowidx[non_native_idx],(i+1)] = 1
    if(length(non_native_idx) != (nrow(d) - length(which(d[,(i+1)]==0))))
    {
      print('Errror: rownums dont match!')
      break
    }
  }
}

ofilename  = 'G:/My Drive/research/sdm_modeling/spdata/non-indigenous_byhuc8_studyspecies_(from NS data).csv'
write.csv(d,ofilename,row.names=FALSE)
