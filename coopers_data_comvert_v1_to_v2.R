# convert cooper's data v1 comid's into v2 comids
library(data.table)
setwd('G:/My Drive/research/sdm_modeling/gis')
filename = 'G:/My Drive//research/sdm_modeling/gis/Dam_Metrics_NHDPlusV1_022317.csv'
cooper <- fread(filename, header = TRUE)
filename = 'G:/My Drive/research/sdm_modeling/gis/NHDv1_to_v2_crosswalk/v1v2crosswalk.csv'  
v1v2cross <- fread(filename, header = TRUE)
# what percentage of comids are in coopers data (87%)
cooper$COMIDV2 = v1v2cross$V2_ComID[match(cooper$COMID,v1v2cross$V1_ComID)] # 7148 reaches in cooper data is not matched (comes out as NA)
write_filename = 'G:/My Drive/research/sdm_modeling/gis/distance2dam_data.csv'
write.csv(cooper,write_filename)

#foo <-fread(write_filename,header=TRUE)