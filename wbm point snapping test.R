# wbm point snapping test.R: method in 'extract_cellID.R' proved to be not accurate enough according to
# CUNY collaborators. Therefore, this is a test script to "snap" the locations to wbm cells as prescribed
# by the collaborators using occpts of aplodinotus grunniens.
library(nhdplusTools)
library(sf)
library(sp)

# example comid
filename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp/Aplodinotus grunniens_wcomid.csv'
spdata = read.csv(filename)
comids = spdata$comid #19442931
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = comids,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

length(comids)
length()
#plotting (optional)
#plot(subset$NHDFlowline_Network$geometry)
#plot(subset$CatchmentSP$geometry,add=TRUE)
#plot(subset$NHDWaterbody$geometry,add=TRUE)
#subset$CatchmentSP$areasqkm

area = subset$CatchmentSP$areasqkm
spdata$catchment_area = area
#write.csv(spdata,filename,row.names=FALSE)

ofile = data.frame(id=1:nrow(spdata),lat=spdata$decimalLatitude, lon=spdata$decimalLongitude, catchment_area=spdata$catchment_area)
readfilename = 'G:/My Drive/research/sdm_modeling/rgispy/examples/snap/real_life_locations_grunniens.csv'
write.csv(ofile,readfilename,row.names=FALSE)
readfilename2 = 'G:/My Drive/research/sdm_modeling/rgispy/examples/snap/real_life_locations_grunniens_test.csv'
ofile2 = ofile[1:10,]
write.csv(ofile2,readfilename2,row.names=FALSE)

par(mfrow=c(2,2))

readfilename3 = 'G:/My Drive/research/sdm_modeling/rgispy/examples/snap/snap_30sec_grunniens.csv'
d = read.csv(readfilename3)
length(which(d$is_naive=='True'))
length(d$is_naive)
hist(d$NetSymmetricDifference,main='tol5',ylim=c(0,550))
#hist(d$NetSymmetricDifference[which(d$is_naive=='True')])

readfilename4 = 'G:/My Drive/research/sdm_modeling/rgispy/examples/snap/snap_30sec_grunniens_outsideTol.csv'
d2 = read.csv(readfilename4)
length(which(d$is_naive=='True'))
length(d2$is_naive)
hist(d2$NetSymmetricDifference,main='outsidetol',ylim=c(0,550))
#hist(d2$NetSymmetricDifference[which(d2$is_naive=='True')])

readfilename5 = 'G:/My Drive/research/sdm_modeling/rgispy/examples/snap/snap_30sec_grunniens_tol10.csv'
d3 = read.csv(readfilename5)
length(which(d$is_naive=='True'))
length(d3$is_naive)
hist(d3$NetSymmetricDifference,main='tol10',ylim=c(0,550))
#hist(d3$NetSymmetricDifference[which(d3$is_naive=='True')])

readfilename6 = 'G:/My Drive/research/sdm_modeling/rgispy/examples/snap/snap_30sec_grunniens_tol15.csv'
d4 = read.csv(readfilename6)
length(which(d4$is_naive=='True'))
length(d4$is_naive)
hist(d4$NetSymmetricDifference,main='tol15',ylim=c(0,550))
#hist(d4$NetSymmetricDifference[which(d4$is_naive=='True')])

