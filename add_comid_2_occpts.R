# script to get comids for all the occurrence points of a species.
#it takes 0.15s to get 1 comid on average (answer: 0.15s)
# resource for coding for calling comids without interruption: https://www.5axxw.com/questions/content/tzg2yg
library(nhdplusTools)
library(sf)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spnamedata = read.csv('./comprehensive_sp_info.csv')
spname_list = spnamedata$name

#problem_idx = read.csv('problematic_sp_idx.csv')[,2]
# get all the comids of the occ points for each sp.
nadatatable = data.frame(spname = rep('A',length(spname_list)),numNA = rep(NA,length(spname_list)))
for (j in 56) #length(spname_list)) # for each sp. #problem_idx[1:floor(length(problem_idx)/2)]) # problem_idx[(floor(length(problem_idx)/2)+2):length(problem_idx)]
{
  spname = spname_list[j]  
  filename = sprintf("G:/My Drive/research/sdm_modeling/spdata/per_sp/%s.csv",spname)
  #rawdata = read.csv(filename)
  data = read.csv(filename)
  if(ncol(data)==1)
  {
    data = read.csv(filename, sep="\t")
  }
  #rel_idx = which(!is.na(rawdata$year+rawdata$month+rawdata$day)) #only take occ points with year, month, and day information
  #data = rawdata[rel_idx,]
  occ_list = cbind(data$decimalLongitude,data$decimalLatitude)
  comid_list = c()
  ptm <- proc.time()
  
  for (i in 1:nrow(occ_list)) # for each occ pt get comid.
  {
    occ = occ_list[i,]
    start_point <- st_sfc(st_point(occ), crs = 4269)
    start_comid <- tryCatch(
      {
        withCallingHandlers(discover_nhdplus_id(point = start_point), message = function(c) if (inherits(c, "message")){stop("")})
        discover_nhdplus_id(point = start_point)
      },
      error = function(e) {
        return(NA)
      }
    )
    #start_comid <- discover_nhdplus_id(start_point)
    comid_list[i] = start_comid
    if(i%%100==0)
    {print(i)}
  }
  #output = rawdata
  output = data
  #comid_col = rep(NA,nrow(rawdata))
  comid_col = rep(NA,nrow(data))
  #comid_col[rel_idx] = comid_list
  comid_col = comid_list
  output$comid = comid_col
  write.csv(output,sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname))
  nadatatable$spname[j] = spname
  nadatatable$numNA[j] = sum(is.na(comid_list))
  proc.time() - ptm
  text = sprintf('%d/%d done',j,length(spname_list))
  print(text)
}
write.csv(nadatatable,'./per_sp/unidentified_comid_num_persp004_problem_idx2.csv')
















# find out why some occ pts have no data returned on their comid.
# is it because it's too far from any flowlines?

testdata = read.csv('./per_sp/Acipenser fulvescens_wcomid.csv')
nacomid_idx = which(is.na(testdata$comid)) # occ pts idx with no comid.
yescomid_idx = which(!is.na(testdata$comid))
idx = nacomid_idx[2]
pt = c(testdata$decimalLongitude[idx],testdata$decimalLatitude[idx])
for (i in 1:length(nacomid_idx))
{
  idx = nacomid_idx[i]
  pt = c(testdata$decimalLongitude[idx],testdata$decimalLatitude[idx])
  print(sprintf('%f,%f',pt[2],pt[1]))
}
for (i in 1:length(yescomid_idx))
{
  idx = yescomid_idx[i]
  pt = c(testdata$decimalLongitude[idx],testdata$decimalLatitude[idx])
  print(sprintf('%f,%f',pt[2],pt[1]))
}

# both occpts with comid and without comid are found near coasts.., what could be the difference btw these 2 occurrence pts
# that makes the nhdplustools to pick up and not pick up nearest reach?
# get upstream tributaries and contrast it with google map
yescomid_idx = which(!is.na(testdata$comid))
idx = yescomid_idx[2]
lat = 34.233522
long = -119.257713
pt = c(long,lat)
pt = c(  -80.888080,38.263727  )
pt = c(testdata$decimalLongitude[idx],testdata$decimalLatitude[idx])
sprintf('%f,%f',pt[2],pt[1])
start_point <- st_sfc(st_point(pt), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
flowline_nldi <- navigate_nldi(list(featureSource = "comid", 
                                    featureID = start_comid), 
                               mode = "DM", 
                               distance_km = 50)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline_nldi$DM$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

flowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

plot(sf::st_geometry(flowline), col = "blue")
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
idx = nacomid_idx[1]
pt = c(testdata$decimalLongitude[idx],testdata$decimalLatitude[idx])
random_point <- st_sfc(st_point(pt), crs = 4269)
plot(random_point, cex = 1.5, lwd = 2, col = "blue", add = TRUE)
plot(sf::st_geometry(flowline_nldi$origin), lwd = 3, col = "red", add = TRUE)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)


# scribbles
data = read.csv('./per_sp/Acantharchus pomotis.csv')
rel_idx = which(!is.na(data$year+data$month+data$day)) #only take occ points with year, month, and day information
data = data[rel_idx,]

occ_list = cbind(data$decimalLongitude,data$decimalLatitude)
i=2
occ = occ_list[i,]
occ2 = occ_list[2,]
start_point <- st_sfc(st_point(occ), crs = 4269)
start_point2 <- st_sfc(st_point(occ2),crs=4269)
start_comid <- discover_nhdplus_id(start_point)
start_comid2 <- discover_nhdplus_id(start_point2)
flowline_nldi <- navigate_nldi(list(featureSource = "comid", 
                                    featureID = start_comid), 
                               mode = "UT", 
                               distance_km = 1000)

flowline_nldi

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline_nldi$DM$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = TRUE,
                         return_data = TRUE, overwrite = TRUE)


flowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

plot(sf::st_geometry(flowline), col = "blue")
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(flowline_nldi$origin), lwd = 3, col = "red", add = TRUE)


get_flowline_index(flowline, start_point)

