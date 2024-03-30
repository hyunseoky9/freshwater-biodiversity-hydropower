# code written for learning how to navigate through nhdplusTools (mostly with ryan peek tutorial which doesn't 
# exist anymore. page recovered by wayback machine saved in sdm_project/important emails from Yetta/Snapping Points to Lines & Calculating Streamline Distances.pdf)

install.packages("nhdplusTools")
library(nhdplusTools)
library(sf)
start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)


setwd('C:/Users/hy324/Google_Drive/research/SDM modeling/gis/dummy')
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = start_comid,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

flowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

plot(sf::st_geometry(flowline), col = "blue")
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)






#ryanpeek tutorial
#https://ryanpeek.org/mapping-in-R-workshop/03_vig_snapping_points_to_line.html
library(sf) # spatial operations
library(mapview) # html mapping
library(leaflet) # html mapping
library(ggplot2) # plotting
library(dplyr) # wrangling data
library(here) # setting directories safely
library(viridis) # color scheme
library(USAboundaries) # county/state boundaries
library(nhdplusTools) # USGS/NHD rivers data
library(rgdal)



# create a point in yosemite valley
yose <- st_sfc(st_point(c(-119.60020, 37.73787)), crs = 4326)

# check class is "sfc" and "sfc_POINT"
class(yose)
yose_comid <- discover_nhdplus_id(yose)
yose_list <- list(featureSource="comid", featureID=yose_comid)
#below line doesn't exist anymore in nhdPlusTools
#discover_nldi_navigation(yose_list)

# get upstream flowlines
yose_us_flowlines <- navigate_nldi(nldi_feature = yose_list,
                                   mode="UT",
                                   distance_km = 5000,
                                   data_source = "flowline")

yose_start <- navigate_nldi(nldi_feature = yose_list,
                                   mode="UT",
                                   distance_km = 1,
                                   data_source = "flowline")


plot(yose_us_flowlines$UM_flowlines$geometry, col = "steelblue")
# get downstream mainstem only (from our starting segment):
yose_ds_flowlines <- navigate_nldi(nldi_feature = yose_list, 
                                   mode = "DM", 
                                   distance_km = 5000,
                                   data_source = "")

# make a list of all the comids we've identified:
all_comids <- c(yose_us_flowlines$UT_flowlines$nhdplus_comid, yose_ds_flowlines$DM_flowlines$nhdplus_comid)



# download all data and create a geopackage with the comid list
yose_gpkg <- subset_nhdplus(comids=all_comids,
                            simplified = TRUE,
                            overwrite = TRUE,
                            output_file = paste0(getwd(), "yose_nhdplus.gpkg"),
                            nhdplus_data = "download",
                            return_data = FALSE)



# check layers in database:

st_layers(paste0(getwd(), "yose_nhdplus.gpkg"))


# pull the flowlines back in
yose_streams <- read_sf(paste0(getwd(), "yose_nhdplus.gpkg"), "NHDFlowline_Network")




prettymapr::prettymap({
  rosm::osm.plot(project = FALSE, 
                 bbox = matrix(st_bbox(yose_streams), byrow = FALSE, ncol = 2, 
                               dimnames = list(c("x", "y"), c("min", "max"))), 
                 type = "cartolight", quiet = TRUE, progress = "none")
  plot(yose_streams$geom, col = "steelblue", lwd = (yose_streams$streamorde / 4), add=TRUE)
  plot(yose, add=TRUE, pch=21, bg="orange", cex=1.5)
  prettymapr::addnortharrow()
})


# find upstream gages
yose_us_gages <- navigate_nldi(yose_list,
                               mode = "UT",
                               distance_km = 5000,
                               data_source = "nwissite")

# get downstream everything from our only upstream gage (Happy Isles)
usgs_point <- list(featureSource="nwissite", featureID = "USGS-11264500")

# find all downstream gages on the mainstem river (Merced/San Joaquin)
yose_ds_gages <- navigate_nldi(yose_list,
                               mode = "DM",
                               distance_km = 5000,
                               data_source = "nwissite")


# let's add these data to our geopackage as well

#reproject gage data
yose_us_gages$UT_nwissite = st_transform(yose_us_gages$UT_nwissite,4269)
yose_ds_gages$DM_nwissite = st_transform(yose_ds_gages$DM_nwissite,4269)

# remember it's best to have everything in the same projection
st_crs(yose_streams)==st_crs(yose_us_gages$UT_nwissite)
st_crs(yose_streams)==st_crs(yose_ds_gages$DM_nwissite)


# write to geopackage: overwite the layer if it exists
st_write(yose_us_gages$UT_nwissite, dsn=paste0(getwd(),"yose_nhdplus.gpkg"), 
         layer="yose_us_gages", append = FALSE, delete_layer = TRUE)

st_write(yose_ds_gages$DM_nwissite, dsn=paste0(getwd(),"yose_nhdplus.gpkg"), 
         layer="yose_ds_gages", append = FALSE, delete_layer = TRUE)


st_layers(paste0(getwd(), "yose_nhdplus.gpkg"))

# plot gages and streams
rosm::osm.plot(project = FALSE, 
               bbox = matrix(st_bbox(yose_streams), byrow = FALSE, ncol = 2, 
                             dimnames = list(c("x", "y"), c("min", "max"))), 
               type = "cartolight", quiet = TRUE, progress = "none")
plot(yose_streams$geom, col = factor(yose_streams$streamorde), lwd = (yose_streams$streamorde / 4), add=TRUE)
plot(yose, add=TRUE, pch=21, bg="black", cex=1.5)
plot(yose_us_gages$UT_nwissite, add=TRUE, pch=21, bg="orange", cex=1.5)
plot(yose_ds_gages$DM_nwissite, add=TRUE, pch=21, bg="maroon", cex=1.5)


# first lets merge all our gages into one dataframe. Make sure in same crs
st_crs(yose_us_gages$UT_nwissite)==st_crs(yose_ds_gages$DM_nwissite)
# now bind together
all_gages <- rbind(yose_us_gages$UT_nwissite, yose_ds_gages$DM_nwissite) 

st_crs(yose_us_gages$UT_nwissite)==st_crs(gages_snapped)
# check if there are duplicates and get rid
all_gages = all_gages[-which(duplicated(all_gages$identifier)==TRUE),]

# first project
all_gages_proj <- st_transform(all_gages, crs = 26910)
yose_streams_proj <- st_transform(yose_streams, crs=26910)

# now snap points to the lines using a 500 meter buffer, select which ID column you want keep for rejoining
gages_snapped <- st_snap_points(all_gages_proj, yose_streams_proj, namevar = "identifier", max_dist = 500)


#gages_snapped = gages_snapped[c(1,nrow(gages_snapped)),]


library(lwgeom)
# create a 1 meter buffer around snapped point
gages_snapped_buff <- st_buffer(gages_snapped, 1)


#plot snapped gages
plot(yose_streams_proj$geom, col = factor(yose_streams$streamorde), lwd = (yose_streams$streamorde / 4))



plot(gages_snapped$geometry[c(1,length(gages_snapped$geometry))], pch=21, bg="orange", cex=1.5, add=TRUE)


# now use lwgeom::st_split to split stream segments
segs <- st_collection_extract(lwgeom::st_split(yose_streams_proj, gages_snapped_buff), "LINESTRING") %>% 
  tibble::rownames_to_column(var = "rowid") %>% 
  mutate(rowid=as.integer(rowid))

# filter to only the mainstem Merced or San Joaquin 
segs_filt <- segs %>% filter(gnis_name %in% c("Merced River", "San Joaquin River") |
                               comid %in% c(21609445, 21609461))


segs_filt_dist <- segs_filt %>% 
  # drop the "loose ends" on either extent (upstream or downstream) of first/last gage
  mutate(seg_len_m = units::drop_units(units::set_units(st_length(.), "m")),
         seg_len_km = seg_len_m/1000) %>% 
  arrange(desc(hydroseq)) %>% 
  mutate(total_len_km = cumsum(seg_len_km)) %>% 
  # filter to just cols of interest
  select(rowid, comid, reachcode, streamorde, hydroseq, seg_len_km, total_len_km, geom)
plot(segs_filt_dist$geom, lwd = (segs_filt_dist$streamorde / 4))

plot(segs_filt$geom, lwd = (segs_filt$streamorde / 4))

gages_snapped_usds <- filter(gages_snapped, identifier %in% c("USGS-11337190", "USGS-11264500"))



plot(yose_streams_proj$geom, col = factor(yose_streams$streamorde), lwd = (yose_streams$streamorde / 4))

plot(gages_snapped$geometry, pch=21, bg="orange", cex=1.5, add=TRUE)
plot(gages_snapped_usds$geometry, pch=21, bg="black", cex=1.5, add=TRUE)


  






st_snap_points <- function(x, y, namevar, max_dist = 1000) {
  
  # this evaluates the length of the data
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  # this part: 
  # 1. loops through every piece of data (every point)
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>% 
    mutate({{namevar}} := x[[namevar]]) %>% 
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)
  
  return(out_xy)
}




