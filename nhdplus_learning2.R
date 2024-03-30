# codes written for learning nhdplustools (especially snapping points to flowline) mostly following 
# https://usgs-r.github.io/nhdplusTools/articles/point_indexing.html#finding-multiple-indexes


library(nhdplusTools)
library(sf)
start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
c3376 = 13294138
start_comid <- discover_nhdplus_id(start_point)
start_comid <- c3376

start_comid = 22221781 # first.dam ex1
start_comid = 22220605 # term.comid ex1


nldimode = "DM"
flowline_pre <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = nldimode, 
                          distance_km = 50)
tributary = as.numeric(flowline_pre$UT_flowlines$nhdplus_comid)
nldimode = "UM"
flowline_pre <- navigate_nldi(list(featureSource = "comid", 
                                   featureID = start_comid), 
                              mode = nldimode, 
                              distance_km = 100)


trib= 13293564 #tributary[ceiling(runif(1,0,1)*length(tributary))]
nldimode = "DM"
flowline_pre <- navigate_nldi(list(featureSource = "comid", 
                                   featureID = trib), 
                              mode = nldimode, 
                              distance_km = 50)


subset_file <- tempfile(fileext = ".gpkg")
if(nldimode == 'UM')
{
  subset <- subset_nhdplus(comids = as.integer(flowline_pre$UM$nhdplus_comid),
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = TRUE,
                           return_data = TRUE, overwrite = TRUE)
  
} else if ( nldimode == 'UT')
{
  subset <- subset_nhdplus(comids = as.integer(flowline_pre$UT$nhdplus_comid),
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = TRUE,
                           return_data = TRUE, overwrite = TRUE)
} else if ( nldimode == "DM")
{
  subset <- subset_nhdplus(comids = as.integer(flowline_pre$DM$nhdplus_comid),
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = TRUE,
                           return_data = TRUE, overwrite = TRUE)
} else if (nldimode == "DT")
{
  subset <- subset_nhdplus(comids = as.integer(flowline_pre$DT$nhdplus_comid),
                           output_file = subset_file,
                           nhdplus_data = "download", 
                           flowline_only = TRUE,
                           return_data = TRUE, overwrite = TRUE)
}

flowline <- subset$NHDFlowline_Network
#catchment <- subset$CatchmentSP
#waterbody <- subset$NHDWaterbody

plot(sf::st_geometry(flowline), col = "red",add=TRUE)
plot(sf::st_geometry(flowline3), col = "blue")
plot(sf::st_geometry(flowline2), col = "red",add=TRUE)


dm = as.numeric(flowline_pre$DM_flowlines$nhdplus_comid)
um = as.numeric(flowline_pre0$UM_flowlines$nhdplus_comid)

# plot with comids on it
library(ggplot2)
library(pbapply)


midpoint <- function(start_comid)
{
  flowline <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = start_comid), 
                            mode = "DM", 
                            distance_km = 1)
  if(class(flowline$origin$geometry)=="NULL")
  {
    out <- c(NA,NA)
  } else {
    g <- st_geometry(st_as_sf(flowline$origin$geometry))
    
    g_mids <- lapply(g, function(x) {
      
      coords <- as.matrix(x)
      
      # this is just a copypaste of View(maptools:::getMidpoints):
      get_mids <- function (coords) {
        dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
        dist_mid <- sum(dist)/2
        dist_cum <- c(0, cumsum(dist))
        end_index <- which(dist_cum > dist_mid)[1]
        start_index <- end_index - 1
        start <- coords[start_index, ]
        end <- coords[end_index, ]
        dist_remaining <- dist_mid - dist_cum[start_index]
        mid <- start + (end - start) * (dist_remaining/dist[start_index])
        return(mid)
      }
      
      mids <- st_point(get_mids(coords))
    })
    
    out <- st_sfc(g_mids, crs = st_crs(st_as_sf(flowline$origin$geometry)))
    out <- st_sf(out)
  }
}
midpoints = pbapply(as.matrix(flowline$comid),1,midpoint)
midpoints2 = as.data.frame(t(matrix(unlist(midpoints),nrow=2)))
midpoints3 = cbind(midpoints2,flowline$comid)
names(midpoints3) = c('long','lat','comid')

map <- geom_sf(data= flowline, inherit.aes=FALSE,color="black",fill=NA)

#ggplot() + map + theme_void() + 
#  geom_text(data=midpoints3,aes(x=long,y=lat,label=comid),size=2) +
#  geom_sf(data=start_point,col='red')

ggplot() + map + theme_void() + 
  geom_text(data=midpoints3,aes(x=long,y=lat,label=comid),size=3)


plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)

# plot US map
library(sf)
dir = 'G:/My Drive/research/sdm_modeling/gis/2018_us_outline.shp'
shape <- read_sf(dir)
plot(sf::st_geometry(shape),col='red')

work_dir = 'F:/sdm_modeling/gis/nhdplustool_learning'
nhdplus_path(file.path(work_dir, "natseamless.gpkg"))
nhdplus_path()


UT_comids <- get_UT(flowline, start_comid)
UT_comids
output_file <- file.path(work_dir, "subset.gpkg")
output_file <-subset_nhdplus(comids = UT_comids,
                             output_file = output_file,
                             nhdplus_data = nhdplus_path(), 
                             return_data = FALSE, overwrite = TRUE)

