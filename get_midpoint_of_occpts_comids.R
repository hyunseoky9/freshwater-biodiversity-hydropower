#get midpoints of the comids of the occurrence points
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

library(sf)
library(sp)
library(maptools)
library(nhdplusTools)
library(pbapply)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spname_list = spnamedata$name

for( i in 321:length(spname_list))
{
  ifilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname_list[i])
  d = read.csv(ifilename)
  d$sampleptlat = d$decimalLatitude
  d$sampleptlong = d$decimalLongitude
  comids = d$comid
  # get the midpoints using apply.
  coords = pbapply(as.matrix(comids),1,midpoint)
  coords = as.data.frame(t(matrix(unlist(coords),nrow=2)))
  d$decimalLongitude = coords[,1]
  d$decimalLatitude = coords[,2]
  #points = sfheaders::sf_point(coords)
  
  ofilename = ifilename
  write.csv(d,ofilename,row.names=FALSE)
  print(sprintf('%d/%d done',i,length(spname_list)))
}
