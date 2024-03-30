filename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
projd = read.csv(filename)
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/measure data.csv'
md = read.csv(filename2)
md$dollar.yr
projd$dollar_yr = NA
projd$dollar_yr = md$dollar.yr[match(projd$documentID,md$ferc.docket)]


filename3 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ORNL_Mitigation_Database/ORNL_Mitigation_Database.csv'
ornl_db = read.csv(filename3)
projd$huc8 = NA
projd$huc8
projid = projd$FERC_project_No._as_stated_in_the_document
projid2 = paste0('P-',trimws(gsub('-[0-9]*','',projid)))
ornl_db = ornl_db[which(!duplicated(ornl_db$FC_Dock)),]
projd$huc8 = ornl_db$HUC08[match(projid2, ornl_db$FC_Dock)]
sum(is.na(projd$huc8))
sum(is.na(projd$FERC_project_No._as_stated_in_the_document))
ornl_db$FC_Dock
projid[69]
  match(projd$FERC_project_No._as_stated_in_the_document)

#write.csv(projd,filename,row.names=FALSE)

# turn coordinate strings into arrays
library(sf)
huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')

coordinate_idx = which(!is.na(projd$coordinate))
for( i in coordinate_idx)
{
  coordinates = projd$coordinate[i]
  coordinates = unlist(strsplit(coordinates,';'))
  lats = c()
  longs = c()
  for(j in 1:length(coordinates))
  {
    coorinates_split = as.numeric(unlist(strsplit(coordinates[j],', ')))
    lats = c(lats,coorinates_split[1])
    longs = c(longs, coorinates_split[2])
    coordinates_num = cbind(longs,lats)
  }
  coords = as.data.frame(coordinates_num)
  names(coords) = c('longitude','latitude')
  points_sf <- st_as_sf(coords, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326, 
                        agr = "constant")
  points_sf = st_transform(points_sf,st_crs(huc8conus))
  joined_data <- st_join(points_sf, huc8conus)
  projd$huc8[i] = paste(unique(joined_data$HUC8),collapse='/')
  print(sprintf('%d/%d done',which(coordinate_idx==i),length(coordinate_idx)))
}
plot(huc8conus$geometry)
plot(points_sf,cex=5,add=TRUE,col='red')
write.csv(projd,filename,row.names=FALSE)
projd$huc8
