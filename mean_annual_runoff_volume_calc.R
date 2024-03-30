# make weighted average of the mean annual runoff (mm) that overlay in the drainage basin polygon
# and multiply it by the area to get the mean annual runoff of the drainage basin
library(dams)
library(data.table)
library(raster)
library(sf)
library(lwgeom)
probids= c("NH00129", "NH00366", "NH00414", "NH00166", "NH00014", "NH00165", "ME00221", "ME00220", "ME00219", "ME00218", "MA00849", "NH00097", "MA00973", "NH00259", "NH00112", "VT00205",
           "VT00199", "VT00151", "VT00028", "MI00650", "NY00689", "NY00678", "MN00653", "SD01095", "SD01092", "SD01093", "SD01094", "TX02296", "TX00024", "MT00652", "ID00155", "ID00222",
           "MT00223", "MT00224", "WA00009", "WA00013", "ID00319", "WA00098", "WA00299", "WA00262", "WA00097", "WA01374", "WA00085", "WA00084", "WA00086", "OR00001", "OR00011", "OR00616",
           "WA00346", "WA00168", "WA00170", "WA00169", "MT00226", "WA00268", "WA00088", "OR00002")
# get the mar raster
raster = raster('G:/My Drive/research/sdm_modeling/dam data/runoff data/McCabe and Wolock 2011 mean annual runoff raster.tif')
# do it for every polygon
{
  nidids = c()
  for( i in 1:18)
  {
    path1 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc_nontempflow/huc%d.csv',i)
    spproj = fread(path1)
    nidids = c(nidids,unique(spproj$udd2hilarri_nidid))
  }
  nidids= unique(nidids)
  nidids= nidids[-which(is.na(nidids))]
  
  idx = apply(as.matrix(nidids),1,function(x) which(nid_subset$nidid ==x))
  nid_subset$nidid[idx[[286]]]
  
  for( i in 1:length(idx))
  {
    if(length(idx[[i]])>1)
    {
      #print(i)
      #print(nid_subset[idx[[i]],c(4,5)])
      #readline()
      idx[[i]] = idx[[i]][1] # only take the first dam.
    }
  }
  
  idx = unlist(idx)
  idx = idx[which(!is.na(idx))]
  featured_dams = nid_subset[idx,]
  
  a = which(nidids %in% nid_subset$nidid)
  b = which(!(nidids %in% nid_subset$nidid)) # dams in sdm not featured in NID.
  length(a)
  length(b)
  nidids[b]
  # add in the dam data for the sdm data dams that are un-featured in the NID manually.
  unfeatured_dams = data.frame()
  unfeatured_dams = rbind(unfeatured_dams,c('ME83016',955,44.5792,-69.5552))
  unfeatured_dams = rbind(unfeatured_dams,c('ME00054',60,43.3946205,-70.4436353))
  unfeatured_dams = rbind(unfeatured_dams,c('ME96105',126,44.285,-70.5371))
  unfeatured_dams = rbind(unfeatured_dams,c('ME83030',711,43.497637, -70.451265))
  unfeatured_dams = rbind(unfeatured_dams,c('GA00599',207,34.076389, -83.804167))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00726',2640,46.746699, -91.485091))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00136',140,43.687772, -89.047921))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00288',180,44.494025, -89.311200))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00284',120,43.472253, -87.991488))
  unfeatured_dams = rbind(unfeatured_dams,c('NC05713',1400,35.4646, -82.5442))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00036',740,46.237210, -91.783966))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00056',710,45.761023, -91.220270))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00184',670,42.843677, -89.172776))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00219',200,43.586347, -90.644188))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00301',95,43.458664, -89.714883))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00181',500,43.223567, -90.466741))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00302',200,43.466794, -89.742323))
  unfeatured_dams = rbind(unfeatured_dams,c('WI00149',1000,45.192620, -89.688284))
  unfeatured_dams = rbind(unfeatured_dams,c('CA10411',165,36.9837,	-120.5))
  names(unfeatured_dams)= c('nidids','max_storage_acreft','lat','long')
  head(unfeatured_dams)
  
  
  # get all the coordinates from featured and unfeatured.
  all_coords  = data.frame(nidid=c(featured_dams$nidid,unfeatured_dams$nidids),long=c(featured_dams$longitude,unfeatured_dams$long),
                           lat=c(featured_dams$latitude,unfeatured_dams$lat))
}

probidx = apply(as.matrix(probids),1,function(x) which(all_coords$nidid==x))

mar_vol = c()
dir = 'C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/'
files = list.files(dir)
for(i in 1:nrow(all_coords))
{
  
  id = all_coords$nidid[i]
  #id = 'NV10439'
  # get the basin polygon
  
  targetfilename = files[which(grepl(sprintf('%s',id),files))]
  targetfilename = targetfilename[which(grepl('_nhd.shp',targetfilename))]
  if(grepl('nhd',targetfilename))
  {
    basin = st_read(sprintf('C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/%s_nhd.shp',id))    
  } else
  {
    basin = st_read( sprintf('C:/Users/Hyun/OneDrive/Desktop/finished research/sdm_modeling/dam drainage basin shape files/%s.shp',id))    
  }
  #plot(basin$geometry)
  #plot(raster,add=TRUE)
  
  wavg <- extract(raster, basin, weights = TRUE, normalizeWeights=TRUE, fun=mean)
  if(is.nan(wavg) | is.na(wavg))
  {
    # get the average value of the MAR cells in the polygon
    cropped_raster = crop(raster,extent(basin))
    masked_raster <- mask(cropped_raster, basin)
    raster_matrix <- as.matrix(masked_raster)
    average_value = mean(raster_matrix[which(!is.na(raster_matrix))])

    # assign the average value to the NAs in the masked raster
    idx_false = which(is.na(as.matrix(masked_raster)))
    numcol = ceiling(idx_false/nrow(masked_raster))
    numrow = idx_false - (numcol-1)*nrow(masked_raster)
    idx = (numrow-1)*ncol(masked_raster) + numcol
    if(all(!is.na(masked_raster[idx])))
    {
      print('you fucked up bro')
    }
    masked_raster[idx] = average_value
    wavg <- extract(masked_raster, basin, weights = TRUE, normalizeWeights=TRUE, fun=mean)
  }
  
  # get area
  # warning: if you get the following error, run 'sf_use_s2(FALSE)':
  #Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
  #Loop 1 is not valid: Edge 46 is degenerate (duplicate vertex)
  area <- try({st_area(st_as_sf(basin$geometry))},silent=TRUE)
  if(class(area)== 'try-error')
  {
    sf_use_s2(FALSE)
    area = st_area(st_as_sf(basin$geometry))
    sf_use_s2(TRUE)
  }
  # volume based mean annual runoff unit is km^3
  o = wavg*as.numeric(area)*10^(-12) 
  mar_vol = c(mar_vol,o)
}
nrow(all_coords)
length(mar_vol)
df = data.frame(nidid=all_coords$nidid,mean_annual_runoff_km3=mar_vol)
write.csv(df,'G:/My Drive/research/sdm_modeling/dam data/runoff data/mean_annual_runoff_volume_km3_bydam.csv')
