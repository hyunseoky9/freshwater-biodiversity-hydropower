# UPDATE (2/28/2022)
# OLD script from 2021 summer that you probably don't need to access anymore. 
# THE SCRIPT THAT GETS THE UPSTREAM DAM DISTANCE FROM COOPERS' DATA IS 'get_dist2dam.R'

# get distance to nearest dam data for each occurrence points.
#get species occurrence data.
# pull out species list
library(sf)
library(nhdplusTools)
library(data.table)
setwd('C:/Users/hy324/Google_Drive/research/SDM modeling/spdata/per_sp')
splist = read.csv("C:/Users/hy324/Google_Drive/research/SDM modeling/spdata/sp_list.csv")
splist = splist$species_name
cooper = fread('C:/Users/hy324/Google_Drive/research/SDM modeling/gis/dam_data.csv',header=TRUE)

for(s in splist)
{
  # pull out occ data for species s
  filename = sprintf("%s.csv",s)
  spdata <- fread(filename, sep = "\t", header = TRUE, na.strings = "\\N")
  spdata$comid = NA # make a new column for comid
  spdata$updamdist = NA # new column for upstream main stream distance.
  #get comid   
  for(i in 1:nrow(spdata))
  {
    occ_pt <- st_sfc(st_point(c(spdata$decimalLongitude[i],spdata$decimalLatitude[i])), crs = 4269)
    gotdata = 0
    while(!gotdata){
      comids <- try(apply(cbind(spdata$decimalLongitude,spdata$decimalLatitude),1,comidcalc))
      if(class(result)!="try-error"){
        gotdata = 1
      } else {
        sprintf('i=%d, error occurred',i)
      }
    }    
    #spdata$updamdist <- cooper$COMIDV2[which(cooper$COMIDV2==spdata$comid[i])]
  }
}


comids <- 



comidcalc <- function(longlat){
  occ_pt <- st_sfc(st_point(longlat), crs = 4269)
  return(discover_nhdplus_id(occ_pt))
}

#######################################################################################
# using nhd plus tool which I stopped working on since finding Cooper et al. 2017 data
########################################################################################

#snap all the occurrence points to flowline
species = read.csv("sp_list.csv")
occ_comid <- discover_nhdplus_id(occ_points) # get comids of occ points


#snap all the dam points to flowline
  # read dam and hydropower csv
setwd("C:/Users/hy324/Google_Drive/research/SDM modeling/gis/nrel datasets")  
damcsv = read.csv("Dams.csv")
hydropowercsv = read.csv("HydroPower.csv")
relcol= c("RECORDID","DAM_NAME","XCoordNID","YCoordNID")
combinedata = rbind(damcsv[,relcol],hydropowercsv[,relcol])
damdata = combinedata[which(duplicated(combinedata$RECORDID)==FALSE),]

for(i in 1:nrow(damdata)){
  dam_pt = st_sfc(st_point(c(damdata$XCoordNID[i], damdata$YCoordNID[i])))
  dam_comid[i] <- discover_nhdplus_id(dam_pt)  
}

# get unique points of dams from the two csv
dam_comid <- discover_nhdplus_id(damdata) # get comids of dam points




#navigate upstream from each occurrence points and find all the dams upstream


#calculate the distances between an occurrence point and all the dams upstream
  
  #find all the common comid's between upstream comid's of occurrence points and downstream comid's of dams
  #calculate the sum of the length of common comid's
  # subtract "loose ends" of the comid's where occurrence point and dam point are.


#find the closest upstream dam from an occurrence point