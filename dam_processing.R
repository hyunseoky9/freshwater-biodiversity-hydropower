#clean up McManamay's and NREL's dam data, get comids, etc. 

library(nhdplusTools)
library(pbapply)
library(sf)
# 1.  get comids for mcmanamay and nrel dam data.
# 
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/ryan.csv'
nrelfilename = 'G:/My Drive/research/sdm_modeling/gis/nrel datasets/Dams.csv'
EHA20filename = 'G:/My Drive/research/sdm_modeling/dam data/ORNL_EHAHydroPlant_FY2020revised.csv'
nabdfilename = 'G:/My Drive/research/sdm_modeling/dam data/nabd_fish_barriers_2012/nabd_fish_barriers_2012.csv' #https://www.sciencebase.gov/catalog/item/56a7f9dce4b0b28f1184dabd
nabd = read.csv(nabdfilename)
ryan = read.csv(ryanfilename)
nrel = read.csv(nrelfilename)
eha20  = read.csv(EHA20filename)

getcomid <- function(coord)
{
  start_point <- st_sfc(st_point(coord), crs = 4269)
  start_comid <- tryCatch(
    {
      withCallingHandlers(discover_nhdplus_id(point = start_point), message = function(c) if (inherits(c, "message")){stop("")})
      discover_nhdplus_id(point = start_point)
    },
    error = function(e) {
      return(NA)
    }
  )  
}

coords = cbind(eha20$Lon,eha20$Lat)
eha20$comid = pbapply(coords,1,getcomid)
write.csv(eha20,EHA20filename,row.names=FALSE)

coords = cbind(ryan$lon,ryan$lat)
ryan$comid = pbapply(coords,1,getcomid)
#write.csv(ryan,ryanfilename,row.names=FALSE)

coords = cbind(nrel$XCoordNID,nrel$YCoordFinal)
nrel$comid = pbapply(coords,1,getcomid)
#write.csv(nrel,nrelfilename,row.names=FALSE)






#1.1 get rid of duplicates in Ryan's dam data. 
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/EHA_FY18_Dam-fromRMcManamay-notpublic.csv' 
ryan = read.csv(ryanfilename)
# find out how many have 'H' in the purpose and how many from there have Mode attrb.
dim(ryan[which(grepl('H',ryan$Purpos) & ryan$Mode!='(null)'),])
# there's 1395 dams that have hydroelectric capacity and has the mode attribute!
ryan = ryan[which(ryan$Mode!='(null)'),] # only dams that have mode attr
ryan = ryan[which(grepl('H',ryan$Purpos)),] # only dams that have hydroelectric capacity ('H' in purpose)
dupcomid = unique(ryan$NHDM_COMID2[which(duplicated(ryan$NHDM_COMID2) & !is.na(ryan$NHDM_COMID2))])
# manually look at all the duplicate comids.
for( i in 1:length(dupcomid))
{
  print(sprintf('%d/%d',i,length(dupcomid)))
  print(ryan[which(ryan$NHDM_COMID2==dupcomid[i]),c('NHDM_COMID2','Upper','DmName','Purpos','Mode')])
  readline()
}

# for every duplicate...
# if there are multiple of them keep one that is 'main' in 'Upper'
for( i in 1:length(dupcomid))
{
  #print(ryan[which(ryan$NHDM_COMID2==dupcomid[i]),c('NHDM_COMID2','Upper','DmName','Purpos','Mode')])
  dupidx = which(ryan$NHDM_COMID2==dupcomid[i])
  # if there are multiple of them that have a non-null purpose and mode keep one that is 'main' in 'Upper'
  mainexist = which(ryan$Upper[dupidx]=='main')
  if(length(mainexist)>0)
  {
    rididx = c(rididx,dupidx[-mainexist])
  }
  if(length(rididx)== (length(dupidx)-1))
  {
    ryan = ryan[-rididx,]  
  } else {   # if there is no dam that has 'main' in 'Upper' attr, then keep the first one on the list.
    rididx = dupidx[2:length(dupidx)]
    ryan = ryan[-rididx,]
  }
  rididx = c()
}
newryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/EHA_FY18_Dam-fromRMcManamay-notpublic_processed.csv' 
#write.csv(ryan,newryanfilename)



# 2. check if all ryan's data is in coopers dam database
# * The NID IDs have changed over time so its not accurate to try to match the NID ids of 
# different datasets made in different years.
library(stringi)
nabdfilename = 'G:/My Drive/research/sdm_modeling/dam data/nabd_fish_barriers_2012/nabd_fish_barriers_2012.csv' #https://www.sciencebase.gov/catalog/item/56a7f9dce4b0b28f1184dabd
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/ryan.csv'
nrelfilename = 'G:/My Drive/research/sdm_modeling/gis/nrel datasets/Dams.csv'
EHA20filename = 'G:/My Drive/research/sdm_modeling/dam data/ORNL_EHAHydroPlant_FY2020revised.csv'
nabd = read.csv(nabdfilename)
ryan = read.csv(ryanfilename)
ryan = ryan[which(ryan$HMR_Stat=='Operational'),]
stri_sub(ryan$EHA_PtID, 7, 6) <- '_'
nrel = read.csv(nrelfilename)
eha20  = read.csv(EHA20filename)






eha20nid = sort(eha20$NID_ID)
ryannid = sort(ryan$NID_ID1)
merged = merge(eha20,ryan,by.x='EHA_PtID',by.y='EHA_PtID')
merged = merged[which(merged$NID_ID!="" & merged$NID_ID1!="(null)"),]
nidids = cbind(merged$NID_ID,merged$NID_ID1)
dim(merged)
length(which(merged$NID_ID!=merged$NID_ID1))
nidDiff = merged$EHA_PtID[which(merged$NID_ID!=merged$NID_ID1)]
cbind(sort(ryan$NID_ID1[which(ryan$EHA_PtID %in% nidDiff)]),
sort(eha20$NID_ID[which(eha20$EHA_PtID %in% nidDiff)]))
names(eha20)
names(ryan)
cbind(merged$PtName.x,merged$PtName.y)[which(merged$NID_ID!=merged$NID_ID1),]
cbind(merged$NID_ID,merged$NID_ID1)[which(merged$NID_ID!=merged$NID_ID1),]

sum(ryan$NID_ID1 %in% nabd$NIDID)
length(ryan$NID_ID1 %in% nabd$NIDID)
length(ryan$NID_ID1)
length(nabd$NIDID)

sum(eha20$NID_ID %in% nabd$NIDID)
length(eha20$NID_ID %in% nabd$NIDID)

sort(unique(nabd$Purposes))
ryan$NID_ID1



# 2.1 give nhd v2 comids for the coopers dam data. 
library(data.table)
library(pbapply)
library(nhdplusTools)
library(sf)
nabdfilename = 'G:/My Drive/research/sdm_modeling/dam data/nabd_fish_barriers_2012/nabd_fish_barriers_2012.csv' #https://www.sciencebase.gov/catalog/item/56a7f9dce4b0b28f1184dabd
nabd = read.csv(nabdfilename)
filename = 'G:/My Drive/research/sdm_modeling/gis/NHDv1_to_v2_crosswalk/v1v2crosswalk.csv'  
v1v2cross <- fread(filename, header = TRUE)
nabd$COMIDV2 = v1v2cross$V2_ComID[match(nabd$COMID,v1v2cross$V1_ComID)]
length(which(is.na(nabd$COMIDV2)))/length(nabd$COMIDV2) # percentage of v1comids that didn't match
nomatchidx = which(is.na(nabd$COMIDV2))
# for those with no matches of v1 comids, find it on your own using getcomid functions.
coords = cbind(nabd$newX[nomatchidx], nabd$newY[nomatchidx])
nabd$COMIDV2[nomatchidx] = pbapply(coords,1,getcomid)
write.csv(nabd,nabdfilename,row.names=FALSE)


# 2.1.2 give v2 comids for the Ryan's data
library(data.table)
library(pbapply)
library(nhdplusTools)
library(sf)
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/EHA_FY18_Dam-fromRMcManamay-notpublic.csv' 
ryan = read.csv(ryanfilename)
filename = 'G:/My Drive/research/sdm_modeling/gis/NHDv1_to_v2_crosswalk/v1v2crosswalk.csv'  
v1v2cross <- fread(filename, header = TRUE)
ryan$NHDM_COMID2 = v1v2cross$V2_ComID[match(ryan$NHDM_COMID,v1v2cross$V1_ComID)]
length(which(is.na(ryan$NHDM_COMID2)))/length(ryan$NHDM_COMID2) # percentage of v1comids that didn't match
nomatchidx = which(is.na(ryan$NHDM_COMID2))

# for those with no matches of v1 comids, find it on your own using getcomid functions.
coords = cbind(ryan$Lon[nomatchidx], ryan$Lat[nomatchidx])
ryan$NHDM_COMID2[nomatchidx] = pbapply(coords,1,getcomid)
#write.csv(ryan,ryanfilename,row.names=FALSE)



#3.0 try get_DM for a dam in ryan's data. see how far downstream it goes. 
# NOT NEEDED
library(nhdplusTools)
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/EHA_FY18_Dam-fromRMcManamay-notpublic.csv' 
ryan = read.csv(ryanfilename)
tcomid = ryan$NHDM_COMID2[1]
UM <- navigate_nldi(list(featureSource = "comid", 
                         featureID = tcomid), 
                    mode = "UM",
                    distance_km = 5000)

UM = as.numeric(UM$UM_flowlines$nhdplus_comid)
length(UM)
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = UM,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = TRUE,
                         return_data = TRUE, overwrite = TRUE)
subset$NHDFlowline_Network$ftype




# 3.1 find the first dam that's upstream main distance for all the points that are 
# known to have a dam upstream.
library(nhdplusTools)
library(pbapply)

spdata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/EHA_FY18_Dam-fromRMcManamay-notpublic_processed.csv' 
nrelfilename = 'G:/My Drive/research/sdm_modeling/gis/nrel datasets/Dams.csv'
ryan = read.csv(ryanfilename)
nrel = read.csv(nrelfilename)
damdata = ryan[,c('ComplxID','NHDM_COMID2','Mode')]
names(damdata) = c("ID","comid","mop")
#damdata = rbind(ryan[,c('NIDID','comid')],nrel[,c('NIDID','comid')])


getdam <- function(occpt,damD)
{
  # get damID of the dam that is certain distance upstream of an occurrence point.
  # occpt = length 2 vector with comid and udd of an occ pt.
  # comid = comid of a reach
  # udd = mainstem upstream distance to the nearest dam
  # damD = dam data (data.ft)

  #ii=1  
  #occpt = as.matrix(occwudd[,c('comid','udd')])[ii,] # test data
  #damD= damdata
  comid =  as.integer(occpt[1])
  udd = occpt[2]
  # find all the comids udd km's upstream of the occ pt. 
  UM <- navigate_nldi(list(featureSource = "comid", 
                                 featureID = comid), 
                            mode = "UM", 
                            distance_km = udd + 100)
  UM = as.numeric(UM$UM_flowlines$nhdplus_comid)
  # find which one of the upstream comids have dams on them. 
  damcomid = UM[which(UM %in% damD$comid)]
  if(length(damcomid)==0) # there's no hydroelectric dam (dam data only has hydroelectric dams)
  {
    damid = NA
  } else if (length(damcomid)>1) # there's more than 1 dam matched
  {
    if(sum(duplicated(damcomid))>0) # if they are the same comid. just take the first one
    {
      damid = damD$ID[which(damD$comid==damcomid[1])]      
    } else # if there's two different dam matched, still just take the first one
    {
      message = 'there was more than two matches, went with the first one.'
      print(message)
      damid = damD$ID[which(damD$comid==damcomid[1])]
    }
  } else # just one match. Ideal.
  {
    damid = damD$ID[which(damD$comid==damcomid)]
  }
  if(is.na(damid))
  {
    mop = NA
  } else
  {
    mop = damD$mop[which(damD$ID==damid)[1]] # take the first element if there's two.
  }
  return(c(damid,mop))
}

startfrom = 64
for (i in startfrom:nrow(spdata))
{
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  print(sprintf('%d/%d',i,nrow(spdata)))
  print(spdata$name[i])
  occ = read.csv(spfilename)
  occwuddidx = which(occ$udd!= -99 & !is.na(occ$udd))
  occwudd = occ[which(occ$udd!= -99 & !is.na(occ$udd)),] # only get occ pts with upstream dam distance
  occ$hdamid = NA
  occ$mode = NA
  if(length(occwuddidx)>0)
  {
    damID = pbapply(as.matrix(occwudd[,c('comid','udd')]),1,getdam,damD=damdata)
    occ$hdamid[occwuddidx] = damID[1,]
    occ$mode[occwuddidx] = damID[2,]
  }
  #write.csv(occ,spfilename)
}
# manually check how the damid and mode look like for species

problemidx = c(579, 576, 574, 558, 549, 546, 542) #c(545, 538, 481)
for (i in problemidx)#(i in 1:nrow(spdata))
{
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  print(sprintf('%d/%d',i,nrow(spdata)))
  print(spdata$name[i])
  occ = read.csv(spfilename)
  occwuddidx = which(occ$udd!= -99 & !is.na(occ$udd))
  occwudd = occ[which(occ$udd!= -99 & !is.na(occ$udd)),] # only get occ pts with upstream dam distance
  if(length(occwuddidx)>0)
  {
    print(cbind(occ$hdamid[occwuddidx],occ$mode[occwuddidx]))
  }
  readline()
}
# get numocc, number of udd and percentage to numocc, number of udd with hdamid and percentage to numocc.
numocc = c()
numwudd = c()
numwdamid = c()
for (i in 1:nrow(spdata))
{
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
  print(sprintf('%d/%d',i,nrow(spdata)))
  print(spdata$name[i])
  occ = read.csv(spfilename)
  numocc[i] = nrow(occ)
  occwuddidx = which(occ$udd!= -99 & !is.na(occ$udd))
  occwudd = occ[which(occ$udd!= -99 & !is.na(occ$udd)),] # only get occ pts with upstream dam distance
  numwudd[i] = length(occwuddidx)
  numwdamid[i] = length(which(!is.na(occ$hdamid)))
}

uddp = numwudd/numocc
damp = numwdamid/numocc

mean(uddp)
mean(damp)
hist(uddp, main= sprintf('percentage of presence points \n with upstream dam distance'),xlab='')
hist(damp, main= sprintf('percentage of presence points with upstream \n hydropower dam with a mode of operation'),xlab='')





# 4. SPECIAL TASK: make sure all the reaches in columbia and snake river have distance to upstream
# mainstem dam value. 
# this is done to validate whether the Cooper's data on the distance to the nearest upstream dam (udd) 
# data is correct. All the reaches in Columbia river should technically have a dam upstream, thus having 
# the udd value 
library(nhdplusTools)
library(pbapply)

# first, get some reaches that are on mainstem columbia river by using comids of dams 
# that are on the river in Ryan's data.
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/EHA_FY18_Dam-fromRMcManamay-notpublic_processed.csv' 
ryan = read.csv(ryanfilename)
#coldams = ryan[which(grepl('COLUMBIA',ryan$Water)),]  # columbia river dams
coldams = ryan[which(ryan$Water=='COLUMBIA RIVER'),]
coldamcomid = coldams$NHDM_COMID2


# get upstream & downstream reaches for each of the comids for 1000km's.
# total length of Columbia river is 2000km.
comid = coldamcomid[1]


getReaches <- function(comid)
{
  UM <- navigate_nldi(list(featureSource = "comid", 
                           featureID = comid), 
                      mode = "UM", 
                      distance_km = 1000)
  UM = as.numeric(UM$UM_flowlines$nhdplus_comid)
  
  DM <- navigate_nldi(list(featureSource = "comid", 
                           featureID = comid), 
                      mode = "DM", 
                      distance_km = 1000)
  DM = as.numeric(DM$DM_flowlines$nhdplus_comid)
  return(c(UM,DM))
}

reach = getReaches(comid)
terminal.reach = 23832907
DM <- navigate_nldi(list(featureSource = "comid", 
                         featureID = terminal.reach), 
                    mode = "DM", 
                    distance_km = 1000)
DM = as.numeric(DM$DM_flowlines$nhdplus_comid)


reaches = pbapply(as.matrix(coldamcomid),1,getReaches)

#check that all the dam's comids are in the same Columbia mainstem
# checked. All the dam's comids are in the same mainstem.
for ( i in 1:length(coldamcomid))
{
  result = all(coldamcomid %in% reaches[[i]])
  print(i)
  print(result)
}

# merge all the reaches together.
colcomid = unique(unlist(reaches))
length(colcomid)

# get udd data from huc 17,16,14, and 10
filenames = c(sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',c(17,16,14,10)))
huc = read.csv(filenames[1])
for ( i in 2:length(filenames))
{
  file = read.csv(filenames[i])
  huc = rbind(huc,file)
}
hucudd = huc$udd

# check all columbia river comids are in huc 17,16,14,10
result = colcomid[which(colcomid %in% huc$comid)]
length(result) # not all colcomid in huc.. weird..

orifilenames = c(sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/original/huc%d.csv',c(17,16,14,10)))
hucori = read.csv(orifilenames[1])
for ( i in 2:length(orifilenames))
{
  file = read.csv(orifilenames[i])
  hucori = rbind(hucori,file)
}
# check all columbia river comids are in huc
resultori = colcomid[which(colcomid %in% hucori$x)]
length(resultori) # not all colcomid in huc.. weird..
resultori[which(! resultori %in% result )]
nonexistent = resultori[which(! resultori %in% result )] # ones in original but not in processed huc data


getFtype <- function(comid)
{
  
  comidread = 0
  class(comidread) = 'try-error'
  while(class(comidread)=='try-error')
  {
    comidread = try({
      subset_file <- tempfile(fileext = ".gpkg")
      subset <- subset_nhdplus(comids = comid,
                               output_file = subset_file,
                               nhdplus_data = "download", 
                               flowline_only = FALSE,
                               return_data = TRUE, overwrite = TRUE)
    })
  }
  return(subset$NHDWaterbody$ftype)
}
nonexi_wbdftype = pbapply(as.matrix(nonexistent),1,getFtype)
for(i in 1:length(nonexi_wbdftype))
{
  if(!is.null(nonexi_wbdftype[[i]]))
  {
    print(nonexi_wbdftype[[i]])
  }
}


subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = nonexistent,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)


length(subset$NHDWaterbody$comid)
length(subset$NHDFlowline_Network$comid)
which(nonexistent %in% subset$NHDWaterbody$comid)


#try plotting 
subset_file <- tempfile(fileext = ".gpkg")
subset_colcomid <- subset_nhdplus(comids = DM,
                                  output_file = subset_file,
                                  nhdplus_data = "download", 
                                  flowline_only = TRUE,
                                  return_data = TRUE, overwrite = TRUE)

subset_colcomid <- subset_nhdplus(comids = colcomid,
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = TRUE,
                         return_data = TRUE, overwrite = TRUE)
flowline <- subset_colcomid$NHDFlowline_Network
flowline2 <- subset$NHDWaterbody
plot(sf::st_geometry(flowline), col = "blue")
plot(sf::st_geometry(flowline[33,]),col="red",add=TRUE)
plot(sf::st_geometry(subset$NHDFlowline_Network), col = "blue")
