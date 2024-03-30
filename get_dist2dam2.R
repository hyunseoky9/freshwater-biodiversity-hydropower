# calculate nearest upstream mainstem dam distance (name the attribute 'udd2')
# The first three functions are functions used in the main script that starts after
# the comment line "MAIN".
library(nhdplusTools)
library(pbapply)
library(sf)
rm(list=ls())
# functions
#damcomid = ryan[ceiling(runif(1,0,1)*nrow(ryan)),]$comid
#damcomid = 6966863
#amcomids = dam.reaches
get_terminal <- function(damcomid,damcomids)
{
  # get terminal reach downstream mainstem from the dam location,
  # terminal reach is either 
  # a) another dam downstraem mainstem from the dam
  # b) last reach that flows to a lake or ocean.
  done = 0
  seedcomid = damcomid
  dm = c()
  # keep going downstream until you find a terminal reach
  while(!done)
  {
    flowlines <- navigate_nldi(list(featureSource = "comid", 
                                     featureID = seedcomid), 
                                mode = "DM", 
                                distance_km = 5000)
    reaches = as.numeric(flowlines$DM_flowlines$nhdplus_comid)
    reachlen = as.numeric(st_length(flowlines$DM_flowlines$geometry))/1000
    if(length(reaches)==1 & length(dm)==0)
    {
      dm = cbind(reaches,reachlen)
      return(dm)
    }
    dammatch = match(reaches,damcomids)
    dammatch.idx = which(!is.na(dammatch[2:length(dammatch)]))+1 # excludes the first reach cuz its the starting dam.
    if(length(dammatch.idx)) # terminal reach is a dam downstream ( a )
    {
      attach = cbind(reaches[1:dammatch.idx[1]],reachlen[1:dammatch.idx[1]])
      dm = rbind(dm,attach)
      done = 1
    } else # no dam downstream.
    {
      if(length(reaches) == 1) # seedcomid is the terminal reach
      {
        done = 1
        #print('terminal')
        #print(length(reaches))
      } else { # seedcomid may not be the terminal reach
        seedcomid = reaches[length(reaches)]
        attach = cbind(reaches,reachlen)
        dm = rbind(dm,attach)
        #print('not terminal')
        #print(length(reaches))
      }
    }
  }
  return(unique(dm)) # get rid of duplicate reaches, where the segments piece together.}  
}
  #dm
#dammatch.idx
#length(dm)

#first.dam = first.dam$comid
#term.comid = last.comid
#damcomids = dam.reaches
get_origin <- function(first.dam,term.comid,damcomids)
{
  # go upstream mainstem of the dam until you hit a start of the mainstem 
  # (origin or another dam) or the first.dam.
  
  # first.dam = comid of the subject dam.
  # term.comid = terminal reach obtained from get_terminal,which we will not go upstream mainstem from
  # damcomids = comids of dams in ryan's dam data.
  done = 0
  seedcomid = term.comid
  um = c()
  while(! done)
  {
    flowlines <- navigate_nldi(list(featureSource = "comid", 
                                    featureID = as.numeric(seedcomid)), 
                               mode = "UM", 
                               distance_km = 5000)
    reaches = as.numeric(flowlines$UM_flowlines$nhdplus_comid)
    if(first.dam %in% reaches) # upstream mainstem leads to the subject dam.
    {
      um = c(um,reaches[1:which(reaches == first.dam)])
      done  = 1
      #print('firstdam')
    } else # upstream mainstem doesn't lead to the subject dam.
    {
      dammatch = match(reaches,damcomids)
      dammatch.idx = which(!is.na(dammatch[2:length(dammatch)]))+1 # excludes the first reach cuz its the starting dam.
      if(length(dammatch.idx)) # there's another dam upstream mainstem
      {
        #print('another dam')
        um = c(um, reaches[1:dammatch.idx[1]])
        done = 1        
      } else { # there's not another dam upstream mainstem.
        if(length(reaches) == 1) # seedcomid is the origin reach
        {
          um = c(um,reaches)
          done = 1
          #print('origin')
          #print(length(reaches))
        } else { # seedcomid may not be the terminal reach
          seedcomid = reaches[length(reaches)]
          um = c(um,reaches)
          #print('not origin')
          #print(length(reaches))
        }
      }
    }
  }
  return(unique(um)) # get rid of duplicate reaches, where the segments piece together.
}

#udd2val = apply(as.matrix(calc_comid),1,udd2calc,
#                damcomid=first.dam$comid, 
#                calc_comids=calc_comid, damreach_dist=damreachD)

#calc_comids = calc_comid
#comid = calc_comid[3]
#damcomid = first.dam$comid
#damreach_dist = damreachD
#calc_comids.length = dm.length[1:length(calc_comid)]
#vals = c()
#for (j in 1:15)
#{
#  val = udd2calc(calc_comid[j], first.dam$comid, calc_comid, damreachD)
#  vals[j] = val
#}



udd2calc <- function(comid,damcomid,calc_comids,damreach_dist,calc_comids.length)
{
  #   calculate upstream mainstem dam distance
  # a. if the reach is on the same comid as the reach where the dam is, 
  #    distance is from the point on the reach where the dam is to 
  #    the downstream end of the reach. *assumes the occ pt is downstream of the dam.
  # b. if the reach is on different comid then the reach where the dam is, the distance is:
  #    dist. btw the dam pt and downstream end of the reach + 
  #    distance of the reaches that are between the dam's reach and the subject reach. +
  #    if(the terminal reach is another dam)
  #    {distance from the dam pt to the upstream end of the reach}
  #    else if(the terminal reach in no another dam)
  #    {distance from the the midpoint of the subject reach to the upstream end of the reach (1/2 reach length)}
  
  # comid = comid to calculate the upstream mainstem distance for
  # damcomid = the mainstem dam's comid.
  # calc_comids = comids that share the mainstem dam, identified by 'damcomid'
  # damreach_dist = dataset that gives upstream and downstream distance from the dam location to the
  #   upstream and downstream end of the dam's reach, respectively. 
  
  if(comid==damcomid) # case a.
  {
    udd2 = damreach_dist$d_length_km[which(damreach_dist$comid == damcomid)]
  } else # case b
  {
    udd2 = damreach_dist$d_length_km[which(damreach_dist$comid == damcomid)]
    idx = which(calc_comid==comid)
    #if(idx==length(calc_comid) & calc_comid[idx] %in% damreach_dist$comid) # if the subject reach is terminal and has a dam on it.
    #{
      # if its the last reach and has a dam on it, its udd value on the reach
      # should be from that dam. Therfore, the rest of the code below in this parenthesis 
      # is useless
      
      #if(idx == 2)
      #{
      #  udd2 = udd2 + damreach_dist$u_length_km[which(damreach_dist$comid == comid)]
      #} else # idx > 2
      #{
      #  udd2 = udd2 + damreach_dist$u_length_km[which(damreach_dist$comid == comid)]
      #  subset_file <- tempfile(fileext = ".gpkg")
      #  subset <- subset_nhdplus(comids = calc_comid[2:(idx-1)],
      #                           output_file = subset_file,
      #                           nhdplus_data = "download", 
      #                           flowline_only = TRUE,
      #                          return_data = TRUE, overwrite = TRUE)
      #  udd2 = udd2 + sum(subset$NHDFlowline_Network$lengthkm)
      #}
    #} else {
      #comidread = 0 # variable made up for error-catching.
      #class(comidread) = 'try-error'
      #trytimes = 0
      #while(class(comidread)=='try-error' & trytimes <= 2) # get ftype of the comid
      #{
      #  comidread = try({
      #    subset_file <- tempfile(fileext = ".gpkg")
      #    subset <- subset_nhdplus(comids = calc_comid[2:(idx-1)],
      #                             output_file = subset_file,
      #                             nhdplus_data = "download", 
      #                             flowline_only = TRUE,
      #                             return_data = TRUE, overwrite = TRUE)
      #    print(subset$NHDFlowline_Network$lengthkm)
      #  })
      #  if( class(comidread)=='try-error')
      #  {
      #    trytimes = trytimes + 1
      #    print(sprintf('try times = %d',trytimes))
      #  }
      #}
      #if (trytimes==51)
      #{
        #print(sprintf('didnt work for comid=%d',comid))
        #UM = navigate_nldi(list(featureSource = "comid", 
        #                       featureID = comid), 
        #                   mode = "UM",
        #                   distance_km = 10)
        #UM = sf::st_geometry(UM$origin)
        #st_length(UM)
      #}
      #udd2 = udd2 + sum(calc_comids.length[2:(idx-1)]) # terminal reach length halved when adding
    #}
    udd2 = udd2 + sum(calc_comids.length[2:idx]) # terminal reach length halved when adding      
  }
  return(udd2)
}

# MAIN
# get all the dams
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/ryan.csv'
ryan = read.csv(ryanfilename)
ryan = ryan[which(grepl('Operational',ryan$HMR_Stat)),] # only operational dams
ryan = ryan[-which(duplicated(ryan$comid)),]# get rid of duplicate comids
ryan = ryan[-which(is.na(ryan$comid)),] # get rid of na comids
# get the up&downstream reach dist of dam reaches.
damreachD = ryan[,c('comid','u_length_km','d_length_km')]

udd2filename = 'G:/My Drive/research/sdm_modeling/dam data/udd2.csv'
udd2info = read.csv(udd2filename)
alreadydone = unique(udd2info$umdam_comid)
unfinished.dam = ryan$comid[which(!is.na(ryan$comid))]
unfinished.dam = unfinished.dam[which(!unfinished.dam %in% alreadydone)]
total_dam_num = length(unfinished.dam)
dam.reaches = ryan$comid
next.dam.id = -1


# algorithm:
#dams2redo = c()
while(length(unfinished.dam))
{
  # 1. pick a dam.(first one on the df)
  if(next.dam.id != -1)
  {
    first.dam = ryan[which(ryan$comid == next.dam.id),] # use the one chosen from last round of the loop
  } else {
    select_idx = ceiling(runif(1,0,1)*length(unfinished.dam))
    subj.comid = unfinished.dam[select_idx]
    first.dam = ryan[which(ryan$comid == subj.comid),] # pick one at random
  }
  print(sprintf('selected dam comid = %d',first.dam$comid))
  # 2. go downstream mainstem of the dam until you either hit a dam or last flowline downstream (ocean or G.Lake)
  dm.comids = get_terminal(first.dam$comid,dam.reaches)
  dm.length = dm.comids[,2]
  dm.comids = dm.comids[,1]
  
  print('get_terminal done')
  term.comid = dm.comids[length(dm.comids)]
  term.comid.dam = term.comid %in% dam.reaches # T/F whether terminal reach is a dam.
  print(term.comid.dam)
  # 2.2 if term.comid is another dam, make that dam the first.dam in the next while-loop
  
  #if(term.comid.dam & term.comid %in% unfinished.dam)
  #{
  #  next.dam.id = term.comid
  #} else
  #{
  #  next.dam.id = -1
  #}
  
  # 3. go upstream mainstem of the dam until you hit a start of the mainstem (origin or another dam) or the first.dam.
  again = 1
  last.comid = term.comid
  first.uncoinciding_comid.idx = length(dm.comids)
  while(again)
  { 
    um.comids = get_origin(first.dam$comid, last.comid, dam.reaches)
    print('get_origin done')
    #4.1 if the um.comid includes the first dam's comid, start calculating udd2 downstream from the first dam until you hit term.comid
    if(first.dam$comid == um.comids[length(um.comids)])
    {
      calc_comid = dm.comids # comids to calculate the udd2 value for
      again = 0
      print('found udd2 reaches')
    } else if (length(dm.comids)==1)
    {
      calc_comid = dm.comids 
      again = 0
    } else { 
    #4.2 if the um.comid is not the first dam's comid, find the most downstream comid that doesn't coincide between um.comids and dm.comids
    # and go though 3. again.
      first.uncoinciding_comid = dm.comids[tail(which(is.na(match(dm.comids[1:first.uncoinciding_comid.idx],um.comids))),n=1)]
      first.uncoinciding_comid.idx = tail(which(is.na(match(dm.comids[1:first.uncoinciding_comid.idx],um.comids))),n=1)
      last.comid = first.uncoinciding_comid
      again = again + 1
      #if(again>2)
      #{
      #  dams2redo = c(dams2redo, first.dam$comid)
      #}
      print('going thru the while loop')
    }
  }
  print('out of the while loop!')
  if( length(calc_comid) > 1 & calc_comid[length(calc_comid)] %in% dam.reaches) # exclude the last reach if its a dam
  {
    calc_comid = calc_comid[-length(calc_comid)]
  }
  #last.comid.dam = calc_comid[length(calc_comid)] %in% dam.reaches  #T/F whether last calc_comid is a dam.
  #print(last.comid.dam)
  #5. calculate the udd2 for the chosen comids.
  udd2val = try({pbapply(as.matrix(calc_comid),1,udd2calc,
                  damcomid=first.dam$comid,
                  calc_comids=calc_comid, damreach_dist=damreachD, calc_comids.length=dm.length[1:length(calc_comid)])})
  if(!class(udd2val)=='try-error')
  {
    print('udd2val done')
    added_data = data.frame(calc_comid,first.dam$EHA_PtID,first.dam$comid,udd2val)
    names(added_data) = c('comid','EHA_PtID','umdam_comid','udd2')
    udd2info = rbind(udd2info,added_data)
    write.csv(udd2info,udd2filename,row.names=FALSE)
    #99. get rid of the first.dam from the unfinished.dam.
    unfinished.dam = unfinished.dam[-which(unfinished.dam==first.dam$comid)]
    print(sprintf("%d/%d done",total_dam_num - length(unfinished.dam),total_dam_num))
  }
}


# get udd2 values for occpts and projection pts.

setwd('G:/My Drive/research/sdm_modeling/spdata')
spnamedata = read.csv('./comprehensive_sp_info.csv')
spname_list = spnamedata$name
udd2filename = 'G:/My Drive/research/sdm_modeling/dam data/udd2.csv'
damdistdata = read.csv(udd2filename)
#names(damdistdata) = 
occ_or_proj= 1 #1= sp occurrene pts. 0= projection area
if(occ_or_proj)
{
  looplength = nrow(spnamedata)
} else {
  looplength = 18 # number of huc units
}

# just a loop to index the speies occurrence data again.
for(i in 1:length(spname_list)) #1:18)
{
    spname = spnamedata$name[i]
    # for getting predictor values of fitting points.(sp occ pts)
    spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
    data = read.csv(spfilename)  
    data$X = 1:nrow(data)
    write.csv(data,spfilename,row.names=FALSE)
    #print(sprintf(length(which(!is.na(data$udd2)))))
}
    

for(i in 1:length(spname_list)) #1:18)
{
  # **spdata could mean species occurrence points or projection area.
  
  if(occ_or_proj)
  {
    spname = spnamedata$name[i]
    # for getting predictor values of fitting points.(sp occ pts)
    spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spname)
    data = read.csv(spfilename)  
  } else{
    # for getting predictor values of projection points.
    hucnum = i
    projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",hucnum)
    data = read.csv(projfilename)
    print(nrow(data))
  }
  if(ncol(data)==1)
  {
    data = read.csv(filename, sep="\t")
  }
  matches = match(data$comid,damdistdata$comid)
  data$udd2 = damdistdata$udd2[matches] # udd=upstream dam distance
  data$ud_ehaID = damdistdata$EHA_PtID[matches]
  data$ud_comid = damdistdata$umdam_comid[matches]
  
  # match occ data comid to coopers damdist comid. reference the script before this one.
  if(occ_or_proj)
  {
    write.csv(data,spfilename,row.names=FALSE)  
  } else{
    write.csv(data,projfilename,row.names = FALSE)
  }
}


#dams2redo = read.csv('G:/My Drive/research/sdm_modeling/dam data/dams2redo.csv')

#length(which(udd2info$umdam_comid %in% dams2redo$x))
#nrow(udd2info)
#udd2info_erased = udd2info[-which(udd2info$umdam_comid %in% dams2redo$x),]
#write.csv(udd2info_erased,udd2filename,row.names=FALSE)
