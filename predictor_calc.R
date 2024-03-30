#get all the temperature and flow related predictor values for each species in the interested points.
rm(list = ls())
library(sf)
library(rgeos)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(dplyr)
library(raster)
setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
start = 2011
finish = 2019

###when doing all the species, get an array of tmin and tmax and spnames
spname = spdata$name
tmins = spdata$tmin
tmaxs = spdata$tmax #dummy value for now #spnamedata$tmax


# just for freshwater darter
idx = which(spdata$name == 'Aplodinotus grunniens')
sp = spdata$name[idx]
tmins = spdata$tmin[idx]
tmaxs = spdata$tmax[idx]



#for(j in 1:(finish-start + 1))
#{
  # access the temperature data for those points for the relevant timeframe calculate the temperature and flow related predictor variables:
  #1. average maximum number of days exceeding maximum temperature threshold in a year
  #2. average maximum number of days going below minimum temperature threshold in a year
  #3-11. average growing degree days by 90, 120, 150 Julian days with 5,8, and 10 degree celsius as base temp.
  
  #12. BFI= base flow index
  #13-14. average date and amount of annual maximum flow
  #15-16. average date and amount of annual minimum flow
#  j=1
#  print(j)
#  yr = (j-1) + start
#  tempfilename = sprintf('G:/My Drive/research/sdm_modeling/gis/wbm/ohio_basin/runs/RiverTemperature_%d.csv',yr)
#  tempd =fread(tempfilename,header=TRUE)
#  print('tempd read')
#  names(tempd)[1] = 'ID'
#  flowfilename = sprintf('G:/My Drive/research/sdm_modeling/gis/wbm/ohio_basin/runs/Discharge_%d.csv',yr)
#  flowd = fread(flowfilename, header=TRUE)
#  print('flowd read')
#  names(flowd)[1] = 'ID'
#}
# getting predictor values for occ pts or projection area?
occ_or_proj= 0 #1= sp occurrene pts. 0= projection area

{
#input:
# sp= array of species name
# tmins= array of minimum temperature threshold
# tmaxs= array of maximum temperature threshold
# start=start year for the timeframe of the relevant data (likely 2000 or 2010)
# finish=finish year for the timeframe of the relevant data (likely 2020)
  
    
  pred_persp = list()
  for(j in 1:length(sp))
  {
    pred_persp[[sp[j]]] = c()
  }
  
  for(j in 1:(finish-start + 1))
  {
  # access the temperature data for those points for the relevant timeframe calculate the temperature and flow related predictor variables:
  #1. average maximum number of days exceeding maximum temperature threshold in a year
  #2. average maximum number of days going below minimum temperature threshold in a year
  #3-11. average growing degree days by 90, 120, 150 Julian days with
  #5,8, and 10 degree celsius as base temp.
    
  #12. BFI= base flow index
  #13-14. average date and amount of annual maximum flow
  #15-16. average date and amount of annual minimum flow
    print(j)
    yr = (j-1) + start
    tempfilename = sprintf('G:/My Drive/research/sdm_modeling/gis/wbm/ohio_basin/runs/RiverTemperature_%d.csv',yr)
    tempd =fread(tempfilename,header=TRUE)
    print('tempd read')
    names(tempd)[1] = 'ID'
    flowfilename = sprintf('G:/My Drive/research/sdm_modeling/gis/wbm/ohio_basin/runs/Discharge_%d.csv',yr)
    flowd = fread(flowfilename, header=TRUE)
    print('flowd read')
    names(flowd)[1] = 'ID'
    
    for(i in 1:length(sp))
    {
      # call in the species occ pts with cellids
      name = sp[i]
      tmin = tmins[i]
      tmax = tmaxs[i]
      
      # **spdata could mean species occurrence points or projection area.
      if(occ_or_proj)
      {
        spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/%s_wcomid.csv',name)
        spdata = read.csv(spfilename)
        spdata = spdata$wbmIDOhio[!is.na(spdata$wbmIDOhio)]  
        relID = spdata
      } else{
        projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/%s_projarea.csv",name)
        projpt = read.csv(projfilename)
        projpt = projpt$wbmIDOhio[!is.na(projpt$wbmIDOhio)]  
        relID = projpt
      }
      print('spdata loaded')
      
      # extract the temperature and flow value for the relevant points
      tempd = tempd[which(tempd$ID %in% relID),]
      if(length(which(tempd$ID %in% relID))!=length(unique(relID)))   # make sure all the relID pts are in the temperature data (tempd)
      {
        print("WARNING: NOT ALL THE POINTS THAT NEED TO BE ASSESSED ARE IN THE TEMPERATURE DATASET!!")
      }
      flowd = flowd[which(flowd$ID %in% relID),]
      if(length(which(tempd$ID %in% relID))!=length(unique(relID)))   # make sure all the relID pts are in the flow data (flowd)
      {
        print("WARNING: NOT ALL THE POINTS THAT NEED TO BE ASSESSED ARE IN THE FLOW DATASET!!")
      }
      print('temp/flow data extracted for relevant points')
      # calculate 1. for the year j.
      mat = cbind(as.matrix(tempd[,2:ncol(tempd)])>tmax,rep(0,nrow(tempd)))
      cum = apply(mat,1,cumsum)
      numday_above_tmax = apply(cum,2,function(x) unique(x[duplicated(x)]))
      if(class(numday_above_tmax)=='list')
      {
        numday_above_tmax = unlist(lapply(numday_above_tmax,function(x) max(x[2:length(x)] - x[1:(length(x)-1)]))) # Average maximum number of days in a year that exceeds tmax
        numday_above_tmax[which(is.na(numday_above_tmax))] = 0
      }
      
      
      # calculate 2. for the year j.
      mat= cbind(as.matrix(tempd[,2:ncol(tempd)])<tmin,rep(0,nrow(tempd)))
      numday_below_tmin = apply(mat,1,sum)
      #numday_below_tmin = apply(cum,2,function(x) unique(x[duplicated(x)]))
      #if(class(numday_below_tmin)=='list')
      #{
      #  numday_below_tmin = unlist(lapply(numday_below_tmin,function(x) max(x[2:length(x)] - x[1:(length(x)-1)]))) # Average maximum number of days in a year that exceeds tmin
      #  numday_below_tmin[which(is.na(numday_below_tmin))] = 0
      #}
      
      # calculate 3-11 for the year j.
      dd90_5c =  apply(as.matrix(tempd[,2:(90+1)])-5,1,function(x) sum(x[which(x>=0)])) # degree day by 90 with 5C as base temp
      dd90_8c =  apply(as.matrix(tempd[,2:(90+1)])-8,1,function(x) sum(x[which(x>=0)])) # degree day by 90 with 8C as base temp
      dd90_10c =  apply(as.matrix(tempd[,2:(90+1)])-10,1,function(x) sum(x[which(x>=0)])) # degree day by 90 with 10C as base temp
      dd120_5c = apply(as.matrix(tempd[,2:(120+1)])-5,1,function(x) sum(x[which(x>=0)])) # degree day by 120
      dd120_8c = apply(as.matrix(tempd[,2:(120+1)])-8,1,function(x) sum(x[which(x>=0)])) # degree day by 120
      dd120_10c = apply(as.matrix(tempd[,2:(120+1)])-10,1,function(x) sum(x[which(x>=0)])) # degree day by 120
      dd150_5c = apply(as.matrix(tempd[,2:(150+1)])-5,1,function(x) sum(x[which(x>=0)])) # degree day by 150
      dd150_8c = apply(as.matrix(tempd[,2:(150+1)])-8,1,function(x) sum(x[which(x>=0)])) # degree day by 150
      dd150_10c = apply(as.matrix(tempd[,2:(150+1)])-10,1,function(x) sum(x[which(x>=0)])) # degree day by 150
      
      # calculate 13-16 for the year j
      maxflow = apply(as.matrix(flowd[,2:ncol(flowd)]),1,max)
      maxflowdate = apply(as.matrix(flowd[,2:ncol(flowd)]),1,function(x) min(which(x==max(x))))
      minflow = apply(as.matrix(flowd[,2:ncol(flowd)]),1,min)
      minflowdate = apply(as.matrix(flowd[,2:ncol(flowd)]),1,function(x) min(which(x==min(x))))
      avgflow = apply(as.matrix(flowd[,2:ncol(flowd)]),1,mean)
      maxminflowdiff = maxflow - minflow
      print('predictors calculated')
      # make the data frame with all the predictor variables in the list under the species' name
      if(j==1){
        pred_persp[[name]] = data.frame(wbmIDOhio=flowd$ID,numday_above_tmax, numday_below_tmin, dd90_5c, dd90_8c, dd90_10c, dd120_5c, dd120_8c, dd120_10c, dd150_5c, dd150_8c, dd150_10c, maxflow, maxflowdate, minflow,
                                        minflowdate,avgflow,maxminflowdiff)
      }else{ # add the data if there's data in there already to ultimately calculate the average rate per yr.
        pred_persp[[name]][,c('numday_above_tmax', 'numday_below_tmin', 'dd90_5c', 'dd90_8c', 'dd90_10c', 'dd120_5c', 'dd120_8c', 'dd120_10c', 'dd150_5c', 'dd150_8c', 'dd150_10c', 'maxflow', 'maxflowdate', 'minflow', 'minflowdate','avgflow','maxminflowdiff')] =
          pred_persp[[name]][,c('numday_above_tmax', 'numday_below_tmin', 'dd90_5c', 'dd90_8c', 'dd90_10c', 'dd120_5c', 'dd120_8c', 'dd120_10c', 'dd150_5c', 'dd150_8c', 'dd150_10c', 'maxflow', 'maxflowdate', 'minflow', 'minflowdate','avgflow','maxminflowdiff')] +
          data.frame(numday_above_tmax, numday_below_tmin, dd90_5c, dd90_8c, dd90_10c, dd120_5c, dd120_8c, dd120_10c, dd150_5c, dd150_8c, dd150_10c , maxflow, maxflowdate, minflow, minflowdate,avgflow,maxminflowdiff)
      }
      print('predictor for the year added to the dataframe')
    }
    print(sprintf('year %d done',yr))
  }
  #write out the calculated temperature & flow related predictor
  for(i in 1:length(sp))
  {
    pred_persp[[sp[i]]][,c('numday_above_tmax', 'numday_below_tmin', 'dd90_5c', 'dd90_8c', 'dd90_10c', 'dd120_5c', 'dd120_8c', 'dd120_10c', 'dd150_5c', 'dd150_8c', 'dd150_10c', 'maxflow', 'maxflowdate', 'minflow', 'minflowdate','avgflow','maxminflowdiff')] =
      pred_persp[[sp[i]]][,c('numday_above_tmax', 'numday_below_tmin', 'dd90_5c', 'dd90_8c', 'dd90_10c', 'dd120_5c', 'dd120_8c', 'dd120_10c', 'dd150_5c', 'dd150_8c', 'dd150_10c', 'maxflow', 'maxflowdate', 'minflow', 'minflowdate','avgflow','maxminflowdiff')]/
      (finish - start + 1) # divide by data frame by number of years to get average.
    if(occ_or_proj)
    {
      spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',name)
      spdata = read.csv(spfilename)
      #match(pred_persp[[sp[i]]]$flowd.ID,spdata$wbmIDOhio)
      #match(spdata$wbmIDOhio,pred_persp[[sp[i]]]$flowd.ID)
      t = merge(spdata, pred_persp[[sp[i]]],by='wbmIDOhio') 
      ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/%s.csv',sp[i])
      write.csv(t,ofilename,row.names=FALSE)  
    } else {
      projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/%s_projarea.csv",name)
      projdata = read.csv(projfilename)
      t = merge(projdata,pred_persp[[sp[i]]],by='wbmIDOhio')
      ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/%s_projarea.csv',sp[i])
      write.csv(t,ofilename,row.names=FALSE)
    }
  }
}

