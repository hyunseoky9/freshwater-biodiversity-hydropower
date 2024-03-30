#vif calculation
library(usdm)
i=314
spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
spdata = read.csv(spdatafilename)

#modelname <- sprintf("%s_maxent_output_pristinerun",spnamedata$name[i])

# conducting sdm with ohio basin for freshwater darter (Aplodinotus grunniens)
# read in data
{
  #spdatafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens.csv'
  #projectionareafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens_projarea.csv'
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  projectionareafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  sp = read.csv(spdatafilename)
  proj = read.csv(projectionareafilename)
  # get rid of points with low netsymmetricdifference value 
  if(nolownetsymdiff)
  {
    sp = sp[which(sp$wbmID_30sec_netsymdiff>0.4),]
    proj = proj[which(proj$wbmID_30sec_netsymdiff>0.4),]
    if(nrow(sp)<30)
    {
      cat(i)
    }
  }
  sp$udd2[which(is.na(sp$udd2))] = 1000000
  proj$udd2[which(is.na(proj$udd2))] = 1000000
  sp$BFI[which(is.na(sp$BFI))] = 0
  proj$BFI[which(is.na(proj$BFI))] = 0
  # change mop field into integer values (categorical)
  proj$mop[which(proj$mop=='(null)')] = NA
  moptypes = unique(proj$mop)[which(!is.na(unique(proj$mop)))]
  moptypes = data.frame(moptypes = moptypes,code = 1:length(moptypes))
  proj$mop = match(proj[,'mop'],moptypes[,1])
  proj$mop[which(is.na(proj$mop))] = 0
  proj$mop = as.factor(proj$mop)
  
  sp$mop[which(sp$mop=='(null)')] = NA
  moptypes = unique(sp$mop)[which(!is.na(unique(sp$mop)))]
  if(length(moptypes)>0)
  {
    moptypes = data.frame(moptypes = moptypes,code = 1:length(moptypes))
    sp$mop = match(sp[,'mop'],moptypes[,1])
  }
  sp$mop[which(is.na(sp$mop))] = 0
  sp$mop = as.factor(sp$mop)
  
  # presence 
  p = rep(0, length(proj$comid))
  p[which(!is.na(match(proj$comid,sp$comid)))] = 1 # presence
  proj$p = p
  sp$p = 1
}
#making occurence and background data subsets (used later in model evaluation)
#presdata <- filter(sp, get('p') ==1)
bckdata <- filter(proj, get('p') ==0)


# predictor
dataversion = 1
if(dataversion == 1)
{
  bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
                                     numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                     dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                     dd150_10c,maxflow,maxflowdate,minflow,
                                     minflowdate,avgtemp,streamorder))
  spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
                                  numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                                  dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                                  dd150_10c,maxflow,maxflowdate,minflow,
                                  minflowdate,avgtemp,streamorder))
  path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df1',spnamedata$name[i])
} else if(dataversion == 2)
{
  bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
                                     dd90_8c,minflow,maxflowdate,
                                     minflowdate))
  
  
  spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
                                  dd90_8c,minflow,maxflowdate,
                                  minflowdate))
  path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df2',spnamedata$name[i])
} else if(dataversion == 3)
{
  bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
                                     numday_below_tmin,dd120_10c,avgflow,maxflowdate,
                                     minflowdate,streamorder))
  spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
                                  numday_below_tmin,dd120_10c,avgflow,maxflowdate,
                                  minflowdate,streamorder))
  
  #bckdata = data.frame(dplyr::select(bckdata,BFI,udd2,mop,waterbody,numday_above_tmax,
  #                                   numday_below_tmin,avgtemp,avgflow,maxflowdate,
  #                                   minflowdate))
  #spdf = data.frame(dplyr::select(sp,BFI,udd2,mop,waterbody,numday_above_tmax,
  #                                numday_below_tmin,avgtemp,avgflow,maxflowdate,
  #                                minflowdate))
  
  path2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df3',spnamedata$name[i])
}

usdm::vif(bckdata)
vif(bckdata)

