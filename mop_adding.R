setwd('G:/My Drive/research/sdm_modeling/spdata')
spdata = read.csv('./comprehensive_sp_info.csv')
 
ryanfilename = 'G:/My Drive/research/sdm_modeling/dam data/ryan.csv'
ryan = read.csv(ryanfilename)
ryan = ryan[which(grepl('Operational',ryan$HMR_Stat)),] # only operational dams
ryan = ryan[-which(duplicated(ryan$comid)),]# get rid of duplicate comids
ryan = ryan[-which(is.na(ryan$comid)),] # get rid of na comids

occ_or_proj = 0
  for(i in 1:18)
  {
    if(occ_or_proj)
    {
      spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spdata$name[i])
      sp = read.csv(spfilename)
    } else{
      #projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens_projarea.csv")
      projfilename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/temp/huc%d.csv",i)
      sp = read.csv(projfilename)
    }
    sp$mop = NA
    idx=which(!is.na(sp$udd2))
    sp$mop[idx] = ryan$Pt_Mode[match(sp$ud_ehaID[idx],ryan$EHA_PtID)]
    if(occ_or_proj)
    {
      write.csv(sp,spfilename,row.names=FALSE)
    } else {
      write.csv(sp,projfilename,row.names= FALSE)
    }
  }




ohioproj = read.csv(projfilename)
filename = sprintf("G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv",5)
huc5proj = read.csv(filename)
idx