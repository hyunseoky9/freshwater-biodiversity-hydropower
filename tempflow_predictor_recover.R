# recover temperature and flow data that's specific to species from projection 
# area built from processed occurrence points.
# recover temperature and flow data that's specific to species from projection 
# area built from processed occurrence points.
library(data.table)
library(dplyr)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
iidx = 1:nrow(spnamedata)
probidx = c(43,315,273,522)
#probidx =c(150,313,349,484,492,553)
iidx = iidx[-probidx]

for(i in c(202,243,263,266))
{
  newfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  oldfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/projareab4bigger_restriction/%s_projarea.csv',spnamedata$name[i])
  newproj = fread(newfilename)
  if(length(which(names(newproj)=='huc2'))==2)
  {
    newproj = newproj[,-ncol(newproj)]
  }
  oldproj = fread(oldfilename)
  oldproj$copiedfromoldproj = 1
  oldproj_neccessity = oldproj %>% select("comid","numday_above_tmax","numday_below_tmin","dd90_5c","dd90_8c","dd90_10c","dd120_5c","dd120_8c","dd120_10c","dd150_5c","dd150_8c","dd150_10c","maxflow","maxflowdate","minflow","minflowdate","avgflow","maxminflowdiff","avgtemp","copiedfromoldproj")
  newproj = merge(newproj, oldproj_neccessity, 
             by.x = "comid", by.y = "comid", 
             all.x = TRUE, all.y = FALSE,sort=FALSE)
  write.csv(newproj,newfilename,row.names=FALSE)
  print(i)
}













#garbage
library(data.table)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
iidx = 1:nrow(spnamedata)
iidx = iidx[-151]
for(i in iidx)
{
  oldfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/temp2')
  newfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp')
  oldproj = fread(oldfilename)
  newproj = fread(newfilename)
  
  # make columns 
  newproj$numday_above_tmax= NA
  newproj$numday_below_tmin= NA
  newproj$dd90_5c= NA
  newproj$dd90_8c= NA
  newproj$dd90_10c= NA
  newproj$dd120_5c= NA
  newproj$dd120_8c= NA
  newproj$dd120_10c= NA
  newproj$dd150_5c= NA
  newproj$dd150_8c= NA
  newproj$dd150_10c= NA
  newproj$maxflow= NA
  newproj$maxflowdate= NA
  newproj$minflow= NA
  newproj$minflowdate= NA
  newproj$avgflow= NA
  newproj$maxminflowdiff = NA
  newproj$avgtemp = NA
  newproj$copiedfromoldproj = 0
  # find which comids are in old proj
  oldproj_idx = apply(as.matrix(newproj$comid),1,function(x) which(oldproj$comid == x))
  newproj_idx = which(!is.na(oldproj_idx))
  oldproj_idx = oldproj_idx[which(!is.na(oldproj_idx))]
  # copy temp and flow related predictors from shared comids from old projection area
  # to new projection area.
  newproj[newproj_idx,
          c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c',
                        'dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c',
                        'dd150_10c','maxflow','maxflowdate','minflow','minflowdate',
                        'avgflow','maxminflowdiff','avgtemp')] =
  oldproj[oldproj_idx,
          c('numday_above_tmax','numday_below_tmin','dd90_5c','dd90_8c',
            'dd90_10c','dd120_5c','dd120_8c','dd120_10c','dd150_5c','dd150_8c',
            'dd150_10c','maxflow','maxflowdate','minflow','minflowdate',
            'avgflow','maxminflowdiff','avgtemp')] 
  newproj$copiedfromoldproj[newproj_idx] = 1
  write.csv(newproj,newfilename,row.names=FALSE)
}

