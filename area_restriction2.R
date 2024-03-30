# area restriction for species with increasing huc unit method. 
# The method chooses the area extent by choosing the largest huc unit upto 6 digit unit
# that can encompass all the presence points. if presence points are across 
# multiple 6 digit huc units, increase the spatial extent through adding more 6 digit unit. 
filename = 'G:/My Drive/research/sdm_modeling/gis/nhdv2_to_wbd_crosswalks/CrosswalkTable_NHDplus_HU12.csv'
com2huc = read.csv(filename)
dim(com2huc)
names(com2huc)
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
multi6 = c()
for ( i in 1:nrow(spnamedata))
{
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  spdata = read.csv(spdatafilename)
  # get corresponding huc units for every occpts
  sphuc = com2huc$HUC_12[apply(as.matrix(spdata$comid),1,function(x) which(com2huc$FEATUREID==x))] # match comid to huc units (not needed anymore cuz they're already matched in the data)
  sphuc = sphuc[which(sphuc>0)] # rid -9999's
  sphucstr = as.character(sphuc) # turn it into string
  idx = which(nchar(sphucstr)==11) # put 0 in front of hucs units who's 2-digit unit is single digit. 
  sphucstr[idx] = paste('0',sphucstr[idx],sep="")
  
  sp_projarea = data.frame() # area restricted species projection area empty medium.
  
  # check if there are multiple 6digit huc units.
  huc6 = substr(sphucstr,1,6)
  
  if(length(unique(huc6))==1)
  {
    print('single unit6')
    # if there's only one 6digit huc, see if there are multiple 8 digit hucs.
    huc8 = substr(sphucstr,1,8)
    
    if(length(unique(huc8))==1)
    {
      print('single unit8')
      # if there's only one 8digit huc, see if there are multiple 10 digit hucs.
      huc10 = substr(sphucstr,1,10)
      
      if(length(unique(huc10))==1)
      {
        print('single unit10')
        huc12 = substr(sphucstr,1,12)
        scale = 12 # scale of huc unit. there's multiple huc 6's, so the scale is 6 here.
        # get the comids in these huc6's
        relhuc12 = as.numeric(unique(huc12))
        idx=apply(as.matrix(relhuc12),1,function(x) which(com2huc$HUC_12==x))
        #sapply(idx,function(x) sum(is.na(x)))
        comid_table = com2huc[unlist(idx),2:3]
        # get the predictors data from projection area by huc2s.
        relhuc2 = sort(as.numeric(unique(substr(unique(huc12),1,2)))) # relevant huc2s.
        for(j in 1:length(relhuc2))
        {
          huc2 = relhuc2[j]
          filename3 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',relhuc2[j])
          huc2D = read.csv(filename3)
          comid = comid_table$FEATUREID[which(floor(comid_table$HUC_12/10^10)==relhuc2[j])]
          if(j==1)
          {
            sp_projarea = cbind(huc2D[which(huc2D$comid %in% comid),],huc2)
          } else
          {
            sp_projarea = rbind(sp_projarea, cbind(huc2D[which(huc2D$comid %in% comid),],huc2))
          }
        }          
      } else {
        scale = 10 # scale of huc unit. there's multiple huc 6's, so the scale is 6 here.
        # get the comids in these huc6's
        relhuc10 = as.numeric(unique(huc10))
        idx=apply(as.matrix(relhuc10),1,function(x) which(floor(com2huc$HUC_12/10^2)==x))
        #sapply(idx,function(x) sum(is.na(x)))
        comid_table = com2huc[unlist(idx),2:3]
        # get the predictors data from projection area by huc2s.
        relhuc2 = sort(as.numeric(unique(substr(unique(huc10),1,2)))) # relevant huc2s.
        for(j in 1:length(relhuc2))
        {
          huc2 = relhuc2[j]
          filename3 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',relhuc2[j])
          huc2D = read.csv(filename3)
          comid = comid_table$FEATUREID[which(floor(comid_table$HUC_12/10^10)==relhuc2[j])]
          if(j==1)
          {
            sp_projarea = cbind(huc2D[which(huc2D$comid %in% comid),],huc2)
          } else
          {
            sp_projarea = rbind(sp_projarea, cbind(huc2D[which(huc2D$comid %in% comid),],huc2))
          }
        }
      }
    } else
    {
      scale = 8 # scale of huc unit. there's multiple huc 6's, so the scale is 6 here.
      # get the comids in these huc6's
      relhuc8 = as.numeric(unique(huc8))
      idx=apply(as.matrix(relhuc8),1,function(x) which(floor(com2huc$HUC_12/10^4)==x))
      #sapply(idx,function(x) sum(is.na(x)))
      comid_table = com2huc[unlist(idx),2:3]
      # get the predictors data from projection area by huc2s.
      relhuc2 = sort(as.numeric(unique(substr(unique(huc8),1,2)))) # relevant huc2s.
      for(j in 1:length(relhuc2))
      {
        huc2 = relhuc2[j]
        filename3 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',relhuc2[j])
        huc2D = read.csv(filename3)
        comid = comid_table$FEATUREID[which(floor(comid_table$HUC_12/10^10)==relhuc2[j])]
        if(j==1)
        {
          sp_projarea = cbind(huc2D[which(huc2D$comid %in% comid),],huc2)
        } else
        {
          sp_projarea = rbind(sp_projarea, cbind(huc2D[which(huc2D$comid %in% comid),],huc2))
        }
      }
    }
  } else
  {
    print('multiple unit6')
    scale = 6 # scale of huc unit. there's multiple huc 6's, so the scale is 6 here.
    # get the comids in these huc6's
    relhuc6 = as.numeric(unique(huc6))
    idx=apply(as.matrix(relhuc6),1,function(x) which(floor(com2huc$HUC_12/10^6)==x))
    #sapply(idx,function(x) sum(is.na(x)))
    comid_table = com2huc[unlist(idx),2:3]
    # get the predictors data from projection area by huc2s.
    relhuc2 = sort(as.numeric(unique(substr(unique(huc6),1,2))))
    for(j in 1:length(relhuc2))
    {
      huc2 = relhuc2[j]
      filename3 = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_huc/huc%d.csv',relhuc2[j])
      huc2D = read.csv(filename3)
      comid = comid_table$FEATUREID[which(floor(comid_table$HUC_12/10^10)==relhuc2[j])]
      if(j==1)
      {
        sp_projarea = cbind(huc2D[which(huc2D$comid %in% comid),],huc2)
      } else
      {
        sp_projarea = rbind(sp_projarea, cbind(huc2D[which(huc2D$comid %in% comid),],huc2))
      }
    }
  }
  ofilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/%s_projarea.csv',spnamedata$name[i])
  write.csv(sp_projarea,ofilename,row.names=FALSE)
  print(sprintf('%d/%d done',i,nrow(spnamedata)))
}



