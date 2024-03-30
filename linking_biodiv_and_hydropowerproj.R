# in the ferc doc, add how many anadromous, game, listed, and any species are in the project's huc 8
rm(list=ls())

#1. get biodiv. related info in to ferc docs details.csv
{
filename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spdata = read.csv(filename)
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename2)

filename3 = 'G:/My Drive/research/sdm_modeling/spdata/listed_byhuc8_studyspecies.csv'
listed = read.csv(filename3)
filename4 = 'G:/My Drive/research/sdm_modeling/spdata/non-indigenous_byhuc8_studyspecies_(from NS data).csv'
nonnative = read.csv(filename4)
filename4.2 = 'G:/My Drive/research/sdm_modeling/spdata/diadromy_byhuc8_studyspecies.csv'
diadromy = read.csv(filename4.2)
dia_sp = spdata$name[which(spdata$diadromous.partial==1 | spdata$diadromous.partial=='partial')]

filename5 = ''
spcountwd = 'G:/My Drive/research/sdm_modeling/sdm_results/spcount/GCM_average'
turnover_filename = sprintf('%s/turnover.csv',spcountwd)
currentpri_filename = sprintf('%s/df2_total_spcount_nsd0.4_currentpri.csv',spcountwd)
currentres_filename = sprintf('%s/df2_total_spcount_nsd0.4_currentres.csv',spcountwd)
futurepri_filename = sprintf('%s/df2_total_spcount_nsd0.4_futurepri.csv',spcountwd)
futureres_filename = sprintf('%s/df2_total_spcount_nsd0.4_futureres.csv',spcountwd)
turnover = read.csv(turnover_filename)
currentprid = read.csv(currentpri_filename)
currentresd = read.csv(currentres_filename)
futureprid = read.csv(futurepri_filename)
futureresd = read.csv(futureres_filename)
# cut out total number of species column
currentprid = currentprid[,1:(ncol(currentprid)-1)]
currentresd = currentresd[,1:(ncol(currentresd)-1)]
futureprid = futureprid[,1:(ncol(futureprid)-1)]
futureresd = futureresd[,1:(ncol(futureresd)-1)]
}
{
fercd$totalspnum_cp = NA
fercd$totalspnum_cr = NA
fercd$totalspnum_fp = NA
fercd$totalspnum_fr = NA
fercd$listedspnum_cp = NA
fercd$listedspnum_cr = NA
fercd$listedspnum_fp = NA
fercd$listedspnum_fr = NA
fercd$nonnativespnum_cp = NA
fercd$nonnativespnum_cr = NA
fercd$nonnativespnum_fp = NA
fercd$nonnativespnum_fr = NA
fercd$gamespnum_cp = NA
fercd$gamespnum_cr = NA
fercd$gamespnum_fp = NA
fercd$gamespnum_fr = NA
fercd$turnover.cr2fr = NA
fercd$turnover.cr2cp = NA
fercd$turnover.cp2fp = NA
fercd$threatenedspnum_cp = NA
fercd$threatenedspnum_cr = NA
fercd$threatenedspnum_fp = NA
fercd$threatenedspnum_fr = NA
fercd$diadromous.spnum_cp = NA
fercd$diadromous.spnum_cr = NA
fercd$diadromous.spnum_fp = NA
fercd$diadromous.spnum_fr = NA
fercd$diadromous_salmonid_cp = NA
fercd$diadromous_salmonid_cr = NA
fercd$diadromous_salmonid_fp = NA
fercd$diadromous_salmonid_fr = NA
fercd$diadromous2.spnum_cp = NA
fercd$diadromous2.spnum_cr = NA
fercd$diadromous2.spnum_fp = NA
fercd$diadromous2.spnum_fr = NA
fercd$migratory.spnum_cp = NA
fercd$migratory.spnum_cr = NA
fercd$migratory.spnum_fp = NA
fercd$migratory.spnum_fr = NA
fercd$fishspnum_cp = NA
fercd$fishspnum_cr = NA
fercd$fishspnum_fp = NA
fercd$fishspnum_fr = NA
fercd$musselspnum_cp = NA
fercd$musselspnum_cr = NA
fercd$musselspnum_fp = NA
fercd$musselspnum_fr = NA
fercd$invmusselspnum_cp = NA
fercd$invmusselspnum_cr = NA
fercd$invmusselspnum_fp = NA
fercd$invmusselspnum_fr = NA
fercd$natmusselspnum_cp = NA
fercd$natmusselspnum_cr = NA
fercd$natmusselspnum_fp = NA
fercd$natmusselspnum_fr = NA
fercd$coldwaterspnum_cp = NA
fercd$coldwaterspnum_cr = NA
fercd$coldwaterspnum_fp = NA
fercd$coldwaterspnum_fr = NA
fercd$warmwaterspnum_cp = NA
fercd$warmwaterspnum_cr = NA
fercd$warmwaterspnum_fp = NA
fercd$warmwaterspnum_fr = NA
fercd$coolwaterspnum_cp = NA
fercd$coolwaterspnum_cr = NA
fercd$coolwaterspnum_fp = NA
fercd$coolwaterspnum_fr = NA
fercd$nonnativemusselspnum_cp = NA
fercd$nonnativemusselspnum_cr = NA
fercd$nonnativemusselspnum_fp = NA
fercd$nonnativemusselspnum_fr = NA
fercd$nativemusselspnum_cp = NA
fercd$nativemusselspnum_cr = NA
fercd$nativemusselspnum_fp = NA
fercd$nativemusselspnum_fr = NA
}
for( i in 1:nrow(fercd))
{
  if(is.na(fercd$huc8[i]))
  {
    next     
  }
  if(grepl('/',fercd$huc8[i]))
  {
    projhuc8 =  as.numeric(unlist(strsplit(fercd$huc8[i],'/')))
    hucmatchidx = which(currentprid$HUC8 %in% projhuc8)
  } else {
    projhuc8 = as.numeric(fercd$huc8[i])
    hucmatchidx = which(currentprid$HUC8==projhuc8)
    if(length(hucmatchidx)==0) # proj huc not in my huc8 data
    {
      next
    }
  }
  # cp=current pri, cr=current res, fp=future pri, fr=future res
  # get which species lives at the project's huc8 according to the SDM result.
  if(length(hucmatchidx>1))
  {
    sp_proj_cp = gsub('\\.',' ',names(currentprid)[which(colSums(currentprid[hucmatchidx,2:ncol(currentprid)])>0) + 1])
    sp_proj_cr = gsub('\\.',' ',names(currentresd)[which(colSums(currentresd[hucmatchidx,2:ncol(currentresd)])>0) + 1])
    sp_proj_fp = gsub('\\.',' ',names(futureprid)[which(colSums(futureprid[hucmatchidx,2:ncol(futureprid)])>0) + 1])
    sp_proj_fr = gsub('\\.',' ',names(futureresd)[which(colSums(futureresd[hucmatchidx,2:ncol(futureresd)])>0) + 1])
  } else {
    sp_proj_cp = gsub('\\.',' ',names(currentprid)[which(currentprid[hucmatchidx,]==1)])  # species names in proj in current pri scenario
    sp_proj_cr = gsub('\\.',' ',names(currentresd)[which(currentresd[hucmatchidx,]==1)])
    sp_proj_fp = gsub('\\.',' ',names(futureprid)[which(futureprid[hucmatchidx,]==1)])
    sp_proj_fr = gsub('\\.',' ',names(futureresd)[which(futureresd[hucmatchidx,]==1)])
  }
  
  # get number of species
  fercd$totalspnum_cp[i] = length(sp_proj_cp)
  fercd$totalspnum_cr[i] = length(sp_proj_cr)
  fercd$totalspnum_fp[i] = length(sp_proj_fp)
  fercd$totalspnum_fr[i] = length(sp_proj_fr)
  
  # get number of listed species
  hucmatchidx_listed = which(listed$HUC8 %in% projhuc8)
  if(length(hucmatchidx_listed)>1)
  {
    listed_sp_athuc8 = names(listed)[which(colSums(listed[hucmatchidx_listed,2:ncol(listed)])>0) + 1]
    listed_sp_athuc8 = gsub('\\.',' ',listed_sp_athuc8)
  } else 
  {
    listed_sp_athuc8 = names(listed)[which(listed[hucmatchidx_listed,2:ncol(listed)]==1)]
    listed_sp_athuc8 = gsub('\\.',' ',listed_sp_athuc8)
  }
  fercd$listedspnum_cp[i] = length(which(!is.na(match(sp_proj_cp,listed_sp_athuc8))))
  fercd$listedspnum_cr[i] = length(which(!is.na(match(sp_proj_cr,listed_sp_athuc8))))
  fercd$listedspnum_fp[i] = length(which(!is.na(match(sp_proj_fp,listed_sp_athuc8))))
  fercd$listedspnum_fr[i] = length(which(!is.na(match(sp_proj_fr,listed_sp_athuc8))))
  
  # get number of threatened species classified by NatureServe (G rank)
  fercd$threatenedspnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$threatened==1)]))
  fercd$threatenedspnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$threatened==1)]))
  fercd$threatenedspnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$threatened==1)]))
  fercd$threatenedspnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$threatened==1)]))
  
  # get number of diadromous species 
  fercd$diadromous.spnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$diadromous==1)]))
  fercd$diadromous.spnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$diadromous==1)]))
  fercd$diadromous.spnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$diadromous==1)]))
  fercd$diadromous.spnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$diadromous==1)]))
  
  # get number of diadromous species while accounting for partially diadromous species.
  dia_sp_inprojhuc8 = names(which(colSums(diadromy[which(!is.na(match(diadromy$HUC8,projhuc8))),])>0))
  dia_sp_inprojhuc8 = dia_sp_inprojhuc8[2:length(dia_sp_inprojhuc8)]
  dia_sp_inprojhuc8 = gsub('\\.',' ',dia_sp_inprojhuc8)  
  fercd$diadromous2.spnum_cp[i] = length(which(sp_proj_cp %in% dia_sp_inprojhuc8))
  fercd$diadromous2.spnum_cr[i] = length(which(sp_proj_cr %in% dia_sp_inprojhuc8))
  fercd$diadromous2.spnum_fp[i] = length(which(sp_proj_fp %in% dia_sp_inprojhuc8))
  fercd$diadromous2.spnum_fr[i] = length(which(sp_proj_fr %in% dia_sp_inprojhuc8))
  
  
  # get number of diadromous salmonid species
  fercd$diadromous_salmonid_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$anadromous_salmonids==1)]))
  fercd$diadromous_salmonid_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$anadromous_salmonids==1)]))
  fercd$diadromous_salmonid_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$anadromous_salmonids==1)]))
  fercd$diadromous_salmonid_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$anadromous_salmonids==1)]))
  # get number of migrating species
  fercd$migratory.spnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$migratory==1)]))
  fercd$migratory.spnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$migratory==1)]))
  fercd$migratory.spnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$migratory==1)]))
  
  # get number of fish species
  fercd$fishspnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$mussel_or_fish==1)]))
  fercd$fishspnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$mussel_or_fish==1)]))
  fercd$fishspnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$mussel_or_fish==1)]))
  fercd$fishspnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$mussel_or_fish==1)]))
  
  # get number of mussel species 
  fercd$musselspnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$mussel_or_fish==0)]))
  fercd$musselspnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$mussel_or_fish==0)]))
  fercd$musselspnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$mussel_or_fish==0)]))
  fercd$musselspnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$mussel_or_fish==0)]))
  
  # get number of cold-water species
  fercd$coldwaterspnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$thermal_pref==0)]))
  fercd$coldwaterspnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$thermal_pref==0)]))
  fercd$coldwaterspnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$thermal_pref==0)]))
  fercd$coldwaterspnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$thermal_pref==0)]))
  
  # get number of warm-water species
  fercd$warmwaterspnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$thermal_pref==1)]))
  fercd$warmwaterspnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$thermal_pref==1)]))
  fercd$warmwaterspnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$thermal_pref==1)]))
  fercd$warmwaterspnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$thermal_pref==1)]))
  
  # get number of cool-water species
  fercd$coolwaterspnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$thermal_pref==2)]))
  fercd$coolwaterspnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$thermal_pref==2)]))
  fercd$coolwaterspnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$thermal_pref==2)]))
  fercd$coolwaterspnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$thermal_pref==2)]))
  
  # get number of nonnative sp.
  hucmatchidx_nonnative = which(nonnative$HUC8 %in% projhuc8)
  if(length(hucmatchidx_nonnative)>1)
  {
    nonnative_sp_athuc8 = names(nonnative)[which(colSums(nonnative[hucmatchidx_nonnative,2:ncol(nonnative)])>0) + 1]
    nonnative_sp_athuc8 = gsub('\\.',' ',nonnative_sp_athuc8)
  } else {
    nonnative_sp_athuc8 = names(nonnative)[which(nonnative[hucmatchidx_nonnative,2:ncol(nonnative)]==1)]
    nonnative_sp_athuc8 = gsub('\\.',' ',nonnative_sp_athuc8)
  }
  fercd$nonnativespnum_cp[i] = length(which(!is.na(match(sp_proj_cp,nonnative_sp_athuc8))))
  fercd$nonnativespnum_cr[i] = length(which(!is.na(match(sp_proj_cr,nonnative_sp_athuc8))))
  fercd$nonnativespnum_fp[i] = length(which(!is.na(match(sp_proj_fp,nonnative_sp_athuc8))))
  fercd$nonnativespnum_fr[i] = length(which(!is.na(match(sp_proj_fr,nonnative_sp_athuc8))))

  # get total invasive mussel species count and native mussel species count.
  
  musselsp_proj_cp = sp_proj_cp[which(sp_proj_cp %in% spdata$name[which(spdata$mussel_or_fish==0)])]
  musselsp_proj_cr = sp_proj_cr[which(sp_proj_cr %in% spdata$name[which(spdata$mussel_or_fish==0)])]
  musselsp_proj_fp = sp_proj_fp[which(sp_proj_fp %in% spdata$name[which(spdata$mussel_or_fish==0)])]
  musselsp_proj_fr = sp_proj_fr[which(sp_proj_fr %in% spdata$name[which(spdata$mussel_or_fish==0)])]
  fercd$nonnativemusselspnum_cp[i] = length(which(nonnative_sp_athuc8[match(sp_proj_cp,nonnative_sp_athuc8)] %in% musselsp_proj_cp))
  fercd$nonnativemusselspnum_cr[i] = length(which(nonnative_sp_athuc8[match(sp_proj_cr,nonnative_sp_athuc8)] %in% musselsp_proj_cr))
  fercd$nonnativemusselspnum_fp[i] = length(which(nonnative_sp_athuc8[match(sp_proj_fp,nonnative_sp_athuc8)] %in% musselsp_proj_fp))
  fercd$nonnativemusselspnum_fr[i] = length(which(nonnative_sp_athuc8[match(sp_proj_fr,nonnative_sp_athuc8)] %in% musselsp_proj_fr))
  fercd$nativemusselspnum_cp[i] = fercd$musselspnum_cp[i] - fercd$nonnativemusselspnum_cp[i]
  fercd$nativemusselspnum_cr[i] = fercd$musselspnum_cr[i] - fercd$nonnativemusselspnum_cr[i]
  fercd$nativemusselspnum_fp[i] = fercd$musselspnum_fp[i] - fercd$nonnativemusselspnum_fp[i]
  fercd$nativemusselspnum_fr[i] = fercd$musselspnum_fr[i] - fercd$nonnativemusselspnum_fr[i]
  
  # get total number of game sp.
  fercd$gamespnum_cp[i] = length(which(sp_proj_cp %in% spdata$name[which(spdata$game_Elman==1)] & !(sp_proj_cp %in% listed_sp_athuc8)))
  fercd$gamespnum_cr[i] = length(which(sp_proj_cr %in% spdata$name[which(spdata$game_Elman==1)] & !(sp_proj_cr %in% listed_sp_athuc8)))
  fercd$gamespnum_fp[i] = length(which(sp_proj_fp %in% spdata$name[which(spdata$game_Elman==1)] & !(sp_proj_fp %in% listed_sp_athuc8)))
  fercd$gamespnum_fr[i] = length(which(sp_proj_fr %in% spdata$name[which(spdata$game_Elman==1)] & !(sp_proj_fr %in% listed_sp_athuc8)))
  
  # get turnover rate of sp.
  hucmatchidx_turnover = which(turnover$pricur.HUC8 %in% projhuc8)
  if(length(hucmatchidx_nonnative)>1)
  {
    fercd$turnover.cr2fr[i] = mean(na.omit(turnover$rescur2resfut[hucmatchidx_turnover]))
    fercd$turnover.cr2cp[i] = mean(na.omit(turnover$rescur2pricur[hucmatchidx_turnover]))
    fercd$turnover.cp2fp[i] = mean(na.omit(turnover$pricur2prifut[hucmatchidx_turnover]))
  } else {
    fercd$turnover.cr2fr[i] = turnover$rescur2resfut[hucmatchidx_turnover]
    fercd$turnover.cr2cp[i] = turnover$rescur2pricur[hucmatchidx_turnover]
    fercd$turnover.cp2fp[i] = turnover$pricur2prifut[hucmatchidx_turnover]
  }
}

ofilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
write.csv(fercd,ofilename,row.names=FALSE)

#2. get measure related info into ferc docs details.csv
rm(list=ls())
{
filename1 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename1)
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/measure_compilation_with_categorization.csv'
measured = read.csv(filename2)
}
{
fercd$tot.c.2022 = NA # total cost of measures
fercd$tot.num.measure = NA # total number of measures
# look at the description in the forloop for the following variables. 
# p.= proprortion of measures related to ...
# p2. = proportion of measures related to ... within freshwater sp. cons. related measures
# c. = total cost of measures (in 2022 dollar) related to ...
# cp. = proportion of total cost of measures related to...
# cp2. = proportion of total cost of measures related to... within freshwater sp. cons. related measures

fercd$p.freshWsp.cons.related = NA
fercd$c.freshWsp.cons.related = NA
fercd$cp.freshWsp.cons.related = NA
fercd$cpermw.freshWsp.cons.related = NA
fercd$p.sp.specific = NA
fercd$p2.sp.specific = NA
fercd$c.sp.specific = NA
fercd$cp.sp.specific = NA
fercd$cp2.sp.specific = NA
fercd$p.direct.specific = NA
fercd$p2.direct.specific = NA
fercd$c.direct.specific = NA
fercd$cp.direct.specific = NA
fercd$cp2.direct.specific = NA
fercd$p.direct.nonspecific = NA
fercd$p2.direct.nonspecific = NA
fercd$c.direct.nonspecific = NA
fercd$cp.direct.nonspecific = NA
fercd$cp2.direct.nonspecific = NA
fercd$p.flow.related = NA
fercd$p2.flow.related = NA
fercd$c.flow.related = NA
fercd$cp.flow.related = NA
fercd$cp2.flow.related = NA
fercd$p.Wquality.related = NA
fercd$p2.Wquality.related = NA
fercd$c.Wquality.related = NA
fercd$cp.Wquality.related = NA
fercd$cp2.Wquality.related = NA
fercd$p.Wtemp.related = NA
fercd$p2.Wtemp.related = NA
fercd$c.Wtemp.related = NA
fercd$cp.Wtemp.related = NA
fercd$cp2.Wtemp.related = NA
fercd$p.WqualityNtemp.related = NA
fercd$p2.WqualityNtemp.related = NA
fercd$c.WqualityNtemp.related = NA
fercd$cp.WqualityNtemp.related = NA
fercd$cp2.WqualityNtemp.related = NA
fercd$p.passage.related = NA
fercd$p2.passage.related = NA
fercd$c.passage.related = NA
fercd$cp.passage.related = NA
fercd$cp2.passage.related = NA
fercd$p.migratingsp.related = NA
fercd$p2.migratingsp.related = NA
fercd$c.migratingsp.related = NA
fercd$cp.migratingsp.related = NA
fercd$cp2.migratingsp.related = NA
fercd$p.migratingsp2.related = NA
fercd$p2.migratingsp2.related = NA
fercd$c.migratingsp2.related = NA
fercd$cp.migratingsp2.related = NA
fercd$cp2.migratingsp2.related = NA
fercd$p.listed.related = NA
fercd$p2.listed.related = NA
fercd$c.listed.related = NA
fercd$cp.listed.related = NA
fercd$cp2.listed.related = NA
fercd$p.game.related = NA
fercd$p2.game.related = NA
fercd$c.game.related = NA
fercd$cp.game.related = NA
fercd$cp2.game.related = NA
fercd$p.mussel.related = NA
fercd$p2.mussel.related = NA
fercd$c.mussel.related = NA
fercd$cp.mussel.related = NA
fercd$cp2.mussel.related = NA
fercd$p.invmussel.related = NA
fercd$p2.invmussel.related = NA
fercd$c.invmussel.related = NA
fercd$cp.invmussel.related = NA
fercd$cp2.invmussel.related = NA
fercd$p.natmussel.related = NA
fercd$p2.natmussel.related = NA
fercd$c.natmussel.related = NA
fercd$cp.natmussel.related = NA
fercd$cp2.natmussel.related = NA
fercd$p.invfish.related = NA
fercd$p2.invfish.related = NA
fercd$c.invfish.related = NA
fercd$cp.invfish.related = NA
fercd$cp2.invfish.related = NA
fercd$p.hatchery.related = NA
fercd$p2.hatchery.related = NA
fercd$c.hatchery.related = NA
fercd$cp.hatchery.related = NA
fercd$cp2.hatchery.related = NA
fercd$p.fishing.related = NA
fercd$p2.fishing.related = NA
fercd$c.fishing.related = NA
fercd$cp.fishing.related = NA
fercd$cp2.fishing.related = NA

}

for( i in 1:nrow(fercd))
{
  projid = fercd$projectID[i]
  rel_measures_idx = which(measured$fercid == projid)
  relm = measured[rel_measures_idx,]
  # total cost measures
  fercd$tot.c.2022[i] = sum(na.omit(relm$annualized.cost.2022))
  # total number of measures
  fercd$tot.num.measure[i] = nrow(relm)
  
  #what proportion of measures are...
  #& how much cost is associated with measures that are...
  
  # freshwater sp conservation related?
  fercd$p.freshWsp.cons.related[i] = length(which(relm$freshWsp.cons.related==1))/nrow(relm)
  fercd$c.freshWsp.cons.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$freshWsp.cons.related==1)]))
  fercd$cp.freshWsp.cons.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$freshWsp.cons.related==1)]))/fercd$tot.c.2022[i]
  fercd$cpermw.freshWsp.cons.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$freshWsp.cons.related==1)]))/fercd$power.capacity.MW.[i]
  
  #	species-specific and specis-targeted?
  fercd$p.sp.specific[i] = length(which(relm$sp.spcific==1))/nrow(relm)
  fercd$p2.sp.specific[i] = length(which(relm$sp.spcific==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.sp.specific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$sp.spcific==1)]))
  fercd$cp.sp.specific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$sp.spcific==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.sp.specific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$sp.spcific==1)]))/fercd$c.freshWsp.cons.related[i]
  
  #directly affecting species persistence & species specific?
  fercd$p.direct.specific[i] = length(which(relm$direct.specific==1))/nrow(relm)
  fercd$p2.direct.specific[i] = length(which(relm$direct.specific==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.direct.specific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$direct.specific==1)]))
  fercd$cp.direct.specific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$direct.specific==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.direct.specific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$direct.specific==1)]))/fercd$c.freshWsp.cons.related[i]
  
  #directly affecting species persistence & non-species specific?
  fercd$p.direct.nonspecific[i] = length(which(relm$direct.nonspecific==1))/nrow(relm)
  fercd$p2.direct.nonspecific[i] = length(which(relm$direct.nonspecific==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.direct.nonspecific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$direct.nonspecific==1)]))
  fercd$cp.direct.nonspecific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$direct.nonspecific==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.direct.nonspecific[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$direct.nonspecific==1)]))/fercd$c.freshWsp.cons.related[i]
  
  #flow related?
  fercd$p.flow.related[i] = length(which(relm$flow.related==1))/nrow(relm)
  fercd$p2.flow.related[i] = length(which(relm$flow.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.flow.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$flow.related==1)]))
  fercd$cp.flow.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$flow.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.flow.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$flow.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # water quality related
  fercd$p.Wquality.related[i] = length(which(relm$Wquality.related==1))/nrow(relm)
  fercd$p2.Wquality.related[i] = length(which(relm$Wquality.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.Wquality.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wquality.related==1)]))
  fercd$cp.Wquality.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wquality.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.Wquality.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wquality.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # water temperature related?
  fercd$p.Wtemp.related[i] = length(which(relm$Wtemp.related==1))/nrow(relm)
  fercd$p2.Wtemp.related[i] = length(which(relm$Wtemp.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.Wtemp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wtemp.related==1)]))
  fercd$cp.Wtemp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wtemp.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.Wtemp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wtemp.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # water quality or temperature related?
  fercd$p.WqualityNtemp.related[i] = length(which(relm$Wtemp.related==1 | relm$Wquality.related==1))/nrow(relm)
  fercd$p2.WqualityNtemp.related[i] = length(which(relm$Wtemp.related==1 | relm$Wquality.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.WqualityNtemp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wtemp.related==1 | relm$Wquality.related==1)]))
  fercd$cp.WqualityNtemp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wtemp.related==1 | relm$Wquality.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.WqualityNtemp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$Wtemp.related==1 | relm$Wquality.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # fish passage related?
  fercd$p.passage.related[i] = length(which(relm$passage.related==1))/nrow(relm)
  fercd$p2.passage.related[i] = length(which(relm$passage.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.passage.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$passage.related==1)]))
  fercd$cp.passage.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$passage.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.passage.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$passage.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # diadromous species related
  fercd$p.migratingsp.related[i] = length(which(relm$migratingsp.related==1))/nrow(relm)
  fercd$p2.migratingsp.related[i] = length(which(relm$migratingsp.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.migratingsp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$migratingsp.related==1)]))
  fercd$cp.migratingsp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$migratingsp.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.migratingsp.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$migratingsp.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # diadromous species related
  fercd$p.migratingsp2.related[i] = length(which(relm$migratingsp_partial.related==1))/nrow(relm)
  fercd$p2.migratingsp2.related[i] = length(which(relm$migratingsp_partial.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.migratingsp2.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$migratingsp_partial.related==1)]))
  fercd$cp.migratingsp2.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$migratingsp_partial.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.migratingsp2.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$migratingsp_partial.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # listed species related
  fercd$p.listed.related[i] = length(which(relm$listed.related==1))/nrow(relm)
  fercd$p2.listed.related[i] = length(which(relm$listed.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.listed.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$listed.related==1)]))
  fercd$cp.listed.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$listed.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.listed.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$listed.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # game species related
  fercd$p.game.related[i] = length(which(relm$game.related==1))/nrow(relm)
  fercd$p2.game.related[i] = length(which(relm$game.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.game.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$game.related==1)]))
  fercd$cp.game.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$game.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.game.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$game.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # mussel.related
  fercd$p.mussel.related[i] = length(which(relm$mussel.related==1))/nrow(relm)
  fercd$p2.mussel.related[i] = length(which(relm$mussel.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.mussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$mussel.related==1)]))
  fercd$cp.mussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$mussel.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.mussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$mussel.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # invmussel.related  
  fercd$p.invmussel.related[i] = length(which(relm$invmussel.related==1))/nrow(relm)
  fercd$p2.invmussel.related[i] = length(which(relm$invmussel.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.invmussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$invmussel.related==1)]))
  fercd$cp.invmussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$invmussel.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.invmussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$invmussel.related==1)]))/fercd$c.freshWsp.cons.related[i]

  # natmussel.related  
  fercd$p.natmussel.related[i] = length(which((relm$invmussel.related==0 | is.na(relm$invmussel.related)) & relm$mussel.related==1))/nrow(relm)
  fercd$p2.natmussel.related[i] = length(which((relm$invmussel.related==0 | is.na(relm$invmussel.related)) & relm$mussel.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.natmussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which((relm$invmussel.related==0 | is.na(relm$invmussel.related)) & relm$mussel.related==1)]))
  fercd$cp.natmussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which((relm$invmussel.related==0 | is.na(relm$invmussel.related)) & relm$mussel.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.natmussel.related[i] = sum(na.omit(relm$annualized.cost.2022[which((relm$invmussel.related==0 | is.na(relm$invmussel.related)) & relm$mussel.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # invfish related  
  fercd$p.invfish.related[i] = length(which(relm$invfish.related==1))/nrow(relm)
  fercd$p2.invfish.related[i] = length(which(relm$invfish.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.invfish.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$invfish.related==1)]))
  fercd$cp.invfish.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$invfish.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.invfish.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$invfish.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # hatchery related.
  fercd$p.hatchery.related[i] = length(which(relm$hatchery.related==1))/nrow(relm)
  fercd$p2.hatchery.related[i] = length(which(relm$hatchery.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.hatchery.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$hatchery.related==1)]))
  fercd$cp.hatchery.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$hatchery.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.hatchery.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$hatchery.related==1)]))/fercd$c.freshWsp.cons.related[i]
  
  # fishing & fishery related  
  fercd$p.fishing.related[i] = length(which(relm$fishing.related==1))/nrow(relm)
  fercd$p2.fishing.related[i] = length(which(relm$fishing.related==1))/length(which(relm$freshWsp.cons.related==1))
  fercd$c.fishing.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$fishing.related==1)]))
  fercd$cp.fishing.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$fishing.related==1)]))/fercd$tot.c.2022[i]
  fercd$cp2.fishing.related[i] = sum(na.omit(relm$annualized.cost.2022[which(relm$fishing.related==1)]))/fercd$c.freshWsp.cons.related[i]
}

ofilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
write.csv(fercd,ofilename,row.names=FALSE)


# get centroid of the respective huc8 the plant is in.


rm(list=ls())
library(sf)
library(tidyverse)

{
  filename1 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
  fercd = read.csv(filename1)
  huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
  huc8conus$HUC8 = as.numeric(huc8conus$HUC8)
}
  
  fercd$huc8centroid_lat = NA # total cost of measures
  fercd$huc8centroid_long = NA # total cost of measures
  centroids <- st_centroid(huc8conus$geometry)
  centroid_coords <- do.call(rbind, centroids) %>% as.data.frame() %>% setNames(c('long','lat'))

  idx = match(fercd$huc8,huc8conus$HUC8)
  fercd$huc8centroid_lat = centroid_coords$lat[idx]
  fercd$huc8centroid_long = centroid_coords$long[idx]
  ofilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
  write.csv(fercd,ofilename,row.names=FALSE)
  
  probcoord = huc8conus$geometry[which(huc8conus$HUC8 %in% c(3050101,3050103,3050104))] %>%
    st_union() %>% st_centroid() %>% {do.call(rbind,.)} %>% as.tibble() %>% setNames(c('long','lat'))
  fercd$huc8centroid_lat[which(grepl('/',fercd$huc8))] = probcoord$lat
  fercd$huc8centroid_long[which(grepl('/',fercd$huc8))] = probcoord$long
  st_centroid  
  
  