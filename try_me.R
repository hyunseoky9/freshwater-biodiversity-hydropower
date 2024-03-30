gage_wq_drn <- function(inputgages,parameterCd,service){
  tmpdsc <- c()
  
  for(i in 1:length(inputgages)){
    if(length(inputgages[[i]][["GAGE_IDENTIFIER"]]) > 0){
      gages <- inputgages[[i]][["GAGE_IDENTIFIER"]]
      sites <- unique(substr(gages, start = 6, stop = nchar(gages)))
      
      print(paste("Retrieving temperature and discharge for list", i, "of", length(inputgages)))
      tmpdscQ <- try(whatNWISdata(sites = sites,
                                  parameterCd = parameterCd,
                                  service = service))
      if(is.character(tmpdscQ)) tmpdscQ <- NULL
      tmpdsc[[length(tmpdsc) + 1]] <- tmpdscQ
    }
  }
  tmpdscD <- ldply(tmpdsc, data.frame)
  tmpdscD <- tmpdscD[!duplicated(tmpdscD),]
  
  print(paste("Retrieving drainage area..."))
  drnQ <- readNWISsite(siteNumbers = unique(tmpdscD$site_no))
  drnS <- drnQ[!is.na(drnQ$drain_area_va),]
  drnS <- drnS[,c(2,13:15,30)]
  tmpdsc_drn <- merge(tmpdscD, drnS, by = "site_no")
  
  print(paste("Fixing date format..."))
  fixedDates <- data.frame(parse_date_time(tmpdsc_drn$begin_date, "%Y%m%d"),
                           parse_date_time(tmpdsc_drn$end_date, "%Y%m%d"), fix.empty.names = FALSE)
  names(fixedDates) <- c("BEGIN_DATE","END_DATE")
  tmpdsc_drn[,"begin_date"] <- fixedDates$BEGIN_DATE
  tmpdsc_drn[,"end_date"] <- fixedDates$END_DATE
  
  print(paste("Merging temperature and discharge data into single row..."))
  tmpdsc_drn_dv <- subset(tmpdsc_drn, data_type_cd == "dv")
  tmpdsc_drn_uv <- subset(tmpdsc_drn, data_type_cd == "uv")
  
  tmpdsc_drn_dv_tmp <- tmpdsc_drn_dv[tmpdsc_drn_dv$parm_cd=="00010" & tmpdsc_drn_dv$stat_cd=="00003",]
  names(tmpdsc_drn_dv_tmp)[names(tmpdsc_drn_dv_tmp) == "begin_date"] <- "TMP_MEAN_DV_BEGIN_DATE"
  names(tmpdsc_drn_dv_tmp)[names(tmpdsc_drn_dv_tmp) == "end_date"] <- "TMP_MEAN_DV_END_DATE"
  names(tmpdsc_drn_dv_tmp)[names(tmpdsc_drn_dv_tmp) == "count_nu"] <- "TMP_MEAN_DV_COUNT_NU"
  tmpdsc_drn_dv_tmp <- subset(tmpdsc_drn_dv_tmp, select=-c(parm_cd,stat_cd))
  
  tmpdsc_drn_uv_tmp <- tmpdsc_drn_uv[tmpdsc_drn_uv$parm_cd=="00010",]
  names(tmpdsc_drn_uv_tmp)[names(tmpdsc_drn_uv_tmp) == "begin_date"] <- "TMP_MEAN_UV_BEGIN_DATE"
  names(tmpdsc_drn_uv_tmp)[names(tmpdsc_drn_uv_tmp) == "end_date"] <- "TMP_MEAN_UV_END_DATE"
  names(tmpdsc_drn_uv_tmp)[names(tmpdsc_drn_uv_tmp) == "count_nu"] <- "TMP_MEAN_UV_COUNT_NU"
  tmpdsc_drn_uv_tmp <- subset(tmpdsc_drn_uv_tmp, select=-c(parm_cd,stat_cd))
  
  tmpdsc_drn_dv_dsc <- tmpdsc_drn_dv[tmpdsc_drn_dv$parm_cd=="00060" & tmpdsc_drn_dv$stat_cd=="00003",]
  names(tmpdsc_drn_dv_dsc)[names(tmpdsc_drn_dv_dsc) == "begin_date"] <- "DSC_MEAN_DV_BEGIN_DATE"
  names(tmpdsc_drn_dv_dsc)[names(tmpdsc_drn_dv_dsc) == "end_date"] <- "DSC_MEAN_DV_END_DATE"
  names(tmpdsc_drn_dv_dsc)[names(tmpdsc_drn_dv_dsc) == "count_nu"] <- "DSC_MEAN_DV_COUNT_NU"
  tmpdsc_drn_dv_dsc <- subset(tmpdsc_drn_dv_dsc, select=-c(parm_cd,stat_cd))
  
  tmpdsc_drn_uv_dsc <- tmpdsc_drn_uv[tmpdsc_drn_uv$parm_cd=="00060",]
  names(tmpdsc_drn_uv_dsc)[names(tmpdsc_drn_uv_dsc) == "begin_date"] <- "DSC_MEAN_UV_BEGIN_DATE"
  names(tmpdsc_drn_uv_dsc)[names(tmpdsc_drn_uv_dsc) == "end_date"] <- "DSC_MEAN_UV_END_DATE"
  names(tmpdsc_drn_uv_dsc)[names(tmpdsc_drn_uv_dsc) == "count_nu"] <- "DSC_MEAN_UV_COUNT_NU"
  tmpdsc_drn_uv_dsc <- subset(tmpdsc_drn_uv_dsc, select=-c(parm_cd,stat_cd))
  
  dv_wqdrnData <- merge(tmpdsc_drn_dv_tmp, tmpdsc_drn_dv_dsc, by = c("site_no","station_nm","site_tp_cd","dec_lat_va","dec_long_va","district_cd","state_cd","county_cd","drain_area_va"), all = TRUE)
  uv_wqdrnData <- merge(tmpdsc_drn_uv_tmp, tmpdsc_drn_uv_dsc, by = c("site_no","station_nm","site_tp_cd","dec_lat_va","dec_long_va","district_cd","state_cd","county_cd","drain_area_va"), all = TRUE)
  wqdrnData_cln <- merge(dv_wqdrnData, uv_wqdrnData, by = c("site_no","station_nm","site_tp_cd","dec_lat_va","dec_long_va","district_cd","state_cd","county_cd","drain_area_va"), all = TRUE)
  wqdrnData_cln$site_no <- paste0("USGS-", wqdrnData_cln$site_no)
  colnames(wqdrnData_cln)[c(1:9)] <- c("GAGE_IDENTIFIER","GAGE_NAME","GAGE_SITETYPE","GAGE_LAT","GAGE_LON","DISTRICT_CD","STATE_CD","COUNTY_CD","DRAIN_AREA_MI2")
  wqdrnData_fin <- wqdrnData_cln[,c(1:9,24:26,41:43,58:60,75:77)]
  
  return(wqdrnData_fin)
}






splitgages_dnstrm <- damgages_dnstrm %>% group_by(DAM_COMID, .add = TRUE) %>% group_split()

system.time(wqgages_dnstrm <- gage_wq_drn(inputgages = splitgages_dnstrm,
                                          parameterCd = c("00010","00060"),
                                          service = c("dv","uv")))



