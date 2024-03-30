# adds degree day information to occurrence data by species.

#download temp data
setwd('G:/My drive/research/sdm_modeling')
dates = seq(as.Date("1970-01-01"), as.Date("2021-07-26"), by="days") # dummy days
year = format(dates, format="%Y")
month = format(dates, format="%m")
day = format(dates, format="%d")
temp = runif(length(dates),-20,50)
zerodd = rep(0,length(dates))
fiftdd = rep(0,length(dates))
tempdata = data.frame(year,month,day,dates,temp,zerodd,fiftdd) #temporary dummy data

#convert temp data to annual degree days (0C and 15C versions)
head(format(days, format="%Y"))
for(y in unique(year)){
  tempofyr = tempdata$temp[tempdata$year==y] #0C base temp
  tempofyr[tempofyr<0] = 0
  tempdata$zerodd[tempdata$year==y] = cumsum(tempofyr)
  tempofyr = tempdata$temp[tempdata$year==y] - 15 #15C base temp
  tempofyr[tempofyr<0] = 0
  tempdata$fiftdd[tempdata$year==y] = cumsum(tempofyr)
}

#pull up gbif occurrence data and add degree day column 
setwd('C:/Users/hy324/Google_Drive/research/SDM modeling/spdata')
datasets = list()
filestr = c('./umn mussel/sp_occurrence_umn_mussel.csv','./usgs_fish/sp_occurrence_usgs_fish.csv','./thermal_tol/sp_occurrence_thermal_tol.csv','./nonindigenous/sp_occurrence_nonind.csv')
writefilename = c('./umn mussel/sp_occurrence_umn_mussel_with_degreedays.csv','./usgs_fish/sp_occurrence_usgs_fish_with_degreedays.csv','./thermal_tol/sp_occurrence_thermal_tol_with_degreedays.csv','./nonindigenous/sp_occurrence_nonind_with_degreedays.csv')
for(j in 1:length(filestr)){
  occ = fread(filestr[j], sep = "\t", header = TRUE, na.strings = "\\N")
  occdates = as.Date(rep(as.Date('5000-01-01'),nrow(occ)))
  #match degree day data dates to occurrence data dates
  date_pres_idx = which(occ$day!="NA")
  datestr = as.Date(sprintf('%s-%s-%s',occ$year,occ$month,occ$day))
  occdates[date_pres_idx] = datestr[date_pres_idx]
  occ$zero_base_dd = tempdata$zerodd[match(occdates,tempdata$dates)]
  occ$fifteen_base_dd = tempdata$fiftdd[match(occdates,tempdata$dates)]
  filename = writefilename[j]
  write.table(occ, file = filename, sep = "\t",
              row.names = FALSE, col.names=TRUE)
}

  





