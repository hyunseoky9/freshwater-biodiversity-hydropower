library(readtext)
rm(list=ls())
setwd('G:/My Drive/research/sdm_modeling/sdm_results/DOE-BCC-CSM2-MR') #DOE-ACCESS-CM2 DOE-BCC-CSM2-MR
files = list.files()
files = files[which(grepl('.out',files))]
newfilenames = gsub('.out','.txt',files)
length(files)
file.rename(files,newfilenames)
for(i in 1:length(newfilenames))
{
  text = readtext(newfilenames[i])
  
  #if('Error' %in% text$text | 'error' %in% text$text | 'Call' %in% text$text)
  if (1==1)
  {
    script = str_split(text$text,'\n')
    print(script)
    readline()
  }
}

files = list.files()
files = files[which(grepl('runtime',files))]
nums = gsub('runtime','',files)
nums = as.numeric(gsub('.csv','',nums))
which(!1:522 %in% nums)
idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index
idx0[which(!1:522 %in% nums)]
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
spnamedata$name[idx0[which(!1:522 %in% nums)]]
