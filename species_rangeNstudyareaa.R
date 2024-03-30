# make a map for each species depicting their study area (boundary for predicting occurrence)
# and the predicted occurrence in baseline current reservoir scenario.
library(ggthemes)
library(ggplot2)
theme_set(theme_bw())
library(pbapply)
library(data.table)
library(sf)
library(raster)
library(sp)

rm(list=ls())

wd = 'G:/My Drive/research/sdm_modeling/spdata/sp_studyareaNoccurrencearea_maps'

# Step 1: Call the pdf command to start the plot

# Step 2: Create the plot with R code
# read in 8digit huc shapefile
huc8conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc8_clipped/huc8_clipped.shp')
huc8conus$HUC8 = as.numeric(huc8conus$HUC8)
# read in 2digit huc shapefile
huc2conus = st_read('G:/My Drive/research/sdm_modeling/gis/wbd/huc8/huc2.shp')
huc2conus$HUC2 
# load in spcount data for 3 different scenario given the gcm
{
  currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
  gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
  scenario = 'pristine_gcm_reservoir'
  threshold = 0.4 # netsymdiff threshold
  if(scenario=='pristine w gcm')
  {
    scenariostr = 'pri'  
  } else
  {
    scenariostr = 'res'
  }
  spcountfilename1 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
  spcount_currentres = read.csv(spcountfilename1)
  
  currentofuture = 'future' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
  gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
  scenario = 'pristine_gcm_reservoir'
  threshold = 0.4 # netsymdiff threshold
  if(scenario=='pristine w gcm')
  {
    scenariostr = 'pri'  
  } else
  {
    scenariostr = 'res'
  }
  spcountfilename2 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
  spcount_futureres = read.csv(spcountfilename2)
  
  currentofuture = 'current' # current tempflow predictors or future tempflow predictors? 1=curernt, 0=future
  gcmver = 'GCM_average' #'DOE-ACCESS-CM2'
  scenario = 'pristine w gcm'
  threshold = 0.4 # netsymdiff threshold
  if(scenario=='pristine w gcm')
  {
    scenariostr = 'pri'  
  } else
  {
    scenariostr = 'res'
  }
  spcountfilename3 = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/spcount/%s/df2_total_spcount_nsd%.1f_%s%s.csv',gcmver,threshold,currentofuture,scenariostr)
  spcount_currentpri = read.csv(spcountfilename3)
}

subject_scenario = spcount_currentres
for( i in 2:ncol(subject_scenario))
{
  
  # get species distribution predicted by SDM (current reservori scenario)
  spname = names(subject_scenario)[i]
  spnamefile = gsub('\\.',' ',spname)
  ofilename = sprintf('%s/%s-map.png',wd,spnamefile)
  png(file = ofilename,   # The directory you want to save the file in
      width = 1061, # The width of the plot in inches
      height = 614) # The height of the plot in inches
  
  spcount_lite = as.data.table(cbind(subject_scenario$HUC8,subject_scenario[,i]))  

    
  # get species study range 
  studyrange_wd = 'G:/My Drive/research/sdm_modeling/spdata/projection_area/by_sp/only_non-tempflow related predictors'
  rangefilename = sprintf('%s/%s_projarea_non-tempflowpredictors.csv',studyrange_wd,gsub('\\.',' ',spname))
  projarea = fread(rangefilename)
  studyarea_huc8 = unique(floor(projarea$huc12/10^4))
  studyarea_01 = rep(0,length(subject_scenario$HUC8))
  studyarea_01[which(!is.na(match(subject_scenario$HUC8,studyarea_huc8)))] = 1
  spcount_lite = cbind(spcount_lite,studyarea_01)
    
  
  # status. 0= neither occurring or study area. 1= only occurring area. 2= 
  names(spcount_lite) = c('HUC8','occurring','studyarea')
  status = rep('neither',nrow(spcount_lite))
  status[which(spcount_lite$occurring==1 & spcount_lite$studyarea==0)] = 'only occurrence area'
  status[which(spcount_lite$occurring==0 & spcount_lite$studyarea==1)] = 'only study area'
  status[which(spcount_lite$occurring==1 & spcount_lite$studyarea==1)] = 'both'
  spcount_lite$Status = status
  
  
  d = merge(huc8conus,spcount_lite,by='HUC8',all.x=TRUE,sort=FALSE)
  map = ggplot(data = d) +
    geom_sf(aes(fill = Status), alpha = 0.5) +
    scale_fill_manual(values = c("only study area" = "red", "only occurrence area" = "blue", "both" = "purple", 'neither' = 'grey')) +
    geom_sf(data = huc2conus, color = "red", fill = NA, size = 0.6) +  # This adds the HUC2 boundaries
    labs(title = spname, fill = "Status") + 
    theme_map()
  
  print(map)
  dev.off()
  print(sprintf('%d/%d done',i,ncol(subject_scenario)))
}
library(rmarkdown)
library(knitr)

# Set the directory where your images are stored
img_dir <- "G:/My Drive/research/sdm_modeling/spdata/sp_studyareaNoccurrencearea_maps"
setwd(img_dir)

# List the PNG image files
img_files <- list.files(img_dir, pattern = "*-map.png", full.names = TRUE)

# Species names (assuming this is correct from your existing code)
spnames <- gsub('\\.',' ', names(subject_scenario))
spnames = spnames[2:length(spnames)]

# Create markdown text with full paths to images
md_text <- paste0(
  lapply(1:length(img_files), function(i) {
    paste0("![](", img_files[i], ")\n\n", "Figure ", i, ": Map for ", spnames[i], "\n\n")
  }),
  collapse = ""
)

# Write markdown to a temporary file
temp_md_file <- tempfile(fileext = ".md")
writeLines(md_text, temp_md_file)

# Render the markdown file to PDF
output_pdf <- gsub(".md", ".pdf", temp_md_file)
render(temp_md_file, output_format = "pdf_document", output_file = output_pdf)
