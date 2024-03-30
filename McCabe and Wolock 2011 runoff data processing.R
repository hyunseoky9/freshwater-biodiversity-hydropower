# processing runoff data from McCabe and Wolock 2011
# runoff data 2000-2009 & 2010-2015: https://www.sciencebase.gov/catalog/item/59c2b980e4b091459a61d425
# id to raster conversion data: https://www.sciencebase.gov/catalog/item/59c55d6be4b017cf313d590e
library(raster)
setwd('G:/My Drive/research/sdm_modeling/dam data/runoff data')
raster = raster('prismid.asc')
runoff1 = read.csv('run2000s.csv')
runoff2 = read.csv('run2010s.csv')

names(runoff1)
#average by the years and then average them together
byyr=  c()
yrs = substr(names(runoff1)[2:(ncol(runoff1)-1)],start=5,stop=8)
for( i in 2000:2009)
{
  hwat = as.matrix(runoff1[,(which(as.numeric(yrs)==i)+1)])
  avgbyyr = apply(hwat,1,mean)
  byyr = cbind(byyr,avgbyyr)
}
yrs2 = substr(names(runoff2)[2:(ncol(runoff2)-1)],start=5,stop=8)
for( i in 2010:2015)
{
  hwat = as.matrix(runoff2[,(which(as.numeric(yrs2)==i)+1)])
  avgbyyr = apply(hwat,1,mean)
  byyr = cbind(byyr,avgbyyr)
}
mar = apply(byyr,1,mean)
length(mar)
mardf = data.frame(prismid =runoff1$prismid ,mean_annual_runoff=mar)
head(mardf)
write.csv(mardf, 'McCabe and Wolock 2011 mean annual runoff.csv')

# make a raster with mean annual runoff
# Get the cell coordinates of the raster
coords <- rasterToPoints(raster)

# Extract X and Y coordinates from the cell coordinates
x_coords <- coords[, 1]
y_coords <- coords[, 2]
prismid <- coords[,3]
coords_df <- data.frame(prismid = prismid, X = x_coords, Y = y_coords)
coords_df2 = merge(coords_df,mardf)
head(coords_df2)


# make raster with the mar values
raster2 = rasterFromXYZ(coords_df2[,2:ncol(coords_df2)])
crs(raster2) = crs(raster)
writeRaster(raster2, "McCabe and Wolock 2011 mean annual runoff raster.tif", format = "GTiff",overwrite=TRUE)


