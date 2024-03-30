#resource:
# Taylor et al. 2020
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md (for splitting occ data into
# test and training data)
library(devtools)
#library(rmaxent)
library(rJava) # need to have Rtools installed: https://cran.rstudio.com/bin/windows/Rtools/
library(dplyr)
library(sf)
library(sp)
library(usdm)
library(sdm)
library(raster)
library(dismo)
library(car)

#Choose descriptive name for model run.  
modelname <- "Descriptive_Model_Name"

# conducting sdm with ohio basin for freshwater darter (Aplodinotus grunniens)
# read in data
{
spdatafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens.csv'
projectionareafilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/Aplodinotus grunniens_projarea.csv'
sp = read.csv(spdatafilename)
proj = read.csv(projectionareafilename)
udd2na = which(is.na(sp$udd2))
proj$udd2[which(is.na(proj$udd2))] = 1000000
proj$BFI[which(is.na(proj$BFI))] = 0
# change mop field into integer values (categorical)
proj$mop[which(proj$mop=='(null)')] = NA
moptypes = unique(proj$mop)[which(!is.na(unique(proj$mop)))]
moptypes = data.frame(moptypes = moptypes,code = 1:length(moptypes))
proj$mop = match(proj[,'mop'],moptypes[,1])
proj$mop[which(is.na(proj$mop))] = 0
proj$mop = as.factor(proj$mop)
# presence 
p = rep(0, length(proj$comid))
p[which(!is.na(match(proj$comid,sp$comid)))] = 1 # presence
proj$p = p
}
#making occurence and background data subsets (used later in model evaluation)
presdata <- filter(proj, get('p') ==1)
bckdata <- filter(proj, get('p') ==0)


# predictor
df1 = data.frame(dplyr::select(proj,BFI,udd2,mop,waterbody,numday_above_tmax,
                              numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
                              dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
                              dd150_10c,maxflow,maxflowdate,minflow,
                              minflowdate))

df2 = data.frame(dplyr::select(proj,BFI,udd2,mop,waterbody,numday_above_tmax,
                              dd90_8c,minflow,maxflowdate,
                              minflowdate))

df = df1 # choose which predictor set to run maxent with.

#All 
#proj,BFI,udd2,mop,waterbody,numday_above_tmax,
#numday_below_tmin,dd90_5c,dd90_8c,dd90_10c,
#dd120_5c,dd120_8c,dd120_10c,dd150_5c,dd150_8c,
#dd150_10c,maxflow,maxflowdate,minflow,
#minflowdate

#only 1 degree day predictor and no numday below_tmin
#proj,BFI,udd2,mop,waterbody,numday_above_tmax, dd90_8c,
#maxflow,maxflowdate,
#minflowdate



# test collinearity
{
var=  cor(subset(df,select=-c(mop)))
var2 = cor(subset(df2,select=-c(mop)))

#VIF
response = rep(1,nrow(df)) # dummy response
model = lm(response~BFI+udd2+mop+waterbody+numday_above_tmax+ dd90_8c+
             maxflow+maxflowdate+
             minflowdate,data=df)
vif(model)



# test multicollinearity
det(var2) # determination of correlation matrix
kappa(df)^(1/2)
kappa(df2)^(1/2)

#corrplot(var_inv,method='number',is.corr = F)              # visualize the multicollinearity
}

#** running maxent


#Paste Maxent.jar (v 3.4.1.) into the java folder of the dismo package at this location & code checks to make sure it will run
system.file("java", package="dismo")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
#if (file.exists(jar) & require(rJava))
  

#Creating output folder path

setwd('G:/My Drive/research/sdm_modeling/sdm_results')
savepath <- dir.create(eval(modelname))

# split occurrence data into testing and training data
# get the same random sample for training and testing
set.seed(1)

# randomly select 75% for training
selected <- sample(1:length(which(p==1)), length(which(p==1)) * 0.75)

occ_train <- selected  # this is the selection to be used for model training
occ_test <- 1:length(which(p==1))    # this is the opposite of the selection which will be used for model testing
occ_test = occ_test[-selected]




#Creating argument list for Maxent in dismo, including RM values from above
RMargs <- c(paste0("allowpartialdata=true"))
RMargs <- c(RMargs, paste0("writeplotdata=TRUE"))
RMargs <- c(RMargs, paste0("removeDuplicates=true"))
RMargs <- c(RMargs, paste0("responsecurves=true"))
RMargs <- c(RMargs, paste0("Jackknife=true"))
RMargs <- c(RMargs, paste0("writebackgroundpredictions=true"))
#other useful args: randomtestpoints(int),randomseed(false),
file.exists(jar)
require(rJava)

#nbgval = nrow(df)

# 1. running maxent by separating test/train outside of the maxent function.
# res: https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
testidx = which(p==1)[occ_test]
p_train = p[-testidx]
df_train = df[-testidx,]
df_bg = df_train[which(p_train==0),]
options(java.parameters = "-Xmx4g" ) # memory allocation.
Descriptive_Model_Name <-maxent(df_train, p_train, args=RMargs,
                                  path="G:/My Drive/research/sdm_modeling/spdata/Descriptive_Model_Name_nonbg", 
                                  silent=FALSE)


  # 1.1 first evaluate with training data to see if you get same auc
mod_eval_train <- dismo::evaluate(p = df_train[which(p_train==1),], a = df_bg, model = Descriptive_Model_Name) 
print(mod_eval_train)
  # 1.2evaluate the model with test data
mod_eval_train_test <- dismo::evaluate(p = df[testidx,], a = df_bg, model = Descriptive_Model_Name) 
print(mod_eval_train_test)

# 2.running maxent by separating test/train inside of the maxent function. 

RMargs2 = RMargs

RMargs2 = c(RMargs2,paste0(sprintf("randomtestpoints=%d",ceiling(length(which(p==1)) * 0.25))))
RMargs2 = c(RMargs2,paste0("randomseed=TRUE"))
Descriptive_Model_Name <-maxent(df, p, args=RMargs2,
                                path="G:/My Drive/research/sdm_modeling/spdata/sdm_testpartition", 
                                silent=FALSE)
  
#Variable contributions and response curves
plot(Descriptive_Model_Name)
response(Descriptive_Model_Name)

#######
##############################
####Projecting model back onto input data to save predictions to segments
###https://rdrr.io/github/johnbaums/rmaxent/f/README.md
#####################################

library(devtools)
#install_github('johnbaums/rmaxent')
library(rmaxent)
predictpres <- rmaxent::project(Descriptive_Model_Name, presdata)
predictpresdf <-as.data.frame(predictpres)

predictbck <- rmaxent::project(Descriptive_Model_Name, bckdata)
predictbckdf <- as.data.frame(predictbck)

predall <- project(Descriptive_Model_Name, proj)
predalldf <- as.data.frame(predall)
max(predalldf$prediction_cloglog)
min(predalldf$prediction_cloglog)

# suitability data based on minimum training presence (MTP)
mtp = min(predalldf$prediction_cloglog[which(proj$p==1)])
hist(predalldf$prediction_cloglog[which(proj$p==0)],breaks=10)
mean(predalldf$prediction_cloglog[which(proj$p==1)])
mean(predalldf$prediction_cloglog[which(proj$p==0)])
mean(predalldf$prediction_cloglog)
hist(predalldf$prediction_cloglog)
s_mtp = rep(0,nrow(predalldf))
s_mtp[which(predalldf$prediction_cloglog>mtp)] = 1

# Maxsss 
# res: Liu et al. 2013; Frans 2018
res_df <- read.csv("G:/My Drive/research/sdm_modeling/spdata/sdm_testpartition/maxentResults.csv", header=TRUE)
max3strain = res_df$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold # this is maxsss. decide whether to do training or test.
max3stest = res_df$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold
s_max3s = rep(0,nrow(predalldf))
s_max3s[which(predalldf$prediction_cloglog>mtp)] = 1


# write out put of the binary prediction and cloglog probability.
o = data.frame(comid=proj$comid,probability=predalldf$prediction_cloglog,s_max3s,s_mtp)
head(o)
ofilename = 'G:/My Drive/research/sdm_modeling/spdata/per_sp_ohio/sdm_output/ohio_grunniens_sdmoutput.csv'
write.csv(o,ofilename,row.names=FALSE)




















# trying to do the sdm through sdm package. Needs rasterization, which is very cumbersome, so most
# likely won't be using them. (garbage)
dis <- dist(df[,c(1,2)])
names(df) = c('x','y','comid')
diag <- min(dis)
res <- sqrt((diag^2)/2)
pts <- df
coordinates(pts) <- ~x+y
r <- raster(ext=extent(pts),res=res)
st <- stack()
start_time <- Sys.time()
for(z in names(pts))
{
  rasOut<-rasterize(pts, r, z)
  st <- stack(st,rasOut) 
}
end_time <- Sys.time()
end_time - start_time

table(is.na(rasOut))
plot(st[[1]])

r_obj = raster(xmn=min(df$x)-0.0002, xmx=max(df$x)+0.0002,
               ymn=min(df$y)-0.0002, ymx=max(df$y)+0.0002)

r_data <- rasterize(x=df[, 1:2], # lon-lat data
                    y=r_obj, # raster object
                    field=df[, 3], # vals to fill raster with
                    fun=mean) # aggregate function
plot(r_data)
table(is.na(r_data[]))
freq(r1, value=NA)


sp$presence = 1
sp = sp[,c('decimalLongitude','decimalLatitude','presence')]
coordinates(sp) <- ~decimalLongitude + decimalLatitude


bio = raster::geoootData('worldclim',var='bio',res=10)

needed_col = c('decimalLongitude','decimalLatitude','udd2','waterbody','BFI','mop',
               'numday_above_tmax',	'numday_below_tmin',	'dd90_5c', 'dd90_8c',	'dd90_10c',
               'dd120_5oc',	'dd120_8c',	'dd120_10c',	'dd150_5c',	'dd150_8c',
               'dd150_10c',	'maxflow',	'maxflowdate',	'minflow',	'minflowdate')
pred = proj[,needed_col]
# make it into a sp object
coordinates(pred) = ~decimalLongitude + decimalLatitude
# rasterize the sp object
rasterize(pred)


# check colinearity structure
# res: https://www.youtube.com/watch?v=wLGanYOLzV8

# run the sdm
install.packages(c('dismo','gbm','tree','mda','class','mgcv'))
d <- sdmData(species~.,sp, predictors= predictorD, bg=list(n=(length(sp)*2)))
getmethodNames()
m <- sdm(species~., d, methods=c('maxent'))
