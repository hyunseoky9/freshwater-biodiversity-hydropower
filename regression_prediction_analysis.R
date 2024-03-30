library(ggplot2)
library(sf)
rm(list=ls())
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename2)
fercd = fercd[which(fercd$including.in.the.study==1),]
filename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/linear_regression_model_predictions.csv'
pred = read.csv(filename)
varsfilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/regression_variables(na_omitted).csv'
vars = read.csv(varsfilename)

pchange = as.data.frame(vars$cdiff_pred/vars$current_pred*100)
names(pchange) = 'percent_change'
par(mar = c(6, 7, 4.1, 2.1))
ggplot(data=pchange, aes(x=percent_change))+geom_histogram()+
  theme_minimal() +
  theme(
    axis.line = element_blank() # Removes axis lines
  )

mean(pchange$percent_change)
hist(pchange$percent_change,xlab='percent change',main='',cex.lab=2,cex.axis=2,n=22)

#vars$current_pred = pred$current_pred
#vars$future_pred = pred$future_pred
#vars$cdiff = pred$future_pred - pred$current # current observed - future predicted
#vars$cdiff_pred = pred$future_pred - pred$current_pred # current predicted -future predicted
#write.csv(vars,'G:/My Drive/research/sdm_modeling/environmental mitigation data/regression_variables(na_omitted).csv',row.names=FALSE)

# plot cost difference (future - current) by region
cdiff_region = sapply(1:4,function(x) vars[which(vars$Project_Region_dummy==x),'cdiff_pred'])
sapply(cdiff_region,median)
par(mfrow=c(2,2))
regionname = c('MW','NE','S','W')
for(i in 1:4)
{
  title = sprintf('histogram of average difference in cost of 
                  mitigation in %s region',regionname[i])
  hist(as.numeric(cdiff_region[[i]]),main=title,xlab='mitigation cost (2022$)')  
}

# plot current cost by region
currc_region = sapply(1:4,function(x) vars[which(vars$Project_Region_dummy==x),'current'])
sapply(currc_region,median)
par(mfrow=c(2,2))
regionname = c('MW','NE','S','W')

for(i in 1:4)
{
  title = sprintf('average current cost of 
                  mitigation in %s region',regionname[i])
  x = as.numeric(currc_region[[i]])
  #x = x[which(x<10^7)]
  #hist(x,main=title,xlab='mitigation cost (2022$)')  
  
  plot(sort(x),ylim=c(0,10^8),main=title)
}
sapply(currc_region,mean)
mean_by_category6 <- tapply(vars$current, vars$Project_Region_dummy, mean, na.rm = TRUE)

plot(vars$logfreshwc, vars$cdiff_pred)
length(which(cdiff==0))

# is larger cost related to larger cost difference?
cbind(vars$current_pred[order(vars$current_pred)],vars$future_pred[order(vars$current_pred)],vars$cdiff_pred[order(vars$current_pred)])
plot(vars$current_pred,vars$cdiff_pred,ylab='cost difference',xlab='current cost')
value = vars$cdiff_pred #cdiff_pred
value = abs(vars$cdiff_pred)
value = log(value  + 1)
value[which(vars$cdiff_pred<0)] = value[which(vars$cdiff_pred<0)]*-1
plot(log(vars$current_pred+1),abs(value),xlab='log cost',ylab='log cost diff')
# answer: projects with larger cost currently will likely experience larger 
# magnitude of difference in cost in the future
apply(cbind(vars$nativemusselspnum_cr,vars$diadromous2.spnum_cr,vars$gamespnum_cr),1, function(x) all(x==0))
cbind(vars$current[order(vars$current)],vars$current_pred[order(vars$current_pred)],
      (vars$current[order(vars$current)]-vars$current_pred[order(vars$current_pred)])/vars$current[order(vars$current)])
# how different is current pred from current observed?
hist((log(vars$current[order(vars$current)]+1))-(log(vars$current_pred[order(vars$current_pred)]+1)),breaks=20)
hist((vars$current[order(vars$current)]-vars$current_pred[order(vars$current_pred)])/vars$current[order(vars$current)],breaks=20)
# * histogram in the difference between current pred and current observed is not normally distributed 
# because the regression was done on the log transformed cost.
# how different is the cost difference across projects with different power capacity (size of the project)?
plot(exp(vars$logpowercap)-1,vars$cdiff_pred,xlab='cost difference',ylab='log power capacity')
plot(abs(value),vars$logpowercap,xlab='cost difference',ylab='log power capacity')
cor(abs(value)[which(value!=0)],vars$logpowercap[which(value!=0)])
summary(lm(log(vars$cdiff_pred[which(vars$cdiff_pred!=0)]+1)~vars$logpowercap[which(vars$cdiff_pred!=0)]))

   
# map out cost difference, change in the number of anadromous, native mussel, and game species
# in the us map
filename = 'G:/My Drive/research/sdm_modeling/gis/tl_2017_us_state.shp'
us = st_read(filename)
idx = match(vars$projectID,fercd$projectID)
currspnum = cbind(fercd$nativemusselspnum_cr[idx],fercd$diadromous2.spnum_cr[idx],fercd$gamespnum_cr[idx])
futspnum = cbind(fercd$nativemusselspnum_fr[idx],fercd$diadromous2.spnum_fr[idx],fercd$gamespnum_fr[idx])
diffspnum = as.data.frame(futspnum - currspnum)
names(diffspnum) = c('nativemussel','diadromous','game')
coords = as.data.frame(cbind(fercd$huc8centroid_long[idx],fercd$huc8centroid_lat[idx]))
names(coords) = c('longitude','latitude')
coords = st_as_sf(coords,coords=c('longitude','latitude'),crs=4326)
coords = st_transform(coords,crs=st_crs(us))

value = vars$cdiff_pred #cdiff_pred
value = abs(vars$cdiff_pred)
value = log10(value  + 1)
value[which(vars$cdiff_pred<0)] = value[which(vars$cdiff_pred<0)]*-1
scaled_values = value
scaled_values[which(scaled_values<0)] = scaled_values[which(scaled_values<0)]/abs(min(scaled_values[which(scaled_values<0)]))
scaled_values[which(scaled_values>0)] = scaled_values[which(scaled_values>0)]/max(scaled_values[which(scaled_values>0)])
get_color <- function(val) {
  if (val < 0) {
    # Scale between light red and red for negative values
    return(colorRampPalette(c("#ffbaba", "#a70000"))(100)[as.integer(-val * 99) + 1])
  } else {
    # Scale between light blue and blue for positive values
    return(colorRampPalette(c("#b3cde0", "#011f4b"))(100)[as.integer(val * 99) + 1])
  }
}
point_colors <- sapply(scaled_values, get_color)
point_colors[which(value==0)] = '#a1a4a2'
# Create the gradient bar
gradient_data <- data.frame(x = 1, y = seq(0, 1, length.out = 200))
ggplot(gradient_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = y)) +
  scale_fill_gradient(low = "#ffbaba", high = "#a70000") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), # Removes grid lines
    panel.border = element_blank(), # Removes panel border
    axis.line = element_blank() # Removes axis lines
  )

# point size should be proportional to log power capacity (size of the project)
value2 = vars$logpowercap
value2_scaled = value2/max(value2)
maxcex = 7
enlarger = 2
point_size = value2_scaled*maxcex +enlarger
breaks_value = c(1,3,5,7,9)
breaks_size = breaks_value/max(value2)*maxcex+enlarger
for(i in 1:length(breaks_size))
{
  if(i==1)
  {
    point_size[which(point_size<=breaks_size[1])] = breaks_size[1]    
  } else {
    point_size[which(point_size<=breaks_size[i] & point_size>breaks_size[i-1])] = breaks_size[i]
  }
}

sizeorder = order(point_size,decreasing=TRUE)
hist(vars$cdiff_pred)
#breaks_value = c(0.07,1,2,3,4,5,6,7)
#breaks_size = breaks_value/max(value2)*maxcex+2
#legend_lab = as.character(floor(exp(breaks_value)))
legend_lab = c('<1','1 - 3','3 - 5','5 - 7','7<')
labname = expression('log'[10]*'(Power Capacity in MW)')
sizeorder = order(point_size,decreasing=TRUE)

# Figure D
ggplot() +
  geom_sf(data=us,fill='white') + 
  geom_sf(data = coords[sizeorder,], aes(size = point_size[sizeorder]), color = point_colors[sizeorder]) +
  scale_size_continuous(name = labname,
                        breaks = breaks_size,
                        labels = legend_lab,
                        range = c(4, 10)) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.y=element_blank(), 
    axis.text.x=element_blank(),
  )
min(value)
max(value)
length(which(value==0))/length(value)
length(which(value>0))/length(value)
length(which(value<0))/length(value)







plot(us$geometry)
plot(coords[sizeorder,], col = point_colors[sizeorder], pch = 20, cex = point_size[sizeorder],add=TRUE)
scale_size_continuous(name = "Power Capacity", 
                      range = c(max(value2), min(value2)), 
                      breaks = c(2, 4, 6), 
                      labels = c("label1", "label2", "label3"))
#legend("topright", legend = c("Large Negative (Red)", "Near Zero (Light Colors)", "Large Positive (Blue)"), 
#       col = c("red", "grey", "blue"), pch = 20)


# which spnum variable drive the cost difference the most?
cdifforder = order(diffspnum$game) #order(vars$cdiff_pred)
cbind(diffspnum[cdifforder,],vars$cdiff_pred[cdifforder])

# how does influence of each category of species in the model affect the prjoect's future cost change?
# get change in cost for change in each species category's count, see if they add upto the cost change 
# from changing all species category's count.
future_pred = list()
for(i in 1:4) # 1=allcategoreis,2=gameonly,3=nativemusselonly,4=diadromousonly
{
  if(i==1)
  {
    subjcat='all'
  } else if(i==2)
  {
    subjcat='game'
  } else if(i==3){
    subjcat='nativemussel'
  } else if(i==4){
    subjcat='diadromous'
  }
  
  bestintmod_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/bestintmod.rds")
  bestintmod = readRDS(bestintmod_filename)
  fmod=bestintmod
  narowidx = which(is.na(match(fercd$projectID,vars$projectID)))
  variables_used <- names(fmod$coefficients)
  variables_used = variables_used[2:length(variables_used)]
  variables_used = gsub('1','',variables_used)
  spnumvariablesNinteractions_used = variables_used[which(grepl('_cr',variables_used)|grepl(':',variables_used))]
  curvars = vars
  futvars = vars
  for(variable in spnumvariablesNinteractions_used)
  {
    if(grepl(':',variable))
    {
      # interaction variable
      intvars = unlist(strsplit(variable,':'))
      intvars = gsub('`','',intvars)
      spnum_stat = grepl('_cr',intvars)
      if(all(spnum_stat))
      {
        # both interaction variables spnum vars
        if(grepl(subjcat,intvars[1]) | subjcat=='all')
        {
          futvariable1 = gsub('_cr','_fr',intvars[1])
        } else {
          futvariable1 = intvars[1]
        }
        if(grepl(subjcat,intvars[2]) | subjcat=='all')
        {
          futvariable2 = gsub('_cr','_fr',intvars[2])
        } else {
          futvariable2 = intvars[2]
        }
        futdat1 = fercd[,which(names(fercd)==futvariable1)]
        futdat2 = fercd[,which(names(fercd)==futvariable2)]
        futdat = futdat1*futdat2
        curdat1 = fercd[,which(names(fercd)==intvars[1])]
        curdat2 = fercd[,which(names(fercd)==intvars[2])]
        curdat = curdat1*curdat2
      } else{
        # only one or no interaction variables are spnum var
        curdat_sub = c()
        futdat_sub = c()
        for(j in 1:2)
        {
          if(spnum_stat[j]==TRUE)
          {
            if(grepl(subjcat,intvars[j]) | subjcat=='all')
            {
              futvariable = gsub('_cr','_fr',intvars[j])
            } else {
              futvariable = intvars[j]
            }
            
            futdat_sub = cbind(futdat_sub,fercd[,which(names(fercd)==futvariable)])
            curdat_sub = cbind(curdat_sub,fercd[,which(names(fercd)==intvars[j])])
          } else {
            if(grepl('dummy',intvars[j]))
            {
              temp = fercd[,which(names(fercd)==gsub('dummy','',intvars[j]))]
              temp[which(temp>0)] = 1
              curdat_sub = cbind(curdat_sub,temp)
              futdat_sub = cbind(futdat_sub,temp)
            } else {
              if(grepl('power',intvars[j]))
              {
                curdat_sub = cbind(curdat_sub,log(fercd$power.capacity.MW.+1))
                futdat_sub = cbind(futdat_sub,log(fercd$power.capacity.MW.+1))
              } else {
                curdat_sub = cbind(curdat_sub,fercd[,which(names(fercd)==intvars[j])])
                futdat_sub = cbind(futdat_sub,fercd[,which(names(fercd)==intvars[j])])
              }
            }
          }
        }
        curdat = curdat_sub[,1]*curdat_sub[,2]
        futdat = futdat_sub[,1]*futdat_sub[,2]
      }
      futdat = futdat[-narowidx]
      curdat = curdat[-narowidx]
      futvars = cbind(futvars,futdat)
      curvars = cbind(curvars,curdat)
      names(futvars)[length(names(futvars))] = variable
      names(curvars)[length(names(curvars))] = variable
    } else {
      # non-interaction variable
      if(grepl(subjcat,variable) | subjcat=='all')
      {
        futvariable = gsub('_cr','_fr',variable)
      } else {
        futvariable = variable  
      }
      futdat = fercd[,which(names(fercd)==futvariable)]
      futvars[,which(names(futvars)==variable)] = futdat[-narowidx]
    }
  }
  names(futvars) = gsub('`','',names(futvars))
  names(curvars) = gsub('`','',names(curvars))
  futvars$cp2dummy.flow.related = as.factor(futvars$cp2dummy.flow.related)
  futvars$cp2dummy.WqualityNtemp.related = as.factor(futvars$cp2dummy.WqualityNtemp.related)
  futvars$cp2dummy.passage.related = as.factor(futvars$cp2dummy.passage.related)
  futvars$cp2dummy.migratingsp2.related = as.factor(futvars$cp2dummy.migratingsp2.related)
  future_pred[[i]] = predict(fmod,newdata=futvars)
}
future_pred = sapply(future_pred,function(x) exp(x)+1)

cdiff_cat = data.frame(all=future_pred[,1]-vars$current_pred,
                             gameonly=future_pred[,2]-vars$current_pred,
                       natmussonly=future_pred[,3]-vars$current_pred,
                       diadonly=future_pred[,4]-vars$current_pred)
cdiff_cat$catsum = cdiff_cat$gameonly+cdiff_cat$natmussonly+cdiff_cat$diadonly
head(cdiff_cat)
for(i in 1:ncol(cdiff_cat)) # differences that are lower than 20 are actually 0's
{
  cdiff_cat[which(cdiff_cat[,i]<20 & cdiff_cat[,i]>-20),i] = 0
}
head(cdiff_cat,10)

# rank which species number variable has the largest influence (magnitude)
cdiff_cat_scaled=cdiff_cat[,2:4]
rank = c()
for( i in 1:nrow(cdiff_cat_scaled))
{
  a = cdiff_cat_scaled[i,] 
  cdiff_cat_scaled[i,] = abs(a)/max(abs(a))
  
  winner = which(abs(a)==max(abs(a)) & max(abs(a))!=0)
  if(length(winner)>0)
  {
    rank[i] = winner
  } else {
    rank[i] = NA
  }
}
for( x in 1:ncol(cdiff_cat_scaled))
{
  rank[which(rank==x)] = names(cdiff_cat_scaled)[x]  
}



table(rank)/(length(rank)-length(which(is.na(rank))))

apply(diffspnum,2,sd)

cdiff_cat[,1]
cdiff_cat$gameonly
vars$current_pred
future_pred[[1]]

head(cdiff_cat)
cdiff_contribution = cdiff_cat[,c('gameonly','natmussonly','diadonly')]
head(cdiff_contribution)
apply(cdiff_contribution,2,function(x) x/abs(cdiff_cat$all))


# looking at the cost difference by regions (featured in the main manuscript)
mean(vars$cdiff_pred)
idx = match(vars$projectID,fercd$projectID)
vars$Project_State = fercd$Project_State[idx]
# west 
varsub = vars[which(vars$Project_State %in% c("WA","OR","ID","MT","WY","CA","UT","CO")),]
# northwest with ca 
varsub = vars[which(vars$Project_State %in% c("WA","OR","ID","MT","WY","CA")),]
mean(varsub$cdiff_pred) # change in mitigation cost
mean(varsub$cdiff_pred/varsub$current_pred*100) # % percent change
nw_idx = which(vars$Project_State %in% c("WA","OR","ID","MT","WY","CA"))
# northwest without CA
varsub = vars[which(vars$Project_State %in% c("WA","OR","ID","MT","WY")),]
mean(varsub$cdiff_pred) # change in mitigation cost
mean(varsub$cdiff_pred/varsub$current_pred*100) # % percent change

# west without northwest
varsub = vars[which(vars$Project_State %in% c("CA","UT","CO")),]
mean(varsub$cdiff_pred) # change in mitigation cost
mean(varsub$cdiff_pred/varsub$current_pred*100) # % percent change

# southeast 
se_idx = which(vars$Project_State %in% c('AL','FL','GA','KY','MD','MS','NC','SC','TN',
                                'VA','WV','LA','AR','AL/GA','SC/NC','TX/LA'))
varsub = vars[which(vars$Project_State %in% c('AL','FL','GA','KY','MD','MS','NC','SC','TN',
                                              'VA','WV','LA','AR','AL/GA','SC/NC','TX/LA')),]
mean(varsub$cdiff_pred) # change in mitigation cost
mean(varsub$cdiff_pred/varsub$current_pred*100) # % percent change

# Northeast 
varsub = vars[which(vars$Project_State %in% c('ME','NH','VT','MA','RI','CT','NY','NJ','PA','MD/PA')),]
mean(varsub$cdiff_pred) # change in mitigation cost
mean(varsub$cdiff_pred/varsub$current_pred*100) # % percent change

# Midwest
varsub = vars[which(vars$Project_State %in% c('IL',
  'IN','IA','KS','MI','MN','MO','NE','ND','SD','OH','WI')),]
mean(varsub$cdiff_pred) # change in mitigation cost
mean(varsub$cdiff_pred/varsub$current_pred*100) # % percent change



mean(varsub$cdiff_pred)
length(which(varsub$cdiff_pred==0))/nrow(varsub)
length(which(varsub$cdiff_pred>0))/nrow(varsub)
length(which(varsub$cdiff_pred<0))/nrow(varsub)

length(which(varsub$cdiff_pred==0))
length(which(varsub$cdiff_pred>0))
length(which(varsub$cdiff_pred<0))
nrow(varsub)

table(diffspnum[nw_idx,]$game)
table(diffspnum[se_idx,]$game)

cbind(vars$cdiff_pred[nw_idx],diffspnum[nw_idx,]$game)
cbind(vars$cdiff_pred[se_idx],diffspnum[se_idx,]$game)
vars$cdiff_pred[se_idx]
dim(vars)

# checking spatial autocorrelation on cost difference.
{
latlong = fercd[match(vars$projectID,fercd$projectID),c('huc8centroid_lat','huc8centroid_long')]
dim(latlong)
residualcoord = tibble(residual=vars$cdiff_pred,Lat=latlong$huc8centroid_lat,Lon=latlong$huc8centroid_long)

resid.dists <- as.matrix(dist(cbind(residualcoord$Lon, residualcoord$Lat)))
dim(resid.dists)
resid.dists.inv = 1/resid.dists
diag(resid.dists.inv) <- 0
infinityidx = which(is.infinite(resid.dists.inv))  # Check for infinite values
resid.dists.inv[infinityidx] = 10^(-10)

resid.dists.invsq = 1/resid.dists^2
diag(resid.dists.invsq) <- 0
infinityidx = which(is.infinite(resid.dists.invsq))  # Check for infinite values
resid.dists.invsq[infinityidx] = 10^(-10)

Moran.I(residualcoord$residual, resid.dists.inv)
Moran.I(residualcoord$residual, resid.dists.invsq)
}

table(diffspnum$game)
table(diffspnum$diadromous)
table(diffspnum$nativemussel)

summary(lm(vars$cdiff_pred~diffspnum$nativemussel+diffspnum$diadromous+diffspnum$game))
