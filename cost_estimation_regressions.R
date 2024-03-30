# make some regression models.
library(ape)
library(psych)
library(car)
library(sets)
library(MASS)
library(MuMIn)
library(tidyverse)


rm(list=ls())
{
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename2)
fercd = fercd[which(fercd$including.in.the.study==1),]
freshwc = fercd$c.freshWsp.cons.related # first potential dependent variable for the regression. 
logfreshwc = log(freshwc+1) # log transform response var
logpowercap = log(fercd$power.capacity.MW. + 1) # log transform power capacity
freshwc[which(is.na(logfreshwc))]
}
# make a new data frame with relevant variables from fercd
{
vars = cbind(fercd$projectID,logfreshwc,logpowercap,fercd[,c("nativemusselspnum_cr",
    "nonnativemusselspnum_cr",
    "gamespnum_cr", "diadromous2.spnum_cr", 
    "cp2.flow.related", "cp2.WqualityNtemp.related", "cp2.passage.related",
    "cp2.game.related", "cp2.listed.related", "cp2.invmussel.related",
    "cp2.natmussel.related","cp2.invfish.related","cp2.migratingsp2.related")])
# make dummy variables for measure-based variables.
vars$cp2dummy.flow.related = 0
vars$cp2dummy.WqualityNtemp.related = 0
vars$cp2dummy.passage.related = 0
vars$cp2dummy.game.related = 0
vars$cp2dummy.migratingsp2.related = 0
vars$cp2dummy.listed.related = 0
vars$cp2dummy.listed.related = 0
vars$cp2dummy.invmussel.related = 0
vars$cp2dummy.natmussel.related = 0
vars$cp2dummy.invfish.related = 0
vars$cp2dummy.flow.related[which(vars$cp2.flow.related>0)] = 1
vars$cp2dummy.WqualityNtemp.related[which(vars$cp2.WqualityNtemp.related>0)] = 1
vars$cp2dummy.passage.related[which(vars$cp2.passage.related>0)] = 1
vars$cp2dummy.game.related[which(vars$cp2.game.related>0)] = 1
vars$cp2dummy.migratingsp2.related[which(vars$cp2.migratingsp2.related>0)] = 1
vars$cp2dummy.listed.related[which(vars$cp2.listed.related>0)] = 1
vars$cp2dummy.invmussel.related[which(vars$cp2.invmussel.related>0)] = 1
vars$cp2dummy.natmussel.related[which(vars$cp2.natmussel.related>0)] = 1
vars$cp2dummy.invfish.related[which(vars$cp2.invfish.related>0)] = 1
vars$cp2dummy.flow.related = as.factor(vars$cp2dummy.flow.related)
vars$cp2dummy.WqualityNtemp.related = as.factor(vars$cp2dummy.WqualityNtemp.related)
vars$cp2dummy.passage.related = as.factor(vars$cp2dummy.passage.related)
vars$cp2dummy.game.related = as.factor(vars$cp2dummy.game.related)
vars$cp2dummy.migratingsp2.related = as.factor(vars$cp2dummy.migratingsp2.related)
vars$cp2dummy.listed.related = as.factor(vars$cp2dummy.listed.related)
vars$cp2dummy.invmussel.related = as.factor(vars$cp2dummy.invmussel.related)
vars$cp2dummy.natmussel.related = as.factor(vars$cp2dummy.natmussel.related)
vars$cp2dummy.invfish.related = as.factor(vars$cp2dummy.invfish.related)

vars$run.of.river = as.factor(fercd$run.of.river)
vars$Project_Region_dummy = as.factor(fercd$Project_Region_dummy)

# get rid of rows with any na's.
nrow(vars)
nrow(na.omit(vars))
narowidx = which(apply(vars,1,function(x) any(is.na(x))))
vars = na.omit(vars)
names(vars)[which(names(vars)=='fercd$projectID')] = 'projectID'
}

# model 1. regress freshwater sp. cons. cost against all the explanatory variables. 
fullmod = lm(vars[,2:(ncol(vars)-1)])

summary(fullmod)

print()
(names(vars[,2:(ncol(vars)-1)]))
  

# dredging code: keep 500 best performing models from dredging.
# only keep models where species count-based predictors 
# have a positive coefficient (theoretical base).
modelnum_lim = 500 #ceiling(0.00005*2^22-1)
mods = list()
mods_i = 1
aics = c()
maxaics = 10000000
sp.related.varidx = which(names(vars) %in% c('listedspnum_cr',"gamespnum_cr","diadromous.spnum_cr","nativemusselspnum_cr","nonnativemusselspnum_cr"))
start.time <- Sys.time()
for(i in 1:(ncol(vars)-3))
{
  combinations = as.matrix(t(combn(3:(ncol(vars)-1),i)))
  for(j in 1:nrow(combinations))
  {
    
    if(any(combinations[j,] %in% sp.related.varidx))
    {
      mod = lm(vars[,c(2,(combinations[j,]))])
      aic = AICc(mod)
      s = summary(mod)
      fishvar = which(rownames(s$coefficients) %in% c('listedspnum_cr',"gamespnum_cr","diadromous.spnum_cr"))
      mussvar = which(rownames(s$coefficients) %in% c("nativemusselspnum_cr","nonnativemusselspnum_cr"))
      if(any(s$coefficients[fishvar,4] <= 0.05) & any(s$coefficients[mussvar,4] <= 0.05))
      {
        if(length(aics)==modelnum_lim)
        {
          maxaics = max(aics)
          if(aic<maxaics)
          {
            excludeidx = which(aics==maxaics)
            mods = mods[-excludeidx]
            aics = aics[-excludeidx]
            
            mods[[modelnum_lim]] = mod
            aics = c(aics,aic)
          }
        } else {
          mods[[mods_i]] = mod
          aics[mods_i] = aic
          mods_i = mods_i + 1
          maxaics = max(aics)
        }
      }
    }
    if(j %% 1000==0)
    {
      print(sprintf('i=%d, %d/%d',i,j,nrow(combinations)))
    }
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

mods_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging.rds")
aics_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_associated_aics.rds")
#saveRDS(mods, file = mods_filename)
#saveRDS(aics, file = aics_filename)
aics = readRDS(aics_filename)
mods = readRDS(mods_filename)
length(mods)
length(aics)
sapply(mods,AICc)==aics
model_order = order(aics)

for( i in model_order)
{
  print(summary(mods[[i]]))
  print(i)
  print(sprintf('aic=%.3f',aics[i]))
  readline()
}

dredgemod = mods[[which(aics==min(aics))]]

summary(dredgemod)
Anova(dredgemod)
stepaic = stepAIC(fullmod)
summary(stepaic)

# make dredging model table
modname <- function(mod)
{
  names = names(mod$coefficients)
  names = gsub('`','',names)
  return(sprintf('logfreshwc ~ %s',paste(names[2:length(names)],collapse=' + ')))
}
modelname = sapply(mods,modname)
adjusted.r.squared = sapply(mods,function(x) summary(x)$adj.r.squared)
r.squared = sapply(mods,function(x) summary(x)$r.squared)
aictable = data.frame(modelname=modelname,aicc = aics,adjusted.r.squared=adjusted.r.squared,r.squared=r.squared)
ofilename_aictable = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_top500.csv'
write.csv(aictable,ofilename_aictable,row.names=FALSE)


# test 4 assumptions of the dredgemod
# homogeneity
plot(dredgemod)
# model passes normality (great) and homogeneity (not great but not bad) and leverage test (barely)

# look at interaction terms
dredgemod_vars = names(dredgemod$coefficients)
dredgemod_vars = dredgemod_vars[2:length(dredgemod_vars)]
dredgemod_vars = gsub('1','',dredgemod_vars)

combinations = as.matrix(t(combn(1:length(dredgemod_vars),2)))

sigint = c()
for( i in 1:nrow(combinations))
{
  intv1name = dredgemod_vars[combinations[i,1]]
  intv2name = dredgemod_vars[combinations[i,2]]
  
  intv1 = vars[,which(names(vars)==intv1name)]
  intv2 = vars[,which(names(vars)==intv2name)]
  idx = 1:length(dredgemod_vars)
  idx = idx[-combinations[i,]]
  rest1 = vars[,which(names(vars)==dredgemod_vars[idx[1]])]
  rest2 = vars[,which(names(vars)==dredgemod_vars[idx[2]])]
  rest3 = vars[,which(names(vars)==dredgemod_vars[idx[3]])]
  rest4 = vars[,which(names(vars)==dredgemod_vars[idx[4]])]
  rest5 = vars[,which(names(vars)==dredgemod_vars[idx[5]])]
  rest6 = vars[,which(names(vars)==dredgemod_vars[idx[6]])]
  rest7 = vars[,which(names(vars)==dredgemod_vars[idx[7]])]
  rest8 = vars[,which(names(vars)==dredgemod_vars[idx[8]])]
  rest9 = vars[,which(names(vars)==dredgemod_vars[idx[9]])]
  intmod = lm(vars$logfreshwc~intv1+intv2+rest1+rest2+rest3+rest4+rest5+rest6+rest7+rest8+rest9+intv1:intv2)
  summarycoeffs = summary(intmod)$coefficients
  if(summarycoeffs[nrow(summarycoeffs),ncol(summarycoeffs)]<0.05 & !is.na(intmod$coefficients[13]))
  {
    sigint = rbind(sigint,c(intv1name,intv2name))   
    #print(summary(intmod))
    #print(c(intv1name,intv2name))
    #readline()
  }
}
sigint
unique(c(sigint[,1],sigint[,2]))
# dredge again with the variables in the best model
# try all the interaction variables as well if the variables in a model include all the 
# components of an interaction variable.
dredge_var_names = names(dredgemod$coefficients)
dredge_var_names = dredge_var_names[2:length(dredge_var_names)]
dredge_var_names = gsub('1','',dredge_var_names)
dredge_vars = vars[c("logfreshwc",dredge_var_names)]
dredge_vars_numeric = dredge_vars
factoridx <- which(sapply(dredge_vars, function(x) class(x) == 'factor'))
for(i in factoridx)
{
  dredge_vars_numeric[,i] = as.numeric(dredge_vars[,i])-1  
}

intcomponents = unique(c(sigint[,1],sigint[,2]))
intcomponents = match(intcomponents,names(dredge_vars))


modelnum_lim = 500 #ceiling(0.00005*2^22-1)
mods = list()
mods_i = 1
aics = c()
maxaics = 10000000
sp.related.varidx2 = which(names(dredge_vars) %in% c('listedspnum_cr',"gamespnum_cr","diadromous.spnum_cr","nativemusselspnum_cr","nonnativemusselspnum_cr"))
for(i in 1:8)
{
  combinations = as.matrix(t(combn(2:ncol(dredge_vars),i)))
  for(j in 1:nrow(combinations))
  {
    
    if(any(combinations[j,] %in% c(3,4,5)))
    {
      vars_final_pre = dredge_vars[,c(1,(combinations[j,]))]
      intvars = list()
      intvars_i = 0
      
      if(sum(combinations[j,] %in% intcomponents)>=2) # get all the possible interaction pair combinations
      {
        intidx = combinations[j,which(combinations[j,] %in% intcomponents)]
        if(length(intidx)==2)
        {
          intcombos = list(t(as.matrix(intidx,nrow=1)))
        } else
        {
          intcombos = apply(as.matrix(2:length(intidx)),1,function(x) t(combn(intidx,x)))
        }
        for(jj in 1:length(intcombos))
        {
          rownames(intcombos[[jj]]) = apply(intcombos[[jj]],1,function(x) paste(names(dredge_vars)[x],collapse=':'))
        }
        if(length(intcombos)>2)
        {
          intcombos = intcombos[1:2]
        }
        # make list of variables of interaction terms that should go into regression
        for(ii in 1)#:length(intcombos))
        {
          intcombos_sub = intcombos[[ii]]
          maxnumintvar = (floor(nrow(dredge_vars)/10)-i) # maximum number of interaction variable possible
          if(nrow(intcombos_sub)==1)
          {
            intvarcombos = list(as.matrix(1))
          } else if(nrow(intcombos_sub)==2)
          {
            intvarcombos = list(t(as.matrix(1:2)),as.matrix(1:2))
          } else
          {
            if(nrow(intcombos_sub)>maxnumintvar)
            {
              intvarcombos = apply(as.matrix(1:maxnumintvar),1, function(x) combn(1:nrow(intcombos_sub),x))              
            } else {
              intvarcombos = apply(as.matrix(1:nrow(intcombos_sub)),1, function(x) combn(1:nrow(intcombos_sub),x))              
            }
          }
          if(length(intvarcombos)<maxnumintvar)
          {
            jjl = length(intvarcombos)            
          } else {
            jjl = maxnumintvar
          }
          for(jj in 1:jjl)
          {
            for(kk in 1:ncol(intvarcombos[[jj]]))
            {
              intvaridx = intcombos_sub[intvarcombos[[jj]][,kk],]
              if(is.matrix(intvaridx)==FALSE)
              {
                intvar = as.matrix(apply(dredge_vars_numeric[,intvaridx],1,prod))                
                colnames(intvar) = rownames(intcombos_sub)[kk]
                intvar_binds = intvar
                if(ii==2)
                {
                  intvar_1storder = apply(combn(intvaridx,2),2,function(x) apply(dredge_vars_numeric[,x],1,prod))
                  colnames(intvar_1storder) = c(1:ncol(intvar_1storder))
                  for(I in 1:ncol(intvar_1storder))
                  {colnames(intvar_1storder)[I] = paste(names(dredge_vars_numeric)[combn(intvaridx,2)[,I]],collapse=":")}
                  intvar_binds = cbind(intvar_binds,intvar_1storder)
                }
              } else {
                intvar_binds = c()
                for(cc in 1:nrow(intvaridx))
                {
                  intvar = as.matrix(apply(dredge_vars_numeric[,intvaridx[cc,]],1,prod))
                  colnames(intvar) = rownames(intvaridx)[cc]
                  intvar_binds = cbind(intvar_binds,intvar)
                  if(ii==2)
                  {
                    intvar_1storder = apply(combn(intvaridx[cc,],2),2,function(x) apply(dredge_vars_numeric[,x],1,prod))
                    colnames(intvar_1storder) = c(1:ncol(intvar_1storder))
                    for(I in 1:ncol(intvar_1storder))
                    {colnames(intvar_1storder)[I] = paste(names(dredge_vars)[combn(intvaridx[cc,],2)[,I]],collapse=":")}
                    intvar_binds = cbind(intvar_binds,intvar_1storder)
                  }
                }
              }
              if(sum(duplicated(colnames(intvar_binds)))>0)
              {
                intvar_binds = intvar_binds[,-which(duplicated(colnames(intvar_binds)))]                
              }
              intvars_i = intvars_i + 1
              intvars[[intvars_i]] = intvar_binds
            }
            print(sprintf('i=%d, %d/%d, ii=%d/%d,jj=%d/%d',i,j,nrow(combinations),ii,length(intcombos),jj,jjl))
          }
        }
      }
      
      print(length(intvars))
      for( k in 1:length(intvars))
      {
        if(length(intvars)==0)
        {
          if(k==1)
          {
            next
          } else {
            vars_final = vars_final_pre
          }
        } else {
          vars_final = cbind(vars_final_pre,intvars[[k]])
        }

        mod = lm(vars_final)
        aic = AICc(mod)
        s = summary(mod)
        fishvar = which(rownames(s$coefficients) %in% c('listedspnum_cr',"gamespnum_cr","diadromous.spnum_cr"))
        mussvar = which(rownames(s$coefficients) %in% c("nativemusselspnum_cr","nonnativemusselspnum_cr"))
        if(any(s$coefficients[fishvar,4] <= 0.05) & any(s$coefficients[mussvar,4] <= 0.05))
        {
          if(length(aics)==modelnum_lim)
          {
            maxaics = max(aics)
            if(aic<maxaics)
            {
              excludeidx = which(aics==maxaics)
              if(length(excludeidx)>1)
              {
                excludeidx = excludeidx[1]
              }
              mods = mods[-excludeidx]
              aics = aics[-excludeidx]
              mods[[modelnum_lim]] = mod
              aics = c(aics,aic)
              }
          } else if(length(aics)>modelnum_lim)
          {
            print('something wrong')
            readline()
          } else 
          {
            mods[[mods_i]] = mod
            aics[mods_i] = aic
            mods_i = mods_i + 1
            maxaics = max(aics)
          }
        }          
      }
    }

    if(j %% 1000==0)
    {
      print(sprintf('i=%d, %d/%d',i,j,nrow(combinations)))
    }
  }
}


mods_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_bestmod_winteraction_vars.rds")
aics_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_bestmod_winteraction_vars_aics.rds")
#saveRDS(mods, file = mods_filename)
#saveRDS(aics, file = aics_filename)
aics = readRDS(aics_filename)
mods = readRDS(mods_filename)
model_order = order(aics)
#aics[which(is.na(aics))] = 1000000
for( i in model_order)
{
  
  #print(summary(mods[[i]]))
  
  vif = try({vif(mods[[i]])},silent=TRUE)
  
  if(class(vif)!='try-error')
  {
    if(all(vif<10))
    {
      print(i)
      print(sprintf
            ('aic=%.3f dredgemod aic= %.3f',aics[i],AICc(dredgemod)))
      par(mfrow=c(1,2))
      qqnorm(mods[[1]]$residuals, pch = 1, frame = FALSE)
      qqline(mods[[1]]$residuals, col = "steelblue", lwd = 2)
      qqnorm(mods[[i]]$residuals, pch = 1, frame = FALSE)
      qqline(mods[[i]]$residuals, col = "steelblue", lwd = 2)
      readline()
    }
  }
}
# 176th best model has better normality than the best model and has VIF values no greater than 10.
# Thus, we select 176th model over the 1st one. The AICc value is still smaller than the best no-interaction model.
#bestintmod = mods[[which(aics==min(aics))]]
bestintmod = mods[[176]]
vif(bestintmod)
plot(bestintmod)
#qqplot
qqnorm(residuals(bestintmod), main = "Q-Q Plot")
qqline(residuals(bestintmod))
# standardized residuals against the fitted plot
std_residuals <- rstandard(bestintmod)
sqrt_std_residuals <- sqrt(abs(std_residuals))
plot(fitted(bestintmod), sqrt_std_residuals, 
     xlab = "Fitted Values", ylab = "Square Root of Standardized Residuals", 
     main = "Square Root of Standardized Residuals vs. Fitted Values")
loess_fit <- loess(sqrt_std_residuals ~ fitted(bestintmod))
lines(fitted(bestintmod)[order(fitted(bestintmod))], predict(loess_fit)[order(fitted(bestintmod))], col = "blue")




bestintmod_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/bestintmod.rds")
saveRDS(bestintmod,file=bestintmod_filename)
# make dredging model table
modname <- function(mod)
{
  names = names(mod$coefficients)
  names = gsub('`','',names)
  return(sprintf('logfreshwc ~ %s',paste(names[2:length(names)],collapse=' + ')))
}
modelname = sapply(mods,modname)
adjusted.r.squared = sapply(mods,function(x) summary(x)$adj.r.squared)
r.squared = sapply(mods,function(x) summary(x)$r.squared)
aictable = data.frame(modelname=modelname,aicc = aics,adjusted.r.squared=adjusted.r.squared,r.squared=r.squared)
ofilename_aictable = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_with_interaction_on_best_model_top500.csv'
write.csv(aictable,ofilename_aictable,row.names=FALSE)



# get Moran's I on the residuals of the best model with interactions.

# make spatial matrix
latlong = fercd[match(vars$projectID,fercd$projectID),c('huc8centroid_lat','huc8centroid_long')]
dim(latlong)
residualcoord = tibble(residual=bestintmod$residuals,Lat=latlong$huc8centroid_lat,Lon=latlong$huc8centroid_long)

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

# make current and future predictions
fmod = bestintmod  # final model of selection


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
      futvariable = gsub('_cr','_fr',intvars)
      futdat1 = fercd[,which(names(fercd)==futvariable[1])]
      futdat2 = fercd[,which(names(fercd)==futvariable[2])]
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
          futvariable = gsub('_cr','_fr',intvars[j])
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
    futvariable = gsub('_cr','_fr',variable)
    futdat = fercd[,which(names(fercd)==futvariable)]
    futvars[,which(names(futvars)==variable)] = futdat[-narowidx]
  }
}
names(futvars) = gsub('`','',names(futvars))
names(curvars) = gsub('`','',names(curvars))
current_pred = predict(fmod,newdata=curvars)
future_pred = predict(fmod,newdata=futvars)

predictions = data.frame(projid = vars$projectID, current= exp(vars$logfreshwc)+1,
                         current_pred=exp(current_pred)+1,future_pred=exp(future_pred)+1)
ofilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/linear_regression_model_predictions.csv'
#write.csv(predictions,ofilename,row.names = FALSE)
fercd[,c('Project_Region_dummy','Project_Region')]

# mixed effect (spatial variation.) using regions.
library(lme4)
library(lmerTest)
mixmod1 = lmer(logfreshwc~
                 logpowercap+
                 nativemusselspnum_cr+
                 nonnativemusselspnum_cr+
                 diadromous.spnum_cr+
                 cp2.WqualityNtemp.related+
                 cp2.listed.related+
                 cp2.natmussel.related+
                 cp2dummy.flow.related+
                 cp2dummy.WqualityNtemp.related+
                 cp2dummy.passage.related+
                 cp2dummy.game.related +
                 (1|Project_Region_dummy),
               data=vars)





# try to see how much increase in gamespnum_cr increases cost

current = predict(dredgemod,data=vars)
summary(dredgemod$fitted.values-current1up)
newdata = data.frame(logpowercap=vars$logpowercap,
                     gamespnum_cr=vars$gamespnum_cr,
                     cp2.listed.related=vars$cp2.listed.related,
                     cp2dummy.flow.related=vars$cp2dummy.flow.related,
                     cp2dummy.passage.related=vars$cp2dummy.passage.related,
                     cp2dummy.migratingsp.related=vars$cp2dummy.migratingsp.related,
                     cp2dummy.listed.related=vars$cp2dummy.listed.related)

newdata$gamespnum_cr = newdata$gamespnum_cr + 1
newdata2 = newdata
newdata2$gamespnum_cr = newdata2$gamespnum_cr + 1
current1up = stats::predict(dredgemod,newdata=newdata)
current2up = stats::predict(dredgemod,newdata=newdata2)
mc = exp(current1up)-exp(dredgemod$fitted.values)
mc = sort(mc)
names(mc) = c()
sort(mc)
hist(mc[which(mc<10^3)],breaks=10)

plot(current)
current1up - current
plot(current1up - current)

points(current1up,col='red')






# 2. regression for each cost category
# this model works in two parts.
# a) regress cost of each category against sdm based and cost based measures.
# b) regress cost of all freshwater sp. cons. related measures against cost of each category.
# all costs should be log transformed for normality
rm(list=ls())
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename2)
fercd = fercd[which(fercd$including.in.the.study==1),]

# b) first.
freshwc = fercd$c.freshWsp.cons.related # first potential dependent variable for the regression. 
logfreshwc = log(freshwc+1) # log transform response var

cvars = cbind(freshwc,fercd[,c("c.flow.related" ,"c.passage.related" ,"c.listed.related" ,"c.game.related" ,"c.mussel.related" ,"c.invmussel.related" ,"c.invfish.related","c.WqualityNtemp.related","c.hatchery.related","c.fishing.related")])
head(cvars)
{
logcvars = as.data.frame(apply(cvars,2,function(x) log(1+x)))
names(logcvars)
logcvars$cdummy.flow.related = 0
logcvars$cdummy.passage.related = 0
logcvars$cdummy.listed.related = 0
logcvars$cdummy.game.related = 0
logcvars$cdummy.mussel.related = 0
logcvars$cdummy.invmussel.related = 0
logcvars$cdummy.invfish.related = 0
logcvars$cdummy.WqualityNtemp.related = 0
logcvars$cdummy.hatchery.related = 0
logcvars$cdummy.fishing.related = 0
logcvars$cdummy.flow.related[which(logcvars$c.flow.related>0)] = 1
logcvars$cdummy.passage.related[which(logcvars$c.passage.related>0)] = 1
logcvars$cdummy.listed.related[which(logcvars$c.listed.related>0)] = 1
logcvars$cdummy.game.related[which(logcvars$c.game.related>0)] = 1
logcvars$cdummy.mussel.related[which(logcvars$c.mussel.related>0)] = 1
logcvars$cdummy.invmussel.related[which(logcvars$c.invmussel.related>0)] = 1
logcvars$cdummy.invfish.related[which(logcvars$c.invfish.related>0)] = 1
logcvars$cdummy.WqualityNtemp.related[which(logcvars$c.WqualityNtemp.related>0)] = 1
logcvars$cdummy.hatchery.related[which(logcvars$c.hatchery.related>0)] = 1
logcvars$cdummy.fishing.related[which(logcvars$c.invfish.related>0)] = 1
logcvars$cdummy.flow.related
logcvars$Project_Region_dummy = fercd$Project_Region_dummy
logcvars$cdummy.flow.related = as.factor(logcvars$cdummy.flow.related)
logcvars$cdummy.passage.related = as.factor(logcvars$cdummy.passage.related)
logcvars$cdummy.listed.related = as.factor(logcvars$cdummy.listed.related)
logcvars$cdummy.game.related = as.factor(logcvars$cdummy.game.related)
logcvars$cdummy.mussel.related = as.factor(logcvars$cdummy.mussel.related)
logcvars$cdummy.invmussel.related = as.factor(logcvars$cdummy.invmussel.related)
logcvars$cdummy.invfish.related = as.factor(logcvars$cdummy.invfish.related)
logcvars$cdummy.WqualityNtemp.related = as.factor(logcvars$cdummy.WqualityNtemp.related)
logcvars$cdummy.hatchery.related = as.factor(logcvars$cdummy.hatchery.related)
logcvars = na.omit(logcvars)
}

 






# dredging code: keep 500 best performing models from dredging.
# only keep models where species count-based predictors 
# have a positive coefficient (theoretical base).
modelnum_lim = 500 #ceiling(0.00005*2^22-1)
mods = list()
mods_i = 1
aics = c()
maxaics = 10000000
for(i in 1:(ncol(logcvars)-2))
{
  combinations = as.matrix(t(combn(2:(ncol(logcvars)-1),i)))
  for(j in 1:nrow(combinations))
  {
    mod = lm(logcvars[,c(1,(combinations[j,]))])
    aic = AICc(mod)
    s = summary(mod)
    if(length(aics)==modelnum_lim)
    {
      maxaics = max(aics)
      if(aic<maxaics)
      {
        excludeidx = which(aics==maxaics)
        mods = mods[-excludeidx]
        aics = aics[-excludeidx]
        
        mods[[modelnum_lim]] = mod
        aics = c(aics,aic)
      }
    } else {
      mods[[mods_i]] = mod
      aics[mods_i] = aic
      mods_i = mods_i + 1
      maxaics = max(aics)
    }
    if(j %% 1000==0)
    {
      print(sprintf('i=%d, %d/%d',i,j,nrow(combinations)))
    }
  }
}

mods_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_model2.rds")
aics_filename = sprintf("G:/My Drive/research/sdm_modeling/environmental mitigation data/dredging_model2_associated_aics.rds")
#saveRDS(mods, file = mods_filename)
#saveRDS(aics, file = aics_filename)
aics = readRDS(aics_filename)
mods = readRDS(mods_filename)
length(mods)
length(aics)
sapply(mods,AIC)==aics
model_order = order(aics)

#for( i in model_order)
#{
#  print(summary(mods[[i]]))
#  print(i)
#  readline()
#}
dredgemod = mods[[which(aics==min(aics))]]
summary(dredgemod)


plot(dredgemod)

# interaction 


# spatial random effect
library(lme4)
library(lmerTest)
mixmod = lmer(freshwc~c.flow.related+c.passage.related+c.mussel.related+
                c.invfish.related+c.WqualityNtemp.related+c.hatchery.related+
                cdummy.flow.related+cdummy.passage.related+cdummy.listed.related+
                (1|Project_Region_dummy),data=logcvars)
summary(mixmod)
step(mixmod)
stepmixmod = lmer(freshwc~c.flow.related+c.passage.related+c.mussel.related+
                c.invfish.related+c.WqualityNtemp.related+c.hatchery.related+
                (1|Project_Region_dummy),data=logcvars)
summary(stepmixmod)
class(mixmod)
plot(stepmixmod)
which(residuals(stepmixmod)< -6)
length(residuals(stepmixmod))

plot(dredgemod)
plot(residuals(dredgemod,type='response'))

logcvars[126,]
hist(logcvars$freshwc)
which(names(dredgemod$residuals)==145)
which(rownames(fercd)==145)
which(rownames(cvars)==145)
cvars[127,]
fercd$projectID[126]

