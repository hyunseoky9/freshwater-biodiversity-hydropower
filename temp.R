wd = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/'
idxs = c('1-8','9','10-11','12')
filenames = sprintf("%sdredging_bestmod_winteraction_%svars_aics.rds",wd,idxs)
filenames2 = sprintf("%sdredging_bestmod_winteraction_%svars.rds",wd,idxs)

aics_comb = sapply(filenames[2:length(filenames)],readRDS)
mods_comb = sapply(filenames2[2:length(filenames2)],readRDS)
length(aics_comb)
length(mods_comb)

class(mods_comb)
length(mods_comb[[3]])
mods = list()
aics = list()
for(i in 1:(length(idxs)-1))
{
  mods = c(mods,mods_comb[[i]])
  aics = c(aics,aics_comb[[i]])
}
length(mods)
order = order(aics)
aics500 = aics[order[1:500]]
mods500 = mods[order[1:500]]
bestintmod = mods500[[which(aics500==min(aics500))]]
summary(bestintmod)
sapply(mods500,function(x) length(x$coefficients))

ofilename = sprintf("%sdredging_bestmod_winteraction_aics.rds",wd)
ofilename_aic = sprintf("%sdredging_bestmod_winteraction.rds",wd)
writeRDS(aics500,ofilename_aic)
writeRDS(mods500,ofilename)

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
            curdat_sub = cbind(curdat_sub,fercd[,which(names(fercd)==intvars[j])])
            futdat_sub = cbind(futdat_sub,fercd[,which(names(fercd)==intvars[j])])
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
cdiff = exp(future_pred) - exp(current_pred)
hist(cdiff)




