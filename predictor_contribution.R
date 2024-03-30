# predictor contribution across the 3 GCMs
library(data.table)

filename = 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
spdata = read.csv(filename)

idxfilename = 'G:/My Drive/research/sdm_modeling/sdm_results/sdm_included_species.csv'
idx0 = read.csv(idxfilename)
idx0 = idx0$index

gcmvers = c('DOE-CNRM-ESM2-1','DOE-BCC-CSM2-MR','DOE-ACCESS-CM2')
meancont = c()
secont = c()
for(gcmver in gcmvers)
{
  varlen =c()
  spcontlist = list()
  j=1
  for( i in idx0)
  {
    spname =  spdata$name[i]
    filename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s/%s_pristinerun_dfdd90_8c_lownetsymdiff0.40/predictor_contribution.csv',gcmver,spname)
    spcontlist[[j]] = fread(filename)
    varlen = c(varlen,length(spcontlist[[j]]))
    j=j+1
  }
  
  fullvarnames = names(spcontlist[[1]])
  for(i in 1:length(spcontlist))
  {
    if(length(spcontlist[[i]])==9)
    {
      idx1 = which(!fullvarnames %in% names(spcontlist[[i]]))
    }
  }
  vars = list()
  getval <- function(list,var)
  {
    idx=which(names(list)==var)
    if(length(idx)==0)
    {
      return(NA)
    } else {
      list[[idx]]
    }
  }
  vars = list()
  j=1
  for(var in fullvarnames)
  {
    vars[[j]] = sapply(spcontlist, getval,var=var)
    j=j+1
  }
  
  meancont = cbind(meancont,sapply(vars,function(x) mean(na.omit(x))))
  secont = cbind(secont,sapply(vars,function(x) sd(na.omit(x))/sqrt(length(na.omit(x)))))
  names(meancont)=fullvarnames
  #length(vars)
  #par(mfrow=c(2,5))
  #for(i in 1:length(vars))
  #{
  #  hist(vars[[i]],main=fullvarnames[i])
  #}
  print(gcmver)
}

meancont = as.data.frame(meancont)
secont = as.data.frame(secont)
names(meancont) = gcmvers
names(secont) = gcmvers

meancont = cbind(fullvarnames,meancont)
secont = cbind(fullvarnames,secont)

o = as.data.frame(apply(as.matrix(2:ncol(meancont)),
                    1,function(x) paste(sprintf('%.2f',meancont[,x]),sprintf('(%.2f)',secont[,x]))))
o = cbind(fullvarnames,o)
names(o)[1] = 'predictor_name'
ofilename = 'G:/My Drive/research/sdm_modeling/sdm_results/analysis/predictor_contribution_across_species.csv'
write.csv(o,ofilename,row.names=FALSE)
