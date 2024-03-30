# number of occurrence points per each species.
setwd('G:/My Drive/research/sdm_modeling/spdata/per_sp')
a = read.csv("Acantharchus pomotis_wcomid.csv")
read.csv("Cichlasoma dimerus.csv",sep="\t")
numtabdelimited = 0
spnamedata = read.csv('../comprehensive_sp_info.csv')
spname_list = spnamedata$species
numdatpoints = c()
for(i in 1:length(spname_list))
{
  spname = spname_list[i]
  filename= sprintf('%s.csv',spname)
  dat = read.csv(filename)
  if(ncol(dat)==1)
  {
    dat = read.csv(filename, sep="\t")
  }
  numdatpoints[i] = nrow(dat)
  #print(spname)
  #print(ncol(dat))
  #if(ncol(dat)==1)
  #{
  #  numtabdelimited = numtabdelimited + 1
  #  print(spname)
  #  print(head(dat))
  #}
  #else if (ncol(dat)==9)
  #{
  #  print(spname)
  #}
}

write.csv(numdatpoints,'numoccpts.csv')
print(numtabdelimited)



filename = 'Cichlasoma dimerus.csv'
read.csv(filename)
filename = 'Cichlasoma salvini.csv'
read.csv(filename)
filename = 'Cichlasoma bimaculatum.csv'
read.csv(filename)
  