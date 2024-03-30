# make a data table which species are federally ESA listed in which huc8s.
# filename=listed_byhuc8_studyspecies.csv
spfilename= 'G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv'
listedfilename = 'G:/My Drive/research/sdm_modeling/spdata/Listed Animals.csv'
partialLfilename = 'G:/My Drive/research/sdm_modeling/spdata/partially_listed_sp.csv'
spdata = read.csv(spfilename)
listeddata = read.csv(listedfilename)
partialL = read.csv(partialLfilename)

partial_listed_idx = which(!is.na(match(spdata$name,partialL$Species.name)))
listed_idx = which(!is.na(match(spdata$name,listeddata$Scientific.Name.Species)))
#spdata$name[listed_idx]
#spdata$listed[listed_idx] = 1
#spdata$listed[partial_listed_idx] = 'partial'

#write.csv(spdata,spfilename,row.names=FALSE)
listed_idx
partial_listed_idx %in% listed_idx


# make the data table

filename = 'G:/My Drive/research/sdm_modeling/spdata/non-indigenous_byhuc8_studyspecies.csv'
d = read.csv(filename)
d[1:nrow(d),2:ncol(d)] = 0
head(d)
dim(d)

# listed everywhere
all_listed_idx = listed_idx[which(! listed_idx %in% partial_listed_idx)]
d[,all_listed_idx+1] = 1
for(i in 1:length(partial_listed_idx))
{
  idx = partial_listed_idx[i]
  huc8s = partialL$ESA.listed.HUC8s[which(partialL$Species.name==spdata$name[idx])]
  huc8s = as.numeric(trimws(unlist(strsplit(huc8s,';'))))
  if(length(huc8s)==0)
  {
    next
  }
  matches = c()
  print(spdata$name[idx])
  for(j in 1:length(huc8s))
  {
    hucdigit = ceiling(log10(huc8s[j])) + (ceiling(log10(huc8s[j]))%%2)
    scaled = huc8s[j]
    scaled_ref = d$HUC8
    if(hucdigit>8)
    {
      scaled = floor(scaled/10^(hucdigit-8))
    } else if (hucdigit <8)
    {
      scaled_ref = floor(scaled_ref/10^(8-hucdigit))
    }
    if(scaled %in% scaled_ref)
    {
      matchidx = which(scaled_ref==scaled)
      if(length(matchidx)>0)
      {
        d[matchidx,idx+1] = 1
        matches= c(matches,matchidx)
      } else {
        break
      }
    }
  }
  
  print(length(huc8s))
  print(sum(d[,idx+1]))
}

ofilename  = 'G:/My Drive/research/sdm_modeling/spdata/listed_byhuc8_studyspecies.csv'
write.csv(d,ofilename,row.names=FALSE)
