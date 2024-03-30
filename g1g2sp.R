#identify which of the study species are G1/G2 species. 
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')
names = spnamedata$name

nsdata = read.csv("G:/My Drive/research/sdm_modeling/spdata/natureserve/freshwater_fish_mussel.csv")
nsdata$Scientific.Name
spinNS = names[which(names %in% nsdata$Scientific.Name)] # species in my dataset in NS dataset. 

Granks = nsdata$NatureServe.Rounded.Global.Rank[which(nsdata$Scientific.Name %in% spinNS)]
table(Granks)
# there are 9 G1 and 16 G2 species in my data.

idx=  which(Granks %in% c("G1","G2"))
d = nsdata[which(nsdata$Scientific.Name %in% spinNS)[idx],]
g1g2sp = d$Scientific.Name # G1 G2 species in my data.
# check the number of data for each and check whether their data is obscured in gbif. 
nrows = c()
for(i in 1:length(g1g2sp))
{
  sp = g1g2sp[i]
  spfilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',sp)
  data = read.csv(spfilename)  
  nrows[i] = nrow(data)
}


# which threatened species are G1/G2?
th = spnamedata$name[ which(spnamedata$threatened==1)]
thspinNS = names[which(th %in% nsdata$Scientific.Name)] # species in my dataset in NS dataset. 

thGranks = nsdata$NatureServe.Rounded.Global.Rank[which(nsdata$Scientific.Name %in% thspinNS)]
table(thGranks)
# surprisingly, none.
