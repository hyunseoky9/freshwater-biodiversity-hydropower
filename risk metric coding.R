# working with ea and eis data. Getting average mitigation and monitoring cost value from the data for the risk metric calculation.
rm(list=ls())
filename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/measure data.csv'
d = read.csv(filename)
minyr = min(d$dollar.yr[which(!is.na(d$dollar.yr))])
# data from 
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/inflation data.csv'
infld = read.csv(filename2) 
infld = infld[which(infld$year>=minyr,),]
infld$dollar = as.numeric(gsub('\\$','',infld$dollar))


# convert all the costs into 2022 dollar
for(i in 1:nrow(d))
{
  if( !is.na(d$annualized.cost[i]) )
  {
    multiplier = infld$dollar[length(infld$dollar)]/infld$dollar[which(infld$year==d$dollar.yr[i])]
    d$annualized.cost[i] = d$annualized.cost[i]*multiplier
    if(!is.na(d$capital.cost[i]))
    {
      d$capital.cost[i] = d$capital.cost[i]*multiplier
    }
    if(!is.na(d$o.m.cost[i]))
    {
      d$o.m.cost[i] = d$o.m.cost[i]*multiplier
    }
    if(!is.na(d$annual.cost[i]))
    {
      d$annual.cost[i] = d$annual.cost[i]*multiplier
    }
    if(!is.na(d$annual.energy.costs[i]))
    {
      d$annual.energy.costs[i] = d$annual.energy.costs[i]*multiplier
    }
  }
}
#1. get average cost of mitigation for listed fish.
#1.1 find which species are listed.
idx = which(!is.na(d$specific.sp) & is.na(d$mussel.related))
splist = unique(d$sp.id[idx])
#break down multiple species strings into single individuals (there are none anymore... 7/13/2023). 
multiplesp = splist[which(grepl(';',splist))]
if(length(multiplesp)>0)
{
  addedsp = c()
  for(sp in multiplesp)
  {
    addedsp = c(addedsp,strsplit(sp,';'))
  }
  addedsp=unlist(addedsp)
  splist = splist[-which(grepl(';',splist))]
}
splist = unique(c(splist,addedsp))
# change rainbow to steel. they are the same
d$sp.id[which(d$sp.id=='rainbow')] = 'steel'
# file made from 1.1 and internet search.
filename3 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/listed sp in ea eis.csv'
listed.data = read.csv(filename3)
listedsp = listed.data$code[which(listed.data$listed==1)]

# 1.2 get average mitigation cost per species per ferc docs(P-XX) and per species per dam (table in a ferc doc)
dd = d[which(!is.na(d$specific.sp)& is.na(d$mussel.related)),]
head(dd)
#by ferc docs
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  species = unique(ddd$sp.id)
  cost_persp = 0
  cost_perspcount = 0
  for(j in 1:length(species))
  {
    if(species[j] %in% listedsp)
    {
      
      cost_persp = cost_persp + sum(ddd$annualized.cost[which(ddd$sp.id==species[j])])
      cost_perspcount = cost_perspcount + 1
    }
  }
  if(cost_perspcount > 0)
  {
    avgcost_persp = cost_persp/cost_perspcount
    cost = cost + avgcost_persp
    costcount = costcount + 1
  }
}
avgcost = cost / costcount # *average mitigation & monitoring annualized cost per listed fish per ferc doc
# by projects ( by table )
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  for( k in 1:length(unique(ddd$table)))
  {
    tb = unique(ddd$table)[k]
    d4 = ddd[which(ddd$table==tb),]
    species = unique(d4$sp.id)
    costpersp = 0
    costperspcount = 0
    for(j in 1:length(species))
    {
      if(species[j] %in% listedsp)
      {
        
        if(is.na(costpersp))
        {
          print(i)
          print(k)
          print(j)
        }
        costpersp = costpersp + sum(d4$annualized.cost[which(d4$sp.id==species[j])])
        costperspcount = costperspcount + 1
      }
    }
    if(costperspcount >0)
    {
      avgcostpersp = costpersp/costperspcount
      cost = cost + avgcostpersp
      costcount = costcount + 1
    }
  }
}
avgcost2 = cost / costcount # *average mitigation & monitoring annualized cost per listed fish per dam project

print(avgcost)
print(avgcost2)


# 2. get average cost of mitigation for mussel
dd = d[which(d$mussel.related==1),]
# by ferc docs
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  costperdoc = sum(ddd$annualized.cost)
  cost = cost + costperdoc
  costcount = costcount + 1
}
avgcost = cost / costcount # *average mitigation & monitoring of mussels annualized cost per ferc doc

# by projects ( by table )
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  for( k in 1:length(unique(ddd$table)))
  {
    tb = unique(ddd$table)[k]
    d4 = ddd[which(ddd$table==tb),]
    costpersp = sum(d4$annualized.cost)
    cost = cost + costpersp
    costcount = costcount + 1
  }
}
avgcost2 = cost / costcount # *average mitigation & monitoring of mussels annualized cost per dam project

print(avgcost)
print(avgcost2)


# 3.get average cost of mitigation for invasive species
dd = d[which(d$invasive.related==1),]
# by ferc docs
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  costperdoc = sum(ddd$annualized.cost)
  cost = cost + costperdoc
  costcount = costcount + 1
}
avgcost = cost / costcount # *average mitigation & monitoring of invasives annualized cost per ferc doc

# by projects ( by table )
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  for( k in 1:length(unique(ddd$table)))
  {
    tb = unique(ddd$table)[k]
    d4 = ddd[which(ddd$table==tb),]
    costpersp = sum(d4$annualized.cost)
    cost = cost + costpersp
    costcount = costcount + 1
  }
}
avgcost2 = cost / costcount # *average mitigation & monitoring of invasives annualized cost per dam project

print(avgcost)
print(avgcost2)



#4. get average cost of mitigation for game fish.
#4.1 find which species are game species.
filename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/measure data.csv'
d = read.csv(filename)
idx = which(!is.na(d$specific.sp) & is.na(d$mussel.related))
splist = unique(d$sp.id[idx])
#break down multiple species strings into single individuals (there are none anymore... 7/13/2023). 
multiplesp = splist[which(grepl(';',splist))]
addedsp = c()
if(length(multiplesp)>0)
{
  for(sp in multiplesp)
  {
    addedsp = c(addedsp,strsplit(sp,';'))
  }
  addedsp=unlist(addedsp)
  splist = splist[-which(grepl(';',splist))]
}
splist = unique(c(splist,addedsp))
# change rainbow to steel. they are the same
d$sp.id[which(d$sp.id=='rainbow')] = 'steel'
# file made from 1.1 and internet search.
filename3 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/listed sp in ea eis.csv'
game.data = read.csv(filename3)
gamesp = game.data$code[which(game.data$game==1)]

# 4.2 get average mitigation cost per species per ferc docs(P-XX) and per species per dam (table in a ferc doc)
dd = d[which(!is.na(d$specific.sp)& is.na(d$mussel.related)),]
head(dd)
#by ferc docs
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  species = unique(ddd$sp.id)
  cost_persp = 0
  cost_perspcount = 0
  for(j in 1:length(species))
  {
    if(species[j] %in% gamesp)
    {
      
      cost_persp = cost_persp + sum(ddd$annualized.cost[which(ddd$sp.id==species[j])])
      cost_perspcount = cost_perspcount + 1
    }
  }
  if(cost_perspcount > 0)
  {
    avgcost_persp = cost_persp/cost_perspcount
    cost = cost + avgcost_persp
    costcount = costcount + 1
  }
}
avgcost = cost / costcount # *average mitigation & monitoring annualized cost per listed fish per ferc doc
# by projects ( by table )
cost = 0
costcount = 0
for(i in 1:length(unique(dd$ferc.docket)))
{
  proj = unique(dd$ferc.docket)[i]
  ddd = dd[which(dd$ferc.docket==proj),]
  for( k in 1:length(unique(ddd$table)))
  {
    tb = unique(ddd$table)[k]
    d4 = ddd[which(ddd$table==tb),]
    species = unique(d4$sp.id)
    costpersp = 0
    costperspcount = 0
    for(j in 1:length(species))
    {
      if(species[j] %in% gamesp)
      {
        
        if(is.na(costpersp))
        {
          print(i)
          print(k)
          print(j)
        }
        costpersp = costpersp + sum(d4$annualized.cost[which(d4$sp.id==species[j])])
        costperspcount = costperspcount + 1
      }
    }
    if(costperspcount >0)
    {
      avgcostpersp = costpersp/costperspcount
      cost = cost + avgcostpersp
      costcount = costcount + 1
    }
  }
}
avgcost2 = cost / costcount # *average mitigation & monitoring annualized cost per listed fish per dam project

print(avgcost)
print(avgcost2)
