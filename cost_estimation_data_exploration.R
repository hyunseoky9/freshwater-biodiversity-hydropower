# data exploration.
rm(list=ls())
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename2)
fercd = fercd[which(fercd$including.in.the.study==1),]
freshwc = fercd$c.freshWsp.cons.related # first potential dependent variable for the regression. 
freshwcmw = fercd$cpermw.freshWsp.cons.related
# total cost that affects fish and mussel conservation
spc = fercd$c.sp.specific # another protential dependent variable for the regression

# total cost that targets specific species of taxonomic groups.

# box plots
par(mfrow=c(1,3))
boxplot(freshwc,main='total cost of freshwater sp. conservation related measures')
boxplot(spc,main='Total cost of species specific measures')
boxplot(freshwcmw,main='Total cost of freshwater conservation
related measures/power capacity(MW)')
# box plots for both potential dependent variables show there are clear outliers
hist(freshwc,main='total cost of freshwater sp. conservation related measures')
hist(spc,main='Total cost of species specific measures')
hist(freshwcmw,main='Total cost of freshwater conservation
related measures/power capacity(MW)')



length(which(freshwc==0))
length(which(spc==0))
# freshwc also have 12 samples with 0 values out of 169 samples (7% of samples)
# spc have 58 sapmles with 0 values out of 169 samples (34%)

# let's try log transforming the variable with +1 to correct for 0 values
logfreshwcmw = log(freshwcmw+1)
logfreshwc = log(freshwc+1)
logspc = log(spc+1)
par(mfrow=c(1,3))
boxplot(logfreshwc,main='log transformed total cost of 
        freshwater sp. conservation related measures')
boxplot(logfreshwcmw,main='log transformedTotal cost of freshwater conservation
related measures/power capacity(MW)')
boxplot(logspc,main='Total cost of species specific measures')

hist(logfreshwc,main='log transformed total cost of 
        freshwater sp. conservation related measures')
hist(logfreshwcmw,main='log transformedTotal cost of freshwater conservation
related measures/power capacity(MW)')
hist(logspc,main='Total cost of species specific measures')

# there is one project with negative total cost due to the power gained from maintaining lake level.
# see first measure in P-2503 (table 4-3).
fercd$projectID[which(freshwc<0)]
fercd$projectID[which(freshwcmw<0)]
fercd$projectID[which(spc<0)]

# dot plot 
plot(logfreshwc)
plot(logfreshwcmw)
logfreshwc

freshwc
plot(logspc)
# histogram of log variable.
hist(logfreshwc)  # kind of left skewed
hist(logfreshwcmw)
hist(logspc) # kind of right skewed

# histogram of the variable that are run-of-river & not run-of-river
hist(logfreshwc[which(fercd$run.of.river==1)],main='total cost for run-of-river dams')
hist(logfreshwc[which(is.na(fercd$run.of.river))], main='total cost for regular dams')

hist(logfreshwcmw[which(fercd$run.of.river==1)],main='total cost for run-of-river dams')
hist(logfreshwcmw[which(is.na(fercd$run.of.river))], main='total cost for regular dams')

# histogram of total cost of measures related to some theme
# mussel, inv.fish, inv.mussel, game, listed, sp.specific, 
# wqualityNtemp, hatchery, fishing, flow, passage, 
candidateresvars = fercd[,c('c.mussel.related', 'c.invfish.related', 'c.invmussel.related', 'c.game.related', 'c.listed.related', 'c.sp.specific', 'c.WqualityNtemp.related', 'c.hatchery.related', 'c.fishing.related', 'c.flow.related', 'c.passage.related')]
for( i in 1:ncol(candidateresvars))
{
  subj = candidateresvars[,i]
  name = names(candidateresvars)[i]
  logsubj = log(subj+1)
  par(mfrow=c(1,4))
  hist(subj,main=name)
  hist(logsubj,main=sprintf('%s logged',name))
  qqnorm(logsubj)
  qqline(logsubj)
  
  qqnorm(logsubj[which(logsubj!=0)],main=sprintf('without 0s (n=%d)',length(which(logsubj!=0))))
  qqline(logsubj[which(logsubj!=0)])
  readline()
}


## qqplot

par(mfrow=c(1,2))
qqnorm(freshwc,main='total cost of freshwater sp. cons. measures')
qqline(freshwc)
qqnorm(logfreshwc,main='log transformed version')
qqline(logfreshwc)

qqnorm(freshwcmw,main='total cost of freshwater sp. cons. measures
       /power capacity')
qqline(freshwcmw,main='total cost of freshwater sp. cons. measures')
qqnorm(logfreshwcmw,main='log transformed version')
qqline(logfreshwcmw)

qqnorm(logfreshwcmw)
qqline(logfreshwcmw)
lambda = 1/2
t.freshwc = ((freshwc)^(lambda)-1)/lambda
t.spc = ((spc)^(lambda)-1)/lambda
qqnorm(t.freshwc)
qqline(t.freshwc)

# power capacity exploration
hist(fercd$power.capacity.MW.,main='power capacity')
hist(log(fercd$power.capacity.MW.),main='power capacity')
pairs(cbind(logfreshwc,log(fercd$power.capacity.MW.)),panel=panel.smooth)
cor(na.omit(cbind(logfreshwc,log(fercd$power.capacity.MW.))))

# based on other transformations (sqrt, 1/3 root), log transformation looks the most normal 

## pair plot
# 1) potential response vars + all SDM explanatory variables + cp explanatory variables
candidatevars = cbind(logfreshwc,fercd[,c("c.freshWsp.cons.related","cp.freshWsp.cons.related",
                         "cpermw.freshWsp.cons.related","c.sp.specific","huc2",
                         "run.of.river","pump.storage","power.capacity.MW.",
                         "annual.power.production..MWh.annually.","totalspnum_cr",
                         "listedspnum_cr","nonnativespnum_cr","gamespnum_cr","diadromous.spnum_cr",
                         "migratory.spnum_cr","coldwaterspnum_cr","warmwaterspnum_cr",
                         "coolwaterspnum_cr","musselspnum_cr","nonnativemusselspnum_cr",
                         "nativemusselspnum_cr",
                         "cp.flow.related","cp.WqualityNtemp.related","cp.passage.related",
                         "cp.migratingsp.related","cp.listed.related","cp.game.related",
                         "cp.mussel.related","cp.invmussel.related","cp.invfish.related",
                         "cp.fishing.related","cp.hatchery.related","p2.flow.related",
                         "p2.passage.related","p2.migratingsp.related","p2.listed.related",
                         "p2.mussel.related","p2.invmussel.related","p2.invfish.related","p2.fishing.related","p2.WqualityNtemp.related",
                         "p2.hatchery.related","cp2.flow.related","cp2.passage.related","cp2.migratingsp.related",
                         "cp2.listed.related","cp2.mussel.related","cp2.invmussel.related","cp2.invfish.related",
                         "cp2.fishing.related","cp2.WqualityNtemp.related","cp2.hatchery.related")])

# 2) log transformed freshWsp response var + explanatory vars after getting rid of correlated ones
candidatevars = cbind(logfreshwc,fercd[,c("power.capacity.MW.","totalspnum_cr","musselspnum_cr",
                                          "diadromous.spnum_cr","cp.flow.related",
                                          "cp.WqualityNtemp.related","cp.passage.related",
                                          "cp.migratingsp.related","cp.listed.related",
                                          "cp.mussel.related","cp.invfish.related",
                                          "cp.fishing.related")])
# 4) log transformed freshWsp response var + explanatory vars after getting rid of correlated ones (cp and p's)
candidatevars = cbind(logfreshwc,fercd[,c("power.capacity.MW.","totalspnum_cr",
                                          "diadromous.spnum_cr","cp.flow.related","p.flow.related",
                                          "cp.WqualityNtemp.related","p.WqualityNtemp.related","cp.passage.related","p.passage.related",
                                          "cp.migratingsp.related","p.migratingsp.related","cp.listed.related","p.listed.related",
                                          "cp.mussel.related","p.mussel.related","cp.invfish.related","p.invfish.related",
                                          "cp.fishing.related","p.fishing.related")])
# 5) same as above but with cp2 and p2 instead of cp and p.
candidatevars = cbind(logfreshwc,fercd[,c("power.capacity.MW.","totalspnum_cr",
                                          "diadromous.spnum_cr","cp2.flow.related","p2.flow.related",
                                          "cp2.WqualityNtemp.related","p2.WqualityNtemp.related","cp2.passage.related","p2.passage.related",
                                          "cp2.migratingsp.related","p2.migratingsp.related","cp2.listed.related","p2.listed.related",
                                          "cp2.mussel.related","p2.mussel.related","cp2.invfish.related","p2.invfish.related",
                                          "cp2.fishing.related","p2.fishing.related")])
# 6) same as 5) but excluding correlated ones (p2.wquality N temp, p2. invfish related, )
candidatevars = cbind(logfreshwc,fercd[,c("power.capacity.MW.","totalspnum_cr",
                                          "diadromous.spnum_cr","cp2.flow.related","p2.flow.related",
                                          "cp2.WqualityNtemp.related","cp2.passage.related","p2.passage.related",
                                          "cp2.migratingsp.related","p2.migratingsp.related","cp2.listed.related","p2.listed.related",
                                          "cp2.mussel.related","p2.mussel.related","cp2.invfish.related",
                                          "cp2.fishing.related","p2.fishing.related")])
# 7) same as 6) but without count based proportions (p2) 
candidatevars = cbind(logfreshwc,fercd[,c("power.capacity.MW.","totalspnum_cr",
                                          "diadromous.spnum_cr","cp2.flow.related",
                                          "cp2.WqualityNtemp.related","cp2.passage.related",
                                          "cp2.migratingsp.related","cp2.listed.related",
                                          "cp2.mussel.related","cp2.invfish.related",
                                          "cp2.fishing.related")])


# measure related explanatory variables
# c. p. and cp.
cNp = fercd[,c("p.freshWsp.cons.related","p.flow.related","c.flow.related","cp.flow.related","p.passage.related","c.passage.related","cp.passage.related","p.migratingsp.related","c.migratingsp.related","cp.migratingsp.related","p.listed.related","c.listed.related","cp.listed.related","p.mussel.related","c.mussel.related","cp.mussel.related","p.invfish.related","c.invfish.related","cp.invfish.related","p.fishing.related","c.fishing.related","cp.fishing.related","p.WqualityNtemp.related","c.WqualityNtemp.related","cp.WqualityNtemp.related")]
# p.
cNp = fercd[,c("p.freshWsp.cons.related","p.flow.related","p.passage.related","p.migratingsp.related","p.listed.related","p.mussel.related","p.invfish.related","p.fishing.related","p.WqualityNtemp.related")]
# p2.
cNp = fercd[,c("p2.flow.related","p2.passage.related","p2.migratingsp.related","p2.listed.related","p2.mussel.related","p2.invfish.related","p2.fishing.related","p2.WqualityNtemp.related")]
cNp2 = fercd[,c("p.flow.related","p.passage.related","p.migratingsp.related","p.listed.related","p.mussel.related","p.invfish.related","p.fishing.related","p.WqualityNtemp.related")]
# cp2
cNp = fercd[,which(grepl('cp2\\.',names(fercd)))]
cNp = fercd[,c("cp2.flow.related","cp2.passage.related","cp2.migratingsp.related",
               "cp2.listed.related","cp2.mussel.related","cp2.invfish.related",
               "cp2.fishing.related","cp2.WqualityNtemp.related","cp2.natmussel.related",
               "cp2.invmussel.related","cp2.migratingsp2.related")]
cNp2 = fercd[,c("cp.flow.related","cp.passage.related","cp.migratingsp.related","cp.listed.related","cp.mussel.related","cp.invfish.related","cp.fishing.related","cp.WqualityNtemp.related")]
# c.
cNp2 = fercd[,c("c.flow.related","c.passage.related","c.migratingsp.related","c.listed.related","c.mussel.related","c.invfish.related","c.fishing.related","c.WqualityNtemp.related")]

# pair plot
pairs(candidatevars,panel=panel.smooth)
for( i in 2:ncol(candidatevars))
{
  plot(candidatevars[,i],logfreshwc,xlab=names(candidatevars)[i])
  readline()
}

# box plots for explanatory variables
for(i in 1:length(names(candidatevars)))
{
  boxplot(candidatevars[,i],main=names(candidatevars)[i])
  print(sprintf('percentage of 0s in the variable: %.3f',length(which(candidatevars[,i]==0))/length(candidatevars[,i])))
  readline()
}
for(i in 1:length(names(cNp)))
{
  par(mfrow=c(1,2))
  boxplot(cNp[,i],main=names(cNp)[i],ylim=c(0,1))
  boxplot(cNp2[,i],main=names(cNp2)[i],ylim=c(0,1))
  readline()
}
for(i in 1:length(names(cNp)))
{
  par(mfrow=c(1,2))
  hist(log(cNp[,i]+1),main=sprintf('log of %s',names(cNp)[i]))
  hist(cNp[,i],main=names(cNp)[i])
  print(sprintf('percentage of 0s in the variable for %s: %.3f',names(cNp)[i],length(which(cNp[,i]==0))/length(cNp[,i])))
  readline()
}


# correlation matrix
cort=cor(na.omit(candidatevars))
cortcNp=cor(na.omit(cNp))
cort = cortcNp
# print ones with absolute correlation above 0.7

for ( i in 1:nrow(cort))
{
  print(i)
  idx = which(abs(cort[i,-i])>0.7)
  if(length(idx)>0)
  {
    for( j in idx)
    {
      if((j+1)>i)
      print(sprintf('(%s,%s):%f',rownames(cort)[i],names(cort[i,-i])[j],cort[i,-i][which(abs(cort[i,-i])>0.7)[which(idx==j)]])) 
    }
  }
}
# total species number, listed sp number, and game species number in huc8 highly correlated
# power capacity and annual power production highly correlated
# invasive mussel related measure cost proportion and mussel related measure cost proportion highly correlated.
# cost of freshwater sp. conservation relevant measures and cost of species specific measures correlated.

t(cort[,which(colnames(cort)=="cpermw.freshWsp.cons.related")])
t(cort[,which(colnames(cort)=="c.freshWsp.cons.related")])
t(cort[,which(colnames(cort)=="power.capacity.MW.")])
t(cort[,which(colnames(cort)=="logfreshwc")])
"cp.freshWsp.cons.related"

# coplot
coplot(logfreshwc~fercd$diadromous.spnum_cr|fercd$huc2,number=3,overlap=0)
coplot(logfreshwc~fercd$diadromous.spnum_cr|fercd$Project_Region_dummy,number=4,overlap=0)

drawloess = function(y,x)
{
  nonnaidx = which(!is.na(y) & !is.na(x))
  x = x[nonnaidx]
  y = y[nonnaidx]
  x=sort(x)
  y=y[order(x)]
  zeroidx = which(rowSums(cbind(x,y))==0 | x==0)
  x=x[-zeroidx]
  y=y[-zeroidx]
  
  mod = loess(y~x)
  lines(x,predict(mod),col='red')
}


# c to cp2
par(mfrow=c(1,1))
plot(log(1+fercd$c.mussel.related)~fercd$cp2.mussel.related)
plot(log(1+fercd$c.invfish.related)~fercd$cp2.invfish.related)
plot(log(1+fercd$c.invmussel.related)~fercd$cp2.invmussel.related)
plot(log(1+fercd$c.game.related)~fercd$cp2.game.related)
plot(log(1+fercd$c.listed.related)~fercd$cp2.listed.related)
plot(log(1+fercd$c.sp.specific)~fercd$cp2.sp.specific)
plot(log(1+fercd$c.WqualityNtemp.related)~fercd$cp2.WqualityNtemp.related)
plot(log(1+fercd$c.hatchery.related)~fercd$cp2.hatchery.related)
plot(log(1+fercd$c.fishing.related)~fercd$cp2.fishing.related)
plot(log(1+fercd$c.flow.related)~fercd$cp2.flow.related)
plot(log(1+fercd$c.passage.related)~fercd$cp2.passage.related)
# c to p2
plot(log(1+fercd$c.mussel.related)~fercd$p.mussel.related)
plot(log(1+fercd$c.invfish.related)~fercd$p.invfish.related)
plot(log(1+fercd$c.invmussel.related)~fercd$p.invmussel.related)
plot(log(1+fercd$c.game.related)~fercd$p.game.related)
plot(log(1+fercd$c.listed.related)~fercd$p.listed.related)
plot(log(1+fercd$c.sp.specific)~fercd$p.sp.specific)
plot(log(1+fercd$c.WqualityNtemp.related)~fercd$p.WqualityNtemp.related)
plot(log(1+fercd$c.hatchery.related)~fercd$p.hatchery.related)
plot(log(1+fercd$c.fishing.related)~fercd$p.fishing.related)
plot(log(1+fercd$c.flow.related)~fercd$p.flow.related)
plot(log(1+fercd$c.passage.related)~fercd$p.passage.related)
# c (species related measures) to spnum
plot(log(1+fercd$c.mussel.related)~fercd$musselspnum_cr)
plot(log(1+fercd$c.invfish.related)~fercd$nonnativespnum_cr)
plot(log(1+fercd$c.invmussel.related)~fercd$nonnativespnum_cr)
plot(log(1+fercd$c.invfish + fercd$c.invmussel.related),fercd$nonnativespnum_cr)
plot(log(1+fercd$c.game.related)~fercd$gamespnum_cr)
plot(log(1+fercd$c.migratingsp.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.migratingsp.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.migratingsp.related + fercd$c.passage.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.migratingsp.related + fercd$c.passage.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.passage.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.passage.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.listed.related)~fercd$listedspnum_cr)
plot(log(1+fercd$c.listed.related)~fercd$diadromous.spnum_cr)
# c (indirect measure) to spnum
plot(log(1+fercd$c.sp.specific)~fercd$totalspnum_cr)
plot(log(1+fercd$c.sp.specific)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.sp.specific)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.sp.specific)~fercd$gamespnum_cr)

plot(log(1+fercd$c.WqualityNtemp.related)~fercd$totalspnum_cr)
plot(log(1+fercd$c.WqualityNtemp.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.WqualityNtemp.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.WqualityNtemp.related)~fercd$gamespnum_cr)

plot(log(1+fercd$c.hatchery.related)~fercd$totalspnum_cr)
plot(log(1+fercd$c.hatchery.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.hatchery.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.hatchery.related)~fercd$gamespnum_cr)

plot(log(1+fercd$c.fishing.related)~fercd$totalspnum_cr)
plot(log(1+fercd$c.fishing.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.fishing.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.fishing.related)~fercd$gamespnum_cr)

plot(log(1+fercd$c.flow.related)~fercd$totalspnum_cr)
plot(log(1+fercd$c.flow.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.flow.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.flow.related)~fercd$gamespnum_cr)

plot(log(1+fercd$c.passage.related)~fercd$totalspnum_cr)
plot(log(1+fercd$c.passage.related)~fercd$diadromous.spnum_cr)
plot(log(1+fercd$c.passage.related)~fercd$migratory.spnum_cr)
plot(log(1+fercd$c.passage.related)~fercd$gamespnum_cr)



coplot(fercd$c.mussel.related)

coplot(logfreshwc~fercd$cp2.WqualityNtemp.related|fercd$Project_Region_dummy,number=4,overlap=0)
coplot(logfreshwc~log(fercd$power.capacity.MW.)|fercd$Project_Region_dummy,number=4,overlap=0)



coplot(logfreshwcmw~fercd$diadromous.spnum_cr|fercd$huc2*fercd$power.capacity.MW.,number=c(3,2),overlap=0)
plot(fercd$power.capacity.MW.)

plot(logfreshwc,log(fercd$power.capacity.MW.))
logpowercap = log(fercd$power.capacity.MW.)
pairs(cbind(logfreshwc,logpowercap),panel=panel.smooth)


