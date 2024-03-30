# make some regression tree models.
library(rpart.plot)
library(rpart)
library(ape)
library(psych)
library(car)
library(sets)
library(MASS)
library(MuMIn)

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
  nrow(fercd)
  nrow(vars)
  narowidx = which(apply(vars,1,function(x) any(is.na(x))))
  nrow(na.omit(vars))
  
  vars = na.omit(vars)
  names(vars)[which(names(vars)=='fercd$projectID')] = 'projectID'
}



target = vars$logfreshwc
dataset = vars[,3:ncol(vars)]
tree.fit <- rpart(target ~ ., data = dataset, method = "anova", control = rpart.control(minsplit = ceiling(nrow(vars))/10, cp = 0.01))
printcp(tree.fit)
plotcp(tree.fit)
rsq.rpart(tree.fit)
cptable = tree.fit$cptable
rsq_x = 1-cptable[,4] # X relative rsq
rsq_app = 1-cptable[,3] # aparent rsq
# best model
s1=which.min(tree.fit$cptable[,"xerror"])
pruned.rtree.fit1<- prune(tree.fit, cp= tree.fit$cptable[s1,"CP"])
rsq.pruned.rtree.fit1 = data.frame(apparent=rsq_app[s1],xrelative=rsq_x[s1])

# model based on the 1SE-rule (you select the simplest model (largest cp) 
#whose error is no more than one standard error above the error of the
#best model (the model with the lowest cross-validated error).)
cptable = tree.fit$cptable
se1 = cptable[7,4]+cptable[7,5]
diff = cptable[,4]-se1
diffneg = diff[which(diff<0)]
s2 = which(diff==diffneg[which.max(diffneg)])
pruned.rtree.fit2<- prune(tree.fit, cp= tree.fit$cptable[s2,"CP"])

# 1se-rule model with at least one 

# plot tree
prp(tree.fit)
prp(pruned.rtree.fit1)
prp(pruned.rtree.fit2)

plot(pruned.rtree.fit2, uniform=TRUE, 
     main="Regression Tree for Median Home Value")
text(pruned.rtree.fit2, use.n=TRUE, all=TRUE, cex=.8)

# predict with the tree
tree = pruned.rtree.fit1
prp(tree)
tree_frame <- tree$frame
variables_used <- unique(tree_frame$var[tree_frame$var != "<leaf>"])
spnumvariables_used = variables_used[which(grepl('_cr',variables_used))]
newdataset = dataset

for( variable in spnumvariables_used)
{
  futvariable = gsub('_cr','_fr',variable)
  newdat = fercd[,which(names(fercd)==futvariable)]
  newdataset[,which(names(newdataset)==variable)] = newdat[-narowidx]
}

varhuc8 = fercd$huc8[-narowidx]
projid = fercd$projectID[-narowidx]
current_predict = predict(tree, newdata=dataset)
future_predict = predict(tree, newdata=newdataset)
costdiff=exp(current_predict)-exp(future_predict)
length(costdiff)
pos_costdiff= data.frame(proj_id=projid[which(costdiff!=0)],huc8=varhuc8[which(costdiff!=0)],logcost_diff=costdiff[which(costdiff!=0)])



#predict(pruned.rtree.fit1, newdata=newdataset)[1:10]
#predict(tree.fit, newdata=newdataset)[1:10]
#predict(pruned.rtree.fit2, newdata=newdataset)[1:10]
