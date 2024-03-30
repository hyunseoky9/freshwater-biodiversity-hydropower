# AIC values of species in /sdm_results 
spnamedata = read.csv('G:/My Drive/research/sdm_modeling/spdata/comprehensive_sp_info.csv')

numoccs = c()
for ( i in 1:nrow(spnamedata))
{
  spdatafilename = sprintf('G:/My Drive/research/sdm_modeling/spdata/per_sp/%s_wcomid.csv',spnamedata$name[i])
  spdata = read.csv(spdatafilename)
  numoccs[i] = length(which(spdata$wbmID_30sec_netsymdiff<0.4))
}
sorted  = sort(numoccs,decreasing=T)
idx0 = which(sorted>=30)
idx0 = idx0[length(idx0)]
best100 = order(numoccs,decreasing=T)[1:idx0]

training.auc = c()
test.auc = c()
for(i in best100)
{
  filename = sprintf('G:/My Drive/research/sdm_modeling/sdm_results/%s_pristinerun_df3_lownetsymdiff0.40_testtest/maxentResults.csv',spnamedata$name[i])
  d = read.csv(filename)
  training.auc = c(training.auc,d$Training.AUC)
  test.auc = c(test.auc, d$Test.AUC)
}
training.auc
test.auc
dd = data.frame(training.auc,test.auc)
dd2 = data.frame(test.auc = test.auc)
boxplot(dd2,ylab='AUC',main='AUC values for 100 species with the most occurrence points (netsymdiff>0.4)',
        names=c('test AUC'),cex.axis=2)

boxplot(c(1,2,3))
boxplot(dd,ylab='AUC',main='AUC values for 100 species with the most occurrence points (netsymdiff>0.4)',
        names=c('training AUC','test AUC'),cex.axis=1.6,cex.lab=1.5)
