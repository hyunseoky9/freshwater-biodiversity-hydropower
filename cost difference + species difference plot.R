# 
library(gridExtra)
filename2 = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/ferc docs details.csv'
fercd = read.csv(filename2)
fercd = fercd[which(fercd$including.in.the.study==1),]
filename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/linear_regression_model_predictions.csv'
pred = read.csv(filename)
varsfilename = 'G:/My Drive/research/sdm_modeling/environmental mitigation data/regression_variables(na_omitted).csv'
vars = read.csv(varsfilename)

idx = match(vars$projectID,fercd$projectID)
currspnum = cbind(fercd$nativemusselspnum_cr[idx],fercd$diadromous2.spnum_cr[idx],fercd$gamespnum_cr[idx])
futspnum = cbind(fercd$nativemusselspnum_fr[idx],fercd$diadromous2.spnum_fr[idx],fercd$gamespnum_fr[idx])
diffspnum = as.data.frame(futspnum - currspnum)
names(diffspnum) = c('nativemussel','diadromous','game')

order = order(vars$cdiff_pred,decreasing=TRUE)
layout.matrix <- matrix(c(1,2,3,4), nrow = 4, ncol = 1)
layout(mat = layout.matrix,
       heights = c(1,1), # Heights of the two rows
       widths = c(1,1)) # Widths of the two columns
varsordered = vars[order,]
vocdiff = varsordered$cdiff_pred

varsordered$cdiff_pred_log_modulus = (vocdiff+1)/abs(vocdiff+1)*log10(abs(vocdiff+1))

diffspnumordered = diffspnum[order,]
p1 = ggplot(data=varsordered,aes(y=cdiff_pred_log_modulus, x=1:nrow(vars))) +
  geom_point() + ylab('') + xlab('')#+ theme( axis.text.y = element_blank())
p2 = ggplot(data=diffspnumordered,aes(y=game,x=1:nrow(vars))) + 
  geom_point() + ylim(-6,6) + ylab('') + xlab('')
p3 = ggplot(data=diffspnumordered,aes(y=nativemussel,x=1:nrow(vars))) + 
  geom_point() + ylim(-6,6) + ylab('') + xlab('')
p4 = ggplot(data=diffspnumordered,aes(y=diadromous,x=1:nrow(vars))) + 
  geom_point() + ylim(-6,6) + ylab('') + xlab('')

grid.arrange(p1,p2,p3,p4,nrow = 4)

  