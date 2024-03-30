# count how many species are included in the species if we only take up to certain
# amount of species per family/ per genus

setwd('F:/sdm_modeling/spdata/')
sp_list = read.csv('comprehensive_sp_info.csv')

# for family
count = table(sp_list$family)
unique(sp_list$family)
barplot(table(sp_list$family))
hist(sp_list$family)
countcurbed = count
thresholds = seq(5,258)
fun<- function(x,array)
{
  array[which(array>x)] = x
  sum(array)
}
fun(5,count)
countcurbed[which(countcurbed > 5)] = 5 #families with more than 5 species gets curbed to 5 species.
sum(count)
sum(countcurbed)
numsp_perth = apply(matrix(thresholds,ncol=1),1,fun,array=count) # how many species are in the study if we increase the threshold from 5 to 258
names(numsp_perth) = thresholds


# for genus
count = table(sp_list$genus)
unique(sp_list$genus)
barplot(table(sp_list$genus))
hist(sp_list$genus)
countcurbed = count
countcurbed[which(countcurbed > 5)] = 5 #families with more than 5 species gets curbed to 5 species.
sum(count)
sum(countcurbed)



#only including species that are either threatened, game, or invasive. 
categories = sp_list[,c('game','threatened','nonindigenous')]
scoresum = apply(categories,1,sum)
length(which(scoresum>0))
