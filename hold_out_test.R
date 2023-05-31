options(java.parameters = "-Xmx2048m")  
library(xlsx)
data_path = '/Users/ali/Desktop/may23/hold_out_test_age/APOE234_Phenotype_mrtrix.csv'
data = as.data.frame(read.csv2(data_path, sep = ",")) %>% select(SUB_ID, AGE_MONTHS)
data$AGE_MONTHS = as.numeric(data$AGE_MONTHS)
data$SUB_ID = as.numeric(data$SUB_ID)

#k=5 fold:
k=5
fold_size = round(dim(data)[1] /k)


numit = 500000
results = matrix(NA, numit, fold_size+1)

set.seed(234)
for (it in 1:numit) {
#it = 1
set = sample( data$SUB_ID  ,  fold_size)
sample_index = which (data$SUB_ID %in% set)
results[it,1:fold_size] = sample_index
sample = data [sample_index , ]
results[it, fold_size+1]  = shapiro.test(sample$AGE_MONTHS)$p.value
}
  
winner = which (results[,fold_size+1] == min(results[,fold_size+1]))
winner_indecies = results[winner, 1:fold_size]
winner_sample = data[winner_indecies,]
#shapiro.test(winner_sample$AGE_MONTHS)$p.value == results[winner, fold_size+1]
# the wil shaprio normality test p-value of the hold out test:
shapiro.test(winner_sample$AGE_MONTHS)$p.value
#histogram 
hist(winner_sample$AGE_MONTHS, col='steelblue', main = "Hold Out Test Age Distribution")
#QQplot
qqnorm(winner_sample$AGE_MONTHS, main=paste0('QQ-Plo with Shairo test P-value =', shapiro.test(winner_sample$AGE_MONTHS)$p.value) )
qqline(winner_sample$AGE_MONTHS)
# sub IDs of hold out test
winner_sample$SUB_ID

sample_holdout = winner_sample




# lets find the second best fold from remaining of the data when test set is taken out:
rem_data = data[-winner_indecies,]
results = matrix(NA, numit, fold_size+1)


for (it in 1:numit) {
  #it = 1
  set = sample( rem_data$SUB_ID  ,  fold_size)
  sample_index = which (rem_data$SUB_ID %in% set)
  results[it,1:fold_size] = sample_index
  sample = rem_data [sample_index , ]
  results[it, fold_size+1]  = shapiro.test(sample$AGE_MONTHS)$p.value
}

winner = which (results[,fold_size+1] == min(results[,fold_size+1]))
winner_indecies = results[winner, 1:fold_size]
winner_sample = rem_data[winner_indecies,]
#shapiro.test(winner_sample$AGE_MONTHS)$p.value == results[winner, fold_size+1]
# the wil shaprio normality test p-value of the second set:
shapiro.test(winner_sample$AGE_MONTHS)$p.value
#histogram 
hist(winner_sample$AGE_MONTHS, col='steelblue', main = "Validation Set Age Distribution")
#QQplot
qqnorm(winner_sample$AGE_MONTHS, main=paste0('QQ-Plo with Shairo test P-value =', shapiro.test(winner_sample$AGE_MONTHS)$p.value) )
qqline(winner_sample$AGE_MONTHS)
# sub IDs of second set
winner_sample$SUB_ID

sample_validation = winner_sample




t.test(sample_validation$AGE_MONTHS,sample_holdout$AGE_MONTHS)
# fail to reject inequality of means 

var.test(sample_validation$AGE_MONTHS,sample_holdout$AGE_MONTHS, alternative = "two.sided")
# fail to reject inequality of variances 

ks.test(sample_validation$AGE_MONTHS,  sample_holdout$AGE_MONTHS)
# fail to reject they are from the same distribution (H_0) vs. they are from different distrubtion (H_a)
