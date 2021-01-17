source("test_models.R",local = T)

data = readRDS("data.RDS")
h_size = 2

set.seed(1231)
mean_solo_acc = c()
mean_solo_auc = c()
for(j in 1:5){
  hospitals = lapply(1:h_size,function(x)train_test_hospital_no_reference_no_em(i = x,data = data)) ## spamming float warnings
  test_acc = lapply(hospitals,function(x)x$test_acc) %>% unlist()
  mean_solo_acc = c( mean_solo_acc,mean(test_acc) )
  test_auc = lapply(hospitals,function(x)x$test_auc) %>% unlist()
  mean_solo_auc = c(mean_solo_auc,mean(test_auc) )
}

mean(mean_solo_acc)
mean(mean_solo_auc)

# hospitals_em = lapply(1:10,function(x)train_test_hospital_no_reference_em(i = x,data = data)) ## spamming float warnings


# test_acc_em = lapply(hospitals_em,function(x)x$test_acc) %>% unlist()
# mean(test_acc_em)
# test_auc_em = lapply(hospitals_em,function(x)x$test_auc) %>% unlist()
# mean(test_auc_em)


saveRDS("RDS/solo_ANN.RDS",object = list(
  acc = mean_solo_acc,
  auc = mean_solo_auc
  # acc_em = test_acc_em,
  # auc_em = test_auc_em
))
