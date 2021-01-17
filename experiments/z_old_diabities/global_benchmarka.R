source("test_models.R")
data = readRDS("data.RDS")

set.seed(1231)
h_size = 20


x = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]$x %>% as.data.frame()
}) %>% rbindlist() %>% as.matrix()
y = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]$y
}) %>% unlist()

# x = rbind(x,data$reference)
# y = c(y,data$reference_y %>% as.character() %>% as.numeric())
#no  embedd ------------------------------------------------------------------

## maybe do 100 reruns -- for MC analysis

no_emm_acc = c()
no_emm_auc = c()

for(i in 1:5){
h1 = create_hospital_model()
tmp_dec = train_model(50,h1$decoder,x_data = x ,y_data = y)
h1$decoder = tmp_dec$new_model
test_no_emm = evaluate_test_no_em(data$test$x,data$test$y,h1)
no_emm_acc = c(no_emm_acc,test_no_emm$acc)
no_emm_auc = c(no_emm_auc,test_no_emm$auc)
  
}

saveRDS(list(acc=no_emm_acc,auc=no_emm_auc),file = "RDS/global_ANN.RDS")

mean(no_emm_acc)
mean(no_emm_auc)

# embedd ------------------------------------------------------------------

# 
# h = create_hospital_model()
# tmp_ae = train_model(500,h$ae,x_data =x,y_data = x)
# h$ae = tmp_ae$new_model
# tmp_dec = train_model(50,h$decoder,x_data =get_embedding(x,model_ = h$ae) ,y_data = y)
# h$decoder = tmp_dec$new_model
# test_emm = evaluate_test_em(data$test$x,data$test$y,h)
# test_emm$acc
# test_emm$auc




# rf ----------------------------------------------------------------------

# source("hoeff_trees.R")
# 
# new = train_tree(x,y)
# test_rf = eval_tree_data(data$test$x,data$test$y,new$model)
# test_rf$acc
# test_rf$auc
# 
# 
# saveRDS("rds/global_metrics.RDS",object = list(
#   embedding = list(
#     acc=test_emm$acc,
#     auc=test_emm$auc
#   ),
#   no_embedding = list(
#     acc=test_no_emm$acc,
#     auc=test_no_emm$auc
#   ),  
#   rf = list(
#     acc=test_rf$acc,
#     auc=test_rf$auc
#   )
# )
# )

