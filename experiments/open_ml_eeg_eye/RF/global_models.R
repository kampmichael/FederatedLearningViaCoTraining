source("models_code.R")

set.seed(1231)

n_h = 20
data = readRDS("../data.RDS")


x = lapply(1:n_h,function(x){
  data[[x]]$x
}) %>% do.call(rbind,.)

y = lapply(1:n_h,function(x){
  data[[x]]$y
}) %>% unlist()

x = rbind(x,data$reference)
y = c(y,data$reference_y)

out_acc = c()
out_auc = c()

for(i in 1:20){
  
  model = train_tree(x,y)
  preds = eval_tree_data(data$test$x,data$test$y,model$model)
  out_auc = c(out_auc,preds$auc)
  out_acc = c(out_acc,preds$acc)
  
}

saveRDS("results_rds/global_all_data.RDS",object = list(acc=out_acc,auc=out_auc))


