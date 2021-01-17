library(randomForest)
library(magrittr)


train_tree = function(x,y){
  model <- randomForest(x=x,y=as.factor(y),ntree=100)
  list(
    model = model
  )
}

eval_tree_data = function(x,y,model){
  preds = predict(object = model,newdata = as.data.frame(x),type = "vote")
  preds = preds[,2]/(preds[,1] + preds[,2])
  acc = sum(round(preds)==y) / nrow(x)
  auc = cvAUC::AUC(predictions = preds,labels = factor(y,levels = c(0,1)))
  return(list(
    preds = preds,
    acc= acc,
    auc = auc
  ))
}


train_test_hospital_solo_tree = function(i,data){
  tmp_data = data[[paste0("hospital_",i)]]
  test = data$test
  
  tree = train_tree(tmp_data$x,tmp_data$y)
  eval_test = eval_tree_data(test$x,test$y,tree$model)
  return(
    list(
      models = tree,
      test_preds = eval_test$preds,
      test_acc = eval_test$acc,
      test_auc = eval_test$auc
    )
  )
}
