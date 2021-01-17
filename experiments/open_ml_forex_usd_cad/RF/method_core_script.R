source("models_code.R",local = T)

data = readRDS("../data.RDS")
data$reference = data$reference[sample(1:nrow(data$reference),size = input_p),]

set.seed(1231)
n_h = 20
### each hospital learn its own embedding with reference data and its own data

solo = lapply(1:n_h, function(x){
  train_test_hospital_solo_tree(x,data)
})

## just hoarding data, relearning whole stream on each phase since it is easier wtih current MOA implementation

hospitals = lapply(1:n_h, function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  tree = train_tree(tmp_data$x,tmp_data$y)
  list(x = tmp_data$x,
       y=tmp_data$y,
       model=tree)
})


### now lets get started

### functions that embed their own data, then sends it to all other to decode it and update their embedding based od alligned embedding

allign_predictions = function(i){
  
  current_model = hospitals[[i]]
  current_data = current_model
  
  ## working data
  tmp = data$reference
  # tmp = tmp[sample(1:nrow(tmp),size = nrow(tmp)/5),]
  ## create temp target Y
  others = setdiff(1:n_h,i)
  other_embeddings = lapply(others,function(x){
    tmp_model = hospitals[[x]]
    predict(tmp_model$model$model,newdata = as.data.frame(tmp),type = "vote")
  })
  holder = other_embeddings[[1]]
  for(j in 2:length(other_embeddings)){
    holder = holder + other_embeddings[[j]]
  }
  holder = holder/length(other_embeddings)
  holder = holder[,2] / (holder[,1] + holder[,2])
  
  # ind = sample(1:nrow(tmp),size = min(100,nrow(tmp)))
  new_x = rbind(current_data$x,tmp)
  new_y = c(current_data$y,round(holder))
  
  tmp_tree = train_tree(new_x,new_y)
  
  current_model$model = tmp_tree
  # current_model$x = new_x
  # current_model$y = new_y
  
  rm(tmp)
  ## global assign
  hospitals[[i]] <<- current_model
  NULL
  
}

get_mean_test_predictions = function(){
  acc = c()
  auc = c()
  for(i in 1:n_h){
    ev = eval_tree_data(x = data$test$x,y = data$test$y,model = hospitals[[i]]$model$model)
    acc = c(acc,ev$acc)
    auc = c(auc,ev$auc)
  }
  return(
    list(
      acc=acc,
      auc=auc
    )
  )
}


epoch = 5

init = get_mean_test_predictions()

test_acc = c(mean(init$acc))
test_auc = c(mean(init$auc))



for(e in 1:epoch){
  
  lapply(1:n_h,allign_predictions)
  init = get_mean_test_predictions()
  
  test_acc = c(test_acc,mean(init$acc))
  test_auc = c(test_auc,mean(init$auc))
  
  plot(test_acc,type="l",ylim = c(0.5,0.65),main=paste0("Reference size: ",input_p))
  lines(test_auc,type="l",col="red")
  
}


output_stats = list(
  acc = test_acc,
  auc = test_auc
)


