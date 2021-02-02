data = readRDS("../data/data_50_n5.RDS")
data_clone = data
n_h = 5
h_size = 5
set.seed(1231)
ee=10


hospitals_result = data.frame(
  type=NA,h1_acc=NA,h2_acc=NA,h3_acc=NA,h4_acc=NA,h5_acc=NA,h1_auc=NA,h2_auc=NA,h3_auc=NA,h4_auc=NA,h5_auc=NA
)

holder_results = rep(NA,11)

library(randomForest)
library(magrittr)
library(projectXcppCode)

## same code should work on CPU with just normal tensorflow/keras

## library(keras)
## library(tensorflow)



# functions ---------------------------------------------------------------

train_tree = function(x,y){
  model <- randomForest(x=x,y=as.factor(y),ntree=200)
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
      test_acc = eval_test$acc,
      test_auc = eval_test$auc
    )
  )
}


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



get_test_predictions = function(){
  acc = c()
  auc = c()
  for(i in 1:n_h){
    ev = eval_tree_data(x = data[[paste0("hospital_",i)]]$test_x,y =data[[paste0("hospital_",i)]]$test_y,model = hospitals[[i]]$model$model)
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



plot_progress = function(){
  
  lims = c(min(epoch_matrix,na.rm = T)-0.05,max(epoch_matrix,na.rm = T)+0.05)
  plot(epoch_matrix[,1],type="l",ylim=lims)
  for(j in 2:5){
    lines(epoch_matrix[,j],type="l")
  }
  for(j in 6:10){
    lines(epoch_matrix[,j],type="l",col="red")
  }
}



# local solo --------------------------------------------------------------


hospitals_result = rbind(hospitals_result,holder_results)
hospitals_result$type[nrow(hospitals_result)] = "local_only"
message("Solo done!")

# global ------------------------------------------------------------------

hospitals_result = rbind(hospitals_result,holder_results)
hospitals_result$type[nrow(hospitals_result)] = "global"

x = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]$x %>% as.data.frame()
}) %>% rbindlist() %>% as.matrix()
y = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]$y
}) %>% unlist()

x = rbind(x,data$reference)
y = c(y,data$reference_y %>% as.character() %>% as.numeric())




model = train_tree(x,y)


for(i in 1:h_size){
  tmp =  eval_tree_data(x =  data[[paste0("hospital_",i)]]$test_x,y = data[[paste0("hospital_",i)]]$test_y,model = model$model)
  hospitals_result[nrow(hospitals_result),i+1] = tmp$acc
  hospitals_result[nrow(hospitals_result),i+6] = tmp$auc
}

n_rep = 3

for(j in 2:n_rep){
  model = train_tree(x,y)
  for(i in 1:h_size){
    tmp =  eval_tree_data(x =  data[[paste0("hospital_",i)]]$test_x,y = data[[paste0("hospital_",i)]]$test_y,model = model$model)
    hospitals_result[nrow(hospitals_result),i+1] = hospitals_result[nrow(hospitals_result),i+1] + tmp$acc
    hospitals_result[nrow(hospitals_result),i+6] = hospitals_result[nrow(hospitals_result),i+6] + tmp$auc
  }
  
  
}

hospitals_result[nrow(hospitals_result),2:11] = hospitals_result[nrow(hospitals_result),2:11]/n_rep

message("Global done!")



# method ------------------------------------------------------------------

results = list(
  
)

input_p = 0.33
data = data_clone
data$reference = data$reference[sample(1:nrow(data$reference),size = input_p*nrow(data$reference)),]

hospitals = lapply(1:n_h, function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  tree = train_tree(tmp_data$x,tmp_data$y)
  list(x = tmp_data$x,
       y=tmp_data$y,
       model=tree)
})

epoch = ee
epoch_matrix = matrix(NA,epoch+1,10)

init = get_test_predictions()

epoch_matrix[1,] = c(init$acc,init$auc)


for(e in 1:epoch){
  
  lapply(1:h_size,allign_predictions)
  init = get_test_predictions()
  
  epoch_matrix[e+1,] = c(init$acc,init$auc)
  
  plot_progress()
  
}


results[[length(results)+1]] = epoch_matrix



input_p = 0.66
data = data_clone
data$reference = data$reference[sample(1:nrow(data$reference),size = input_p*nrow(data$reference)),]

hospitals = lapply(1:n_h, function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  tree = train_tree(tmp_data$x,tmp_data$y)
  list(x = tmp_data$x,
       y=tmp_data$y,
       model=tree)
})

epoch = ee
epoch_matrix = matrix(NA,epoch+1,10)

init = get_test_predictions()

epoch_matrix[1,] = c(init$acc,init$auc)


for(e in 1:epoch){
  
  lapply(1:h_size,allign_predictions)
  init = get_test_predictions()
  
  epoch_matrix[e+1,] = c(init$acc,init$auc)
  
  plot_progress()
  
}


results[[length(results)+1]] = epoch_matrix




input_p = 1
data = data_clone
data$reference = data$reference[sample(1:nrow(data$reference),size = input_p*nrow(data$reference)),]

hospitals = lapply(1:n_h, function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  tree = train_tree(tmp_data$x,tmp_data$y)
  list(x = tmp_data$x,
       y=tmp_data$y,
       model=tree)
})

epoch = ee
epoch_matrix = matrix(NA,epoch+1,10)

init = get_test_predictions()

epoch_matrix[1,] = c(init$acc,init$auc)


for(e in 1:epoch){
  
  lapply(1:h_size,allign_predictions)
  init = get_test_predictions()
  
  epoch_matrix[e+1,] = c(init$acc,init$auc)
  
  plot_progress()
  
}


results[[length(results)+1]] = epoch_matrix

p = c(0.33,0.66,1)

for(i in 1:length(results)){
  
  hospitals_result = rbind(hospitals_result,holder_results)
  hospitals_result$type[nrow(hospitals_result)] = paste0("reference_size_",p[i])
  tmp = results[[i]]
  
  if(i==1){
    hospitals_result[2,2:11] = tmp[1,]/length(results)
  }else{
    hospitals_result[2,2:11] = hospitals_result[2,2:11] + tmp[1,]/length(results)
  }
  
  tmp = apply(tmp,2,function(x)EMA_cpp(x,5)) %>% na.omit()
  ## we act like we know when to stop
  hospitals_result[nrow(hospitals_result),2:11] = tmp[apply(tmp,1,mean) %>% which.max(),]
}

results[[length(results)+1]] = hospitals_result %>% na.omit()

saveRDS(results,"data_50_n5.RDS")


