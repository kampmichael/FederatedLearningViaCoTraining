data = readRDS("../data/data_30_n5.RDS")
data_clone = data
h_size = 5
set.seed(1231)
init_epoch = 100
ee = 100

hospitals_result = data.frame(
  type=NA,h1_acc=NA,h2_acc=NA,h3_acc=NA,h4_acc=NA,h5_acc=NA,h1_auc=NA,h2_auc=NA,h3_auc=NA,h4_auc=NA,h5_auc=NA
)

holder_results = rep(NA,11)

## personal library configured for my GPU
library(projectXcppCode)
GPU_test() ## first one will set up GPU and returned error
GPU_test() ## actual check

## same code should work on CPU with just normal tensorflow/keras

## library(keras)
## library(tensorflow)



# functions ---------------------------------------------------------------



create_hospital_model = function(){
  
  in_shape = 54
  units = 128
  en_dim = 54
  out_dim = 1
  
  decoder =  keras_model_sequential() %>%
    layer_dense(input_shape = en_dim,units = units,activation = "tanh",dtype="float64") %>% 
    layer_dense(units = units,activation = "tanh",dtype="float64") %>% 
    layer_dense(units = out_dim,activation = "sigmoid",dtype="float64")
  
  decoder %>% keras::compile(
    metric="accuracy",
    loss = "binary_crossentropy",
    optimizer = "adam"
  )
  
  return(list(
    decoder = decoder
  ))
  
}

train_model = function(n_epoch,model,x_data,y_data){
  hist = model %>% keras::fit(
    epoch = n_epoch,
    x = x_data,
    y = y_data,
    verbose = F,
    # validation_split=0.2,
    batch_size = 1024
  )
  return(list(
    new_model = model,
    loss = tail(hist$metrics[[1]],1),
    metric = tail(hist$metrics[[2]],1)
  ))
}

evaluate_test_no_em = function(data_x,data_y,model_list){
  
  preds = model_list$decoder(data_x) %>% as.numeric()
  
  acc = sum(round(preds)==data_y) / nrow(data_x)
  auc = cvAUC::AUC(predictions = preds,labels = factor(data_y,levels = c(0,1)))
  return(list(
    acc= acc,
    auc = auc
  ))
}



train_test_hospital_no_reference_no_em = function(i,data){
  tmp_data = data[[paste0("hospital_",i)]]
  
  h = create_hospital_model()
  tmp_dec = train_model(init_epoch,h$decoder,x_data = tmp_data$x,y_data =tmp_data$y)
  h$decoder = tmp_dec$new_model
  eval_test = evaluate_test_no_em(data_x = tmp_data$test_x,data_y =tmp_data$test_y,model_list = h)
  return(
    list(
      test_acc = eval_test$acc,
      test_auc = eval_test$auc
    )
  )
}



allign_predictions = function(i){
  
  current_model = hospital_models[[i]]
  current_data = data[[paste0("hospital_",i)]]
  
  ## working data
  ## hospitals never share its own data
  tmp = data$reference
  
  ## create temp target Y
  others = setdiff(1:h_size,i)
  other_embeddings = lapply(others,function(x){
    tmp_model = hospital_models[[x]]
    tmp_model$decoder(tmp) %>% as.matrix()
  })
  holder = other_embeddings[[1]]
  for(j in 2:length(other_embeddings)){
    holder = holder + other_embeddings[[j]]
  }
  holder = holder/length(other_embeddings)
  
  tmp_decoder = train_model(n_epoch = 1,model = current_model$decoder,x_data = rbind(current_data$x,tmp),y_data = c(current_data$y,round(holder)))
  # always derive with all the reference data!
  
  # ind_ref = sample(1:nrow(tmp),min(nrow(current_data$x)*2.25,nrow(tmp)))
  # tmp_decoder = train_model(n_epoch = 1,model = current_model$decoder,x_data = rbind(current_data$x,tmp),y_data = c(current_data$y,round(holder)))
  ## always derive fixed percentage of your data and reference data!
  
  current_model$decoder = tmp_decoder$new_model
  rm(tmp)
  ## global assign
  hospital_models[[i]] <<- current_model
  NULL
  
}


get_test_predictions = function(){
  acc = c()
  auc = c()
  for(i in 1:h_size){
    ev <- evaluate_test_no_em(data_x = data[[paste0("hospital_",i)]]$test_x,data_y = data[[paste0("hospital_",i)]]$test_y,model_list = hospital_models[[i]])
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
# 
# hospitals = lapply(1:h_size,function(x)train_test_hospital_no_reference_no_em(i = x,data = data)) ## spamming float warnings
# hospitals_result[nrow(hospitals_result),2:6] = lapply(hospitals,function(x)x$test_acc) %>% unlist()
# hospitals_result[nrow(hospitals_result),7:11] = lapply(hospitals,function(x)x$test_auc) %>% unlist()
# 
# n_rep = 3
# 
# for(j in 2:n_rep){
#   hospitals = lapply(1:h_size,function(x)train_test_hospital_no_reference_no_em(i = x,data = data)) ## spamming float warnings
#   hospitals_result[nrow(hospitals_result),2:6] = hospitals_result[nrow(hospitals_result),2:6] + lapply(hospitals,function(x)x$test_acc) %>% unlist()
#   hospitals_result[nrow(hospitals_result),7:11] = hospitals_result[nrow(hospitals_result),7:11] + lapply(hospitals,function(x)x$test_auc) %>% unlist()
# }
# 
# 
# hospitals_result[nrow(hospitals_result),2:11] = hospitals_result[nrow(hospitals_result),2:11]/n_rep
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




h_glob = create_hospital_model()
tmp_dec = train_model(init_epoch,h_glob$decoder,x_data = x ,y_data = y)
h_glob$decoder = tmp_dec$new_model


for(i in 1:h_size){
  tmp =  evaluate_test_no_em(data_x = data[[paste0("hospital_",i)]]$test_x,data_y = data[[paste0("hospital_",i)]]$test_y,h_glob)
  hospitals_result[nrow(hospitals_result),i+1] = tmp$acc
  hospitals_result[nrow(hospitals_result),i+6] = tmp$auc
}

# n_rep = 3
# 
# for(j in 2:n_rep){
#   h_glob = create_hospital_model()
#   tmp_dec = train_model(init_epoch,h_glob$decoder,x_data = x ,y_data = y)
#   h_glob$decoder = tmp_dec$new_model
#   
#   
#   for(i in 1:h_size){
#     tmp =  evaluate_test_no_em(data_x = data[[paste0("hospital_",i)]]$test_x,data_y = data[[paste0("hospital_",i)]]$test_y,h_glob)
#     hospitals_result[nrow(hospitals_result),i+1] = hospitals_result[nrow(hospitals_result),i+1] + tmp$acc
#     hospitals_result[nrow(hospitals_result),i+6] = hospitals_result[nrow(hospitals_result),i+6] + tmp$auc
#   }
#   
#   
# }
# 
# hospitals_result[nrow(hospitals_result),2:11] = hospitals_result[nrow(hospitals_result),2:11]/n_rep

message("Global done!")



# method ------------------------------------------------------------------

results = list(
  
)

input_p = 0.33
data = data_clone
data$reference = data$reference[sample(1:nrow(data$reference),size = input_p*nrow(data$reference)),]

hospital_models = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  test = data$test
  h = create_hospital_model()
  tmp_dec = train_model(init_epoch,h$decoder,x_data =tmp_data$x ,y_data = tmp_data$y)
  h$decoder = tmp_dec$new_model
  return(h)
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

hospital_models = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  test = data$test
  h = create_hospital_model()
  tmp_dec = train_model(init_epoch,h$decoder,x_data =tmp_data$x ,y_data = tmp_data$y)
  h$decoder = tmp_dec$new_model
  return(h)
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

hospital_models = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  test = data$test
  h = create_hospital_model()
  tmp_dec = train_model(init_epoch,h$decoder,x_data =tmp_data$x ,y_data = tmp_data$y)
  h$decoder = tmp_dec$new_model
  return(h)
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

saveRDS(results,"data_30_n5.RDS")


