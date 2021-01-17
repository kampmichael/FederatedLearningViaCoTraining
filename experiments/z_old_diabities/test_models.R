## personal library configured for my GPU
library(projectXcppCode)
GPU_test()
## same code should work on CPU with just normal tensorflow/keras
## library(keras)
## library(tensorflow)


create_hospital_model = function(){
  
  in_shape = 8
  units = 32
  en_dim = 8
  out_dim = 1
  
  # model_ae = keras_model_sequential() %>% 
  #   layer_dense(input_shape = in_shape,units = units,activation = "tanh") %>% 
  #   layer_dense(units = units,activation = "tanh") %>% 
  #   layer_dense(units = en_dim,activation = "linear",name = "hidden") %>% 
  #   layer_dense(units = units,activation = "tanh") %>% 
  #   layer_dense(units = units,activation = "tanh") %>% 
  #   layer_dense(units = in_shape)
  # 
  #  # model_ae
  # 
  # model_ae %>% keras::compile(
  #   metric = "mse",
  #   loss = "mse",
  #   optimizer = "adam"
  # )

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
    # ae = model_ae,
    decoder = decoder
  ))
  
}

# get_embedding = function(data,model_){
#   model_hidden_ = keras_model(inputs = model_$input,outputs = model_$get_layer("hidden")$output)
#   model_hidden_(data) %>% as.matrix()
# }

train_model = function(n_epoch,model,x_data,y_data){
  hist = model %>% keras::fit(
    epoch = n_epoch,
    x = x_data,
    y = y_data,
    verbose = F,
    batch_size = 32
  )
  return(list(
    new_model = model,
    loss = tail(hist$metrics[[1]],1),
    metric = tail(hist$metrics[[2]],1)
  ))
}

# hospital_1 = create_hospital_model()
# hospital_1$ae
# hospital_1$decoder
# 
# new = train_model(n_epoch = 1,model = hospital_1$decoder,x_data = matrix(rnorm(100),10,4),y_data =  rep(1,10))
# new

# evaluate_test_em = function(data_x,data_y,model_list){
#   
#   embedd = get_embedding(as.matrix(data_x),model_ = model_list$ae) %>% as.matrix()
#   preds = model_list$decoder(embedd) %>% as.numeric()
#   # preds = model_list$decoder(data_x) %>% as.numeric()
# 
#   acc = sum(round(preds)==data_y) / nrow(data_x)
#   auc = cvAUC::AUC(predictions = preds,labels = factor(data_y,levels = c(0,1)))
#   return(list(
#     preds = preds,
#     acc= acc,
#     auc = auc
#   ))
# }

evaluate_test_no_em = function(data_x,data_y,model_list){
  
  # embedd = get_embedding(as.matrix(data_x),model_ = model_list$ae) %>% as.matrix()
  # preds = model_list$decoder(embedd) %>% as.numeric()
  preds = model_list$decoder(data_x) %>% as.numeric()
  
  acc = sum(round(preds)==data_y) / nrow(data_x)
  auc = cvAUC::AUC(predictions = preds,labels = factor(data_y,levels = c(0,1)))
  return(list(
    preds = preds,
    acc= acc,
    auc = auc
  ))
}


# train_test_hospital_no_reference_em = function(i,data){
#   tmp_data = data[[paste0("hospital_",i)]]
#   test = data$test
#   
#   h = create_hospital_model()
#   tmp_ae = train_model(500,h$ae,x_data = tmp_data$x,y_data = tmp_data$x)
#   h$ae = tmp_ae$new_model
#   tmp_enc = get_embedding(tmp_data$x,model_ = h$ae)
#   tmp_dec = train_model(50,h$decoder,x_data = tmp_enc,y_data =tmp_data$y)
#   h$decoder = tmp_dec$new_model
#   eval_test = evaluate_test_em(data_x = test$x,data_y =test$y,model_list = h)
#   return(
#     list(
#       models = h,
#       test_preds = eval_test$preds,
#       test_acc = eval_test$acc,
#       test_auc = eval_test$auc
#     )
#   )
# }


train_test_hospital_no_reference_no_em = function(i,data){
  tmp_data = data[[paste0("hospital_",i)]]
  test = data$test
  
  h = create_hospital_model()
  tmp_dec = train_model(30,h$decoder,x_data = tmp_data$x,y_data =tmp_data$y)
  h$decoder = tmp_dec$new_model
  eval_test = evaluate_test_no_em(data_x = test$x,data_y =test$y,model_list = h)
  return(
    list(
      models = h,
      test_preds = eval_test$preds,
      test_acc = eval_test$acc,
      test_auc = eval_test$auc
    )
  )
}

