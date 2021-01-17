source("test_models.R",local = T) ## load setup

# input_p = 10 ### here cone control the amount of reference data -- inputed in extrenal script
data = readRDS("../data.RDS")
data$reference = data$reference[sample(1:nrow(data$reference),size = input_p),]

# set.seed(12311)

h_size = 20

### initialize and learn init 20 epochs

hospital_models = lapply(1:h_size,function(x){

  h = create_hospital_model()

  # message(x," done")
  return(h)
})



hospital_models = lapply(1:h_size,function(x){
  tmp_data = data[[paste0("hospital_",x)]]
  test = data$test

  h = hospital_models[[x]]

  tmp_dec = train_model(30,h$decoder,x_data =tmp_data$x ,y_data = tmp_data$y)
  h$decoder = tmp_dec$new_model

  # message(x," done")
  return(h)
})


### now lets get started

### functions that embed their own data, then sends it to all other to decode it and update their embedding based od alligned embedding

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
  ## always derive ficed percentage of your data and refernce data!
  
  current_model$decoder = tmp_decoder$new_model
  rm(tmp)
  ## global assign
  hospital_models[[i]] <<- current_model
  NULL
  
}

get_mean_test_predictions = function(){
  acc = c()
  auc = c()
  for(i in 1:h_size){
    ev = evaluate_test_no_em(data_x = data$test$x,data_y = data$test$y,model_list = hospital_models[[i]])
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


epoch = 10

init = get_mean_test_predictions()

test_acc = c(mean(init$acc))
test_auc = c(mean(init$auc))


for(e in 1:epoch){
  
  lapply(1:h_size,allign_predictions)
  init = get_mean_test_predictions()
  
  test_acc = c(test_acc,mean(init$acc))
  test_auc = c(test_auc,mean(init$auc))
  
  plot(test_acc,type="l",ylim = c(0.5,0.65),main=paste0("Reference size: ",input_p))
  lines(test_auc,type="l",col="red")
  # message("Epoch: ",e)
}

### this variable outside scripts expects

output_stats = list(
  acc = test_acc,
  auc = test_auc
)


# saveRDS("RDS/reference_data_metric.RDS",object = list(acc = test_acc,auc=test_auc))
