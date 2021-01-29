
# 50% ---------------------------------------------------------------------

library(magrittr)

set.seed(1231)

data = read.csv("data.csv")
data[,1:14] = scale(data[,1:14])
data[,15] = data[,15] - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:7500,1:14] %>% as.matrix()  ## reference
reference_y = data[1:7500,15] 


spl = split(7501:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,100,400) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:14] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,15] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:14] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,15] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


saveRDS("data_50_n5.RDS",object = hospitals)



# 30% ---------------------------------------------------------------------


data = read.csv("data.csv")
data[,1:14] = scale(data[,1:14])
data[,15] = data[,15] - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:5000,1:14] %>% as.matrix()  ## reference
reference_y = data[1:5000,15] 


spl = split(5001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,100,400) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:14] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,15] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:14] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,15] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


saveRDS("data_30_n5.RDS",object = hospitals)




# 18% ---------------------------------------------------------------------


data = read.csv("data.csv")
data[,1:14] = scale(data[,1:14])
data[,15] = data[,15] - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:2500,1:14] %>% as.matrix()  ## reference
reference_y = data[1:2500,15] 


spl = split(2501:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,100,400) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:14] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,15] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:14] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,15] %>% as.character() %>% as.numeric()
}

names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


saveRDS("data_18_n5.RDS",object = hospitals)



