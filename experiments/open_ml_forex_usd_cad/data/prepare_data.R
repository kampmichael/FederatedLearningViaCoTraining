
# 50% ---------------------------------------------------------------------

library(magrittr)

set.seed(1231)

data = read.csv("data.csv")
data = data[,-1]
data[,1:10] = scale(data[,1:10])
data[,11] = as.factor(data[,11]) %>% as.numeric() - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:21000,1:10] %>% as.matrix()  ## reference
reference_y = data[1:21000,11] 


spl = split(21001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,1000,4000) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:10] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,11] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:10] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,11] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


saveRDS("data_50_n5.RDS",object = hospitals)



# 30% ---------------------------------------------------------------------


data = read.csv("data.csv")
data = data[,-1]
data[,1:10] = scale(data[,1:10])
data[,11] = as.factor(data[,11]) %>% as.numeric() - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:13000,1:10] %>% as.matrix()  ## reference
reference_y = data[1:13000,11] 


spl = split(13001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,1000,4000) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:10] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,11] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:10] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,11] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


saveRDS("data_30_n5.RDS",object = hospitals)




# 18% ---------------------------------------------------------------------


data = read.csv("data.csv")
data = data[,-1]
data[,1:10] = scale(data[,1:10])
data[,11] = as.factor(data[,11]) %>% as.numeric() - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:7000,1:10] %>% as.matrix()  ## reference
reference_y = data[1:7000,11] 


spl = split(7001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,1000,4000) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:10] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,11] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:10] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,11] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


saveRDS("data_18_n5.RDS",object = hospitals)

