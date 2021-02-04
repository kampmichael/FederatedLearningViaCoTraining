
# 50% ---------------------------------------------------------------------

library(magrittr)
library(projectXcppCode)
GPU_test()

set.seed(1231)

data = read.csv("poker.csv")
data = data[sample(1:nrow(data)),]
data = data[1:200000,]
table(data[,11])
y = (data[,11] >1 ) %>% as.numeric()
a = (data[,c(F,T)])/max(data)
data = cbind(a,y)
## eye data
# categorical, balanced
# 15K 


reference = data[1:100000,1:5] %>% as.matrix()  ## reference
reference_y = data[1:100000,6] 


spl = split(100001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,5000,10000) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:5] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,6] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:5] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,6] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


library(RJSONIO)
obj = toJSON(hospitals)
write(obj,"data_50_n5.json")

saveRDS("data_50_n5.RDS",object = hospitals)



# 30% ---------------------------------------------------------------------


data = read.csv("poker.csv")
data = data[sample(1:nrow(data)),]
data = data[1:200000,]
table(data[,11])
y = (data[,11] >1 ) %>% as.numeric()
a = (data[,c(F,T)])/max(data)
data = cbind(a,y)
## eye data
# categorical, balanced
# 15K 


reference = data[1:60000,1:5] %>% as.matrix()  ## reference
reference_y = data[1:60000,6] 


spl = split(60001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,5000,10000) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:5] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,6] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:5] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,6] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


library(RJSONIO)
obj = toJSON(hospitals)
write(obj,"data_30_n5.json")

saveRDS("data_30_n5.RDS",object = hospitals)





# 18% ---------------------------------------------------------------------


data = read.csv("poker.csv")
data = data[sample(1:nrow(data)),]
data = data[1:200000,]
table(data[,11])
y = (data[,11] >1 ) %>% as.numeric()
a = (data[,c(F,T)])/max(data)
data = cbind(a,y)
## eye data
# categorical, balanced
# 15K 


reference = data[1:33000,1:5] %>% as.matrix()  ## reference
reference_y = data[1:33000,6] 


spl = split(33001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,5000,10000) %>% round() ## variable size
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  tmp_ind = sample(spl[[i]],size = length(spl[[i]])*0.66)
  test_ind = setdiff(spl[[i]],tmp_ind)
  hospitals[[i]]$x = data[tmp_ind,1:5] %>% as.matrix()
  hospitals[[i]]$y = data[tmp_ind,6] %>% as.character() %>% as.numeric()
  hospitals[[i]]$test_x = data[test_ind,1:5] %>% as.matrix()
  hospitals[[i]]$test_y = data[test_ind,6] %>% as.character() %>% as.numeric()
}


names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y


library(RJSONIO)
obj = toJSON(hospitals)
write(obj,"data_18_n5.json")

saveRDS("data_18_n5.RDS",object = hospitals)


