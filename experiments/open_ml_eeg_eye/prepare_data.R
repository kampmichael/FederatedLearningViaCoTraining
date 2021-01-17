library(magrittr)

set.seed(1231)

data = read.csv("data.csv")
data[,1:14] = scale(data[,1:14])
data[,15] = data[,15] - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:10000,1:14] %>% as.matrix()
reference_y = data[1:10000,15] 
test = data[10001:13000,]


spl = split(13001:nrow(data),1:20)
## variable size of hospitals
for(i in 1:10){
  size = runif(1,0,33) %>% round()
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+10]] = c(spl[[i+10]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  hospitals[[i]]$x = data[spl[[i]],1:14] %>% as.matrix()
  hospitals[[i]]$y = data[spl[[i]],15] %>% as.character() %>% as.numeric()
}

names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y
hospitals$test = list()
hospitals$test$x = test[,1:14] %>% as.matrix()
hospitals$test$y = test[,15] %>% as.numeric()

saveRDS("data.RDS",object = hospitals)

## global models

