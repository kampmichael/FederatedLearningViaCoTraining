library(magrittr)

set.seed(1231)

data = read.csv("data.csv")
data[,2:11] = scale(data[,2:11])
data[,12] = data[,12] %>% as.logical() %>% as.numeric() 
## ignore the time for now
data = data[sample(1:nrow(data)),2:12]
## eye data
# categorical, balanced
# 15K 


reference = data[1:30000,1:10] %>% as.matrix()
reference_y = data[1:30000,11] 
test = data[30001:37500,]


spl = split(37501:nrow(data),1:20)
## variable size of hospitals
for(i in 1:10){
  size = runif(1,0,150) %>% round()
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+10]] = c(spl[[i+10]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  hospitals[[i]]$x = data[spl[[i]],1:10] %>% as.matrix()
  hospitals[[i]]$y = data[spl[[i]],11] %>% as.character() %>% as.numeric()
}

names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y
hospitals$test = list()
hospitals$test$x = test[,1:10] %>% as.matrix()
hospitals$test$y = test[,11] %>% as.numeric()

saveRDS("data.RDS",object = hospitals)

## global models

