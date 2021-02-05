library(magrittr)

set.seed(1231)

data = read.csv("data.csv")
data[,1:14] = scale(data[,1:14])
data[,15] = data[,15] - 1
data = data[sample(1:nrow(data)),]
## eye data
# categorical, balanced
# 15K 


reference = data[1:11500,1:14] %>% as.matrix()
reference_y = data[1:11500,15] 
test = data[11500:14000,]


spl = split(14001:nrow(data),1:5)
## variable size of hospitals
for(i in 1:2){
  size = runif(1,0,50) %>% round()
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+2]] = c(spl[[i+2]],who) %>% sample ## add to enemy
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

saveRDS("data_80.RDS",object = hospitals)

library(RJSONIO)
obj = toJSON(hospitals)
write(obj,"data_80_n5.json")

## global models