library(magrittr)
library(DMwR)

set.seed(1231)

d1 = read.csv("../../synthetic data/data1.csv")[,-1]
d2 = read.csv("../../synthetic data/data2.csv")[,-1]


h1 = d1[one <- sample(1:nrow(d1),6000),]
h2 = d2[two <- sample(1:nrow(d2),6000),]

reference = rbind(
  d1,d2
)
reference = reference[sample(setdiff(1:nrow(reference),c(one,two+nrow(d1))),size = 35000),]

hospitals = list()
hospitals[[1]] = list()
hospitals[[1]]$x = h1[1:300,1:10] %>% as.matrix()
hospitals[[1]]$y = h1[1:300,11] %>% as.character() %>% as.numeric()
hospitals[[2]] = list()
hospitals[[2]]$x = h2[1:300,1:10] %>% as.matrix()
hospitals[[2]]$y = h2[1:300,11] %>% as.character() %>% as.numeric()


names(hospitals) = paste0("hospital_",1:length(hospitals))
hospitals$reference = reference
# hospitals$reference_y = reference_y
hospitals$test = list()
hospitals$test$x = hospitals$reference[30001:35000,1:10] %>% as.matrix()
hospitals$test$y =  hospitals$reference[30001:35000,11] %>% as.character() %>% as.numeric()
hospitals$reference = hospitals$reference[,1:10] %>% as.matrix()

saveRDS("data.RDS",object = hospitals)



