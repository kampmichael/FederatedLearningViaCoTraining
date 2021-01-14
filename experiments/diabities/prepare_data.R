library(magrittr)
library(DMwR)

set.seed(1231)

data<- read.csv("diabetes.csv") 

data[,1:8] = scale(data[,1:8])

diab = data

test = data[s<-sample(350,x = 1:nrow(data)),]
diab = data[-s,]

diab$Outcome = as.factor(diab$Outcome)
data<- SMOTE(Outcome~., diab, perc.over=1000, perc.under=200, k=5, learner=NULL)
# table(data$Outcome)
data = data[sample(1:nrow(data)),]

reference = data[1:3500,1:8] %>% as.matrix()
reference_y = data[1:3500,9] 
spl = split(3501:nrow(data),1:20)
## variable size of hospitals
for(i in 1:10){
  size = runif(1,0,15) %>% round()
  who = sample(spl[[i]],size = size)
  spl[[i]] = setdiff(spl[[i]],who)
  spl[[i+10]] = c(spl[[i+10]],who) %>% sample ## add to enemy
}
hospitals = list()
for(i in 1:length(spl)){
  hospitals[[i]] = list()
  hospitals[[i]]$x = data[spl[[i]],1:8] %>% as.matrix()
  hospitals[[i]]$y = data[spl[[i]],9] %>% as.character() %>% as.numeric()
}

names(hospitals) = paste0("hospital_",1:length(spl))
hospitals$reference = reference
hospitals$reference_y = reference_y
hospitals$test = list()
hospitals$test$x = test[,1:8] %>% as.matrix()
hospitals$test$y = test[,9] %>% as.numeric()

saveRDS("data.RDS",object = hospitals)





# pure --------------------------------------------------------------------


