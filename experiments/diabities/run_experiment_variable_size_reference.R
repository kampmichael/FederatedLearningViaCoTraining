library(magrittr)
results = list()
size = nrow((tmp<-readRDS("data.RDS"))$reference) 
print(size)

set.seed(1231)

# #average size of hospital data is 
# lapply(tmp[1:20],function(x)nrow(x$x)) %>% unlist %>% mean


candidates = c(
  2,10,20,35,50, 75, 100,125,150,175,
  200, 250, 300,  400,  600,  1000,
  1500, 2000,
  3000,  3500
)


for(i in candidates){
  
  rm(list = setdiff(ls(),c("results","candidates","i")))
  input_p = i
  message("Trying : ",input_p) 
  source("reference_data_hospitals_no_embedding.R") ## input P sent to the script --> ugly code
  # output_stats
  
  results[[length(results)+1]] = output_stats
  
}

names(results) = paste0("ref_size_",candidates)
saveRDS(results,file = "RDS/ANN_variable_reference_size_fixed_reference.RDS")




