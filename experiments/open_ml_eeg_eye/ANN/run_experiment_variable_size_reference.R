library(magrittr)
results = list()

set.seed(1231)

# #average size of hospital data is 
# lapply(tmp[1:20],function(x)nrow(x$x)) %>% unlist %>% mean


candidates = c(
  10,25,50,100,250,500,
  750,1000,2500,5000,10000
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
saveRDS(results,file = "RDS/ANN_method.RDS")




