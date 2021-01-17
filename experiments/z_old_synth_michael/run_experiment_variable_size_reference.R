library(magrittr)
results = list()
size = nrow((tmp<-readRDS("data.RDS"))$reference) 
print(size)

set.seed(1231)

# #average size of hospital data is 
# lapply(tmp[1:20],function(x)nrow(x$x)) %>% unlist %>% mean


candidates = c(
  100,500, 1000,2500,5000,15000,30000
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
saveRDS(results,file = "RDS/ANN_variable_reference_size_all_reference.RDS")




