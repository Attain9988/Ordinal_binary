
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")
value_return <- function(value_ind, distrib_list){
  all_risks_and_probs[[distrib_list]][[2]][value_ind]
}

matrix_of_inits <- matrix(data= NA, nrow = length(all_risks_and_probs), 
                          ncol = 5)
for(dist_index in 1:length(all_risks_and_probs)){
  for(value_index in 1:5){
    matrix_of_inits[dist_index,value_index]<-
      all_risks_and_probs[[dist_index]][[2]][value_index] 
  }
}

#find all levels
level_finder <- function(column){
  unique(matrix_of_inits[,column])
}
uq_col<- sapply(1:5, level_finder)

