
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")
value_return <- function(value_ind, distrib_list){
  distrib_list[[2]][value_ind]
}

matrix_of_inits<- mapply(value_return, 1:5, all_risks_and_probs)
#puts into a matrix, col 1 is r_i, 2 r_f, 3 risk increase, 4 multinom dist, and 5 level
data_for_linear <- matrix(data = matrix_of_inits, nrow = length(all_risks_and_probs), ncol = 5, 
                          byrow = TRUE)
#find all levels
level_finder <- function(column){
  unique(data_for_linear[,column])
}
uq_col<- sapply(1:5, level_finder)
