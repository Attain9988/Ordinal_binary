#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
lapply(c("./Functions/model_fcts.R", "./Functions/simulations_fcts.R",
         "./Functions/simul_expand_fcts.R"), source)


load("./Data/labels.Rdata")

all_risks_and_probs <- readRDS("./Data/risk_prob.rds")

#calculates number of successes for a category given the number in the category
#and the conditional probability, needed for single simulation fct
succeses_fct <- function(multinomial, risk){
  prob_val<- rbinom(1, multinomial, risk)
  return(prob_val)
}


#single sim fct
#calculates one simulation based on one probability and risk distribution
#returns a matrix of the number of total  events in each category, successes, 
#and failures uses while to avoid empty categories
single_sim_fct <- function(probs_vals, risks_vals){
  #generates fully empty categories for the inital while
  category_totals <- rep(0, length(probs_vals))
  #uses while to ensure no empty categories by regenerating
  while (any(category_totals == 0)) {
    #generates random samples for sampling num_data_per_sim data points
    category_totals <- rmultinom(1, num_data_per_sim, probs_vals)
  }
  
  #bind probs and risks to 
  probs_risks <- cbind(category_totals, risks_vals)
  successes_vect <- sapply(category_totals, succeses_fct, risk= risks_vals)
  #calculate failures from 
  failures_vect <- category_totals-successes_vect
  matrix_vals <- as.matrix(cbind( successes_vect, failures_vect))
  colnames(matrix_vals)<- c( 1, 0)
  rownames(matrix_vals)<- seq(1, length(failures_vect))
  return(matrix_vals)
}



#-----
#for a single distribution does a simulation, expansion, and coverage, 
#returns the output #takes in only distribution index and risk and probs portion
single_distribution_simulation_fct <- function(current_risk_prob){
  #calculate the simulation and output
  simulation_matrix <- 
    single_sim_fct(current_risk_prob[,1], current_risk_prob[,2])
  #expand
  expanded <- expand_fct(simulation_matrix)
  #grab the true values of risk
  true_values <- current_risk_prob[,2]
  #find the success rates
  contained_vect<- logistic_fct(expanded, true_values)
  return(contained_vector)
}

#now for a single distribution
distribution_results <- function(distrib_index){
  #pull just the needed part to generate data
  current_risk_prob <- risks_probs[[distrib_index]][[1]]
  #initialize a matrix for averaging
  distrib_contained_mat <- matrix(data = NA, nrow = num_sims)
  for (sim_index in 1:num_sims) {
    distrib_contained_mat[sim_index,]<- 
      single_distribution_simulation_fct(current_risk_prob)
  }
  #get coverage rates
  coverage_cat <- mean(distrib_contained_mat[,1])
  coverage_nom <- mean(distrib_contained_mat[,2])
  coverage_mid <- mean(distrib_contained_mat[,3])
  return(c(coverage_cat,coverage_nom,coverage_mid))
}

cl <- makeCluster(num_cores)
registerDoParallel(cl)


coverage <- foreach(b_index = 1:length(all_risks_and_probs), 
                    .combine = 'cbind', .packages = "epitools") %dopar% {
  
  #run distributions in parallel
  distribution_results(b_index)
}
stopCluster(cl)
saveRDS(coverage, file = "./Data/coverage.rds")

