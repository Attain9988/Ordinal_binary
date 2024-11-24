#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
lapply(c("./Functions/score_interval.R"), source)


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

#--- functions for expanding

#midrank fct
# takes in a table of simulated data, one item simulations_list[[]][[]]
midrank_fct <- function(simulation_table){
  #empty of length of degree (number of rows)
  midrank_vals <- rep(NA, nrow(simulation_table))
  #set initial final previous row
  n_f_prev_row <- 0
  #cl*alculate for each row
  for (row_index in 1:nrow(simulation_table)) {
    #find the midrank
    mid <- sum(simulation_table[row_index,])/2 + n_f_prev_row
    #output
    midrank_vals[row_index]<- mid
    #set new final
    n_f_prev_row<- sum(simulation_table[1:row_index,])
  }
  return(midrank_vals)
}

#nominal fct
nominal_fct <- function(simulation_table){
  #preallocate
  nominal_vals <- rep(NA, nrow(simulation_table))
  #calc for each row
  for (row_index in 1:nrow(simulation_table)) {
    nominal_vals[row_index] <- row_index
  }
  return(nominal_vals)
}


#expand data so logistic works
expand_fct <- function(simulation_table){
  #full table with one column for success=1, one column for category/nominal, 
  ##one for midrank
  simul_table<- as.matrix(simulation_table)
  expanded_table <- matrix(data = NA, nrow = num_data_per_sim, ncol = 4)
  
  
  #calc mid and nominal vectors
  midrank_vector <- midrank_fct(simul_table)
  nominal_vector <- nominal_fct(simul_table)
  #bind vectors to table
  #pull degree/ number of rows so we can index
  degree_val <- nrow(simul_table)
  #set initial previous index
  prev_index <- 0
  for (row_index_val in 1:degree_val) {
    #total number of entries in category
    n_occurences<- sum(simul_table[row_index_val,])
    if (n_occurences != 0)
    {
      #add in mid and nominal
      mid_vect <- rep(midrank_vector[row_index_val], n_occurences)
      nom_vect <- rep(nominal_vector[row_index_val], n_occurences)
      #calculate last index
      end_index <- n_occurences+prev_index
      #first index
      init_index <- prev_index+1
      expanded_table[init_index:end_index, 3] <- nom_vect
      #midrank
      expanded_table[init_index:end_index, 4] <- mid_vect
      #iterate index to find ends
      prev_index <- end_index
    }
  }
  #column names
  colnames(expanded_table)<- c( "Success=1", "categoric", "nominal", "midrank")
  #expanded_data
  expanded_data <- expand.table(simulation_table)
  #return(expanded_data)
  #input expanded data, with switch to accomodate desired columns
  expanded_table[,2] <- expanded_data[,1]
  expanded_table[,1] <- expanded_data[,2]
  #Jenky fix for unclear return of 1s and 2s, checked for correct
  expanded_table[,1] <- expanded_table[,1] %% 2
  #write in factor for category
  expanded_table[1:num_data_per_sim, 2] <- 
    as.factor(expanded_table[1:num_data_per_sim, 2])
  
  return(expanded_table)
}
#---
#functions for the modeling and success
#for a single expanded output contained in
# the expanded version of simulations
#build the models and predictions
#need the expanded simulation and the true risks
#logistic per category
#for each category, odds of success vs not success given level
#does all of the function for the expanded, needs corresponding distrib index 
#returns whether or not enough of the categories were captured by the score
logistic_fct <- function(expanded, true_risks){
  
  expanded<- as.data.frame(expanded)
  #force as factor
  expanded$categoric <- as.factor(expanded$categoric)
  #models
  cat_mod <- glm(`Success=1` ~ categoric, data = expanded)
  nom_mod <- glm(`Success=1`~ nominal, data = expanded)
  mid_mod <- glm(`Success=1` ~ midrank, data = expanded)
  
  
  
  #makes vectors of levels for confint/predic
  newdat <- data.frame( categoric = as.factor(unique(expanded$categoric)), 
                        nominal = unique(expanded$nominal),
                        midrank = unique(expanded$midrank))
  
  
  #predictions--i.e. P(success | level)
  predict_cat <- predict(cat_mod, newdata = newdat, type = "response")
  predict_nom <- predict(nom_mod, newdata = newdat, type = "response")
  predict_mid <- predict(mid_mod, newdata = newdat, type = "response")
  
  #within score
  contained_cat <- rep(NA, length(unique(expanded$categoric)))
  contained_nom <- rep(NA, length(unique(expanded$categoric)))
  contained_mid <- rep(NA, length(unique(expanded$categoric)))
  #fore each category (multinomial level)
  for (category_index in 1:length(unique(expanded$categoric))) {
    #pull values
    true_val<-true_risks[category_index]
    #number of entries in that category
    nval<- length(which(expanded$categoric == category_index))
    #score interval w/95% conf
    score_cat <- score_interval (predict_cat[category_index], nval, .05)
    score_nom <- score_interval(predict_nom[category_index], nval, .05)
    score_mid <- score_interval(predict_mid[category_index], nval, .05)
    #is contained, true if true value within bounds
    contained_cat[category_index] <- true_val >= score_cat[1] & 
      true_val <= score_cat[2]
    contained_nom[category_index] <- true_val >= score_nom[1] & 
      true_val <= score_nom[2]
    contained_mid[category_index] <- true_val >= score_mid[1] & 
      true_val <= score_mid[2]
  }
  #is nominal contained, 0 not contained 1 contained. 
  ##At least 60% of the categories must contain
  cat_cont <- ifelse(mean(contained_cat) < contained_thres, 0,1)
  #is nominal contained, 0 not contained 1 contained
  nom_cont <- ifelse(mean(contained_nom) < contained_thres, 0,1)
  #mid
  mid_cont <- ifelse(mean(contained_mid) < contained_thres, 0,1)
  #return list/vect with 
  return(c(cat_cont, nom_cont, mid_cont))
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
  return(contained_vect)
}

#now for a single distribution
distribution_results <- function(distrib_index){
  #pull just the needed part to generate data
  current_risk_prob <- all_risks_and_probs[[distrib_index]][[1]]
  #initialize a matrix for averaging
  distrib_contained_mat <- matrix(data = NA, nrow = num_sims, ncol = 3)
  for (sim_index in 1:num_sims) {
    #print(sim_index)
    #print(distrib_index)
    distrib_contained_mat[sim_index,]<- 
      single_distribution_simulation_fct(current_risk_prob)
  }
  #get coverage rates
  coverage_cat <- mean(distrib_contained_mat[,1])
  coverage_nom <- mean(distrib_contained_mat[,2])
  coverage_mid <- mean(distrib_contained_mat[,3])
  variance_cat <- var(distrib_contained_mat[,1])
  variance_nom <- var(distrib_contained_mat[,2])
  variance_mid <- var(distrib_contained_mat[,3])
  return(c(coverage_cat,coverage_nom,coverage_mid, variance_cat, variance_nom,
           variance_mid))
}
num_cores <- detectCores()

#make a cluster of the cores and parallize
cl <- makeCluster(num_cores)
registerDoParallel(cl)
coverage <- foreach(b_index = 1:50, 
                    .combine = 'rbind', .packages = "epitools") %dopar% {
  
  #run distributions in parallel
  distribution_results(b_index)
}
stopCluster(cl)
saveRDS(coverage, file = "./Data/coverage.rds")
