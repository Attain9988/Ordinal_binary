
#calculates number of successes for a category given the number in the category
#and the conditional probability
succeses_fct <- function(multinomial, risk){
  prob_val<- rbinom(1, multinomial, risk)
  return(prob_val)
}

#calculates one simulation based on one probability and risk distribution
#returns a matrix of the number of total  events in each category, successes, 
#and failures
single_sim_fct <- function(probs_vals, risks_vals){
  #generates random samples for sampling num_data_per_sim data points
  category_totals <- rmultinom(1, num_data_per_sim, probs_vals)
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
