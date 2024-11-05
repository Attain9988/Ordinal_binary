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


