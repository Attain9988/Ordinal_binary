#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions

load("./Data/labels.Rdata")
multinomials <- readRDS("./Data/multinomials.rds")
risks <- readRDS("./Data/risks.rds")


#risks and probability generation. 
#need to pull in the degrees, 

#make a single list so can apply. [[unique distribution id]][[risk/prob =1, 
#vector of stuff =2]]
#initialize list with enough room
all_risks_and_probs<- vector(mode = "list", length = 1000000)
#set counter for each entry to 1
counter_for_distribs <- 1
#for every kind of risk distribution
#risks in second column, probs in first, both in matrix in [[]][[1]]
#[[]][[2]] holds various other needed--r_i, r_f, type of risk distrib, 
##prob distrib, degree of multinomial
for (l_index in 1:length(risks)) {
  #for every initial rist
  for (o_index in 1:length(risks[[l_index]])){
    #for every final risk
    for (p_index in 1:length(risks[[l_index]][[o_index]])){
      #for degree of multinom
      for (n_index in 1:length(risks[[l_index]][[o_index]][[p_index]])) {
        risk_temp <- risks[[l_index]][[o_index]][[p_index]][[n_index]]
        #for multinom distribution
        for (m_index in 1:length(distribution_labels)) {
          #initialize positions for matrix and vector
          all_risks_and_probs[[counter_for_distribs]]<- 
            vector(mode= "list", length =2)
          
          #write in matrix for one column of risks, one of probs
          all_risks_and_probs[[counter_for_distribs]][[1]]<- 
            matrix(nrow = degrees[n_index], 
                                                       ncol = 2)
          all_risks_and_probs[[counter_for_distribs]][[1]][,2]<- 
            risks[[l_index]][[o_index]][[p_index]][[n_index]]
          all_risks_and_probs[[counter_for_distribs]][[1]][,1]<- 
            multinomials[[n_index]][[m_index]]
          #write in important characteristics
          #initial and final from the risks vector
          risk_temp_i <- min(risks[[l_index]][[o_index]][[p_index]][[n_index]])
          risk_temp_f <- max(risks[[l_index]][[o_index]][[p_index]][[n_index]])
          #pull the kind of risk distribution
          risk_temp_distrib <- risk_distrib_labels[l_index]
          #pull the probability distribution for the multinom
          prob_temp_distrib <- distribution_labels[m_index]
          #pull the degree
          degree_temp <- degrees[n_index]
          all_risks_and_probs[[counter_for_distribs]][[2]] <- 
            c(risk_temp_i, risk_temp_f, risk_temp_distrib, prob_temp_distrib, 
              degree_temp)
          #update counter
          counter_for_distribs<- counter_for_distribs +1
        }
      }
      
    }
    
  }
}
#remove nulls
all_risks_and_probs <- Filter(Negate(is.null), all_risks_and_probs)

#write to file
#initialize and open
sink("risks_and_probs.txt")
#output
print(all_risks_and_probs)
#close
sink()

#clear all_risks_and_probs??***
###
#now do the simulations



#code for simulation

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
  #generates random samples for sampling 1000 times
  category_totals <- rmultinom(1, 1000, probs_vals)
  #bind probs and risks to 
  probs_risks <- cbind(category_totals, risks_vals)
  successes_vect <- sapply(category_totals, succeses_fct, risk= risks_vals)
  #calculate failures from 
  failures_vect <- category_totals-successes_vect
  matrix_vals <- as.matrix(cbind( successes_vect, failures_vect))
  colnames(matrix_vals)<- c( "success", "failure")
  rownames(matrix_vals)<- seq(1, length(failures_vect))
  return(matrix_vals)
}

#parallelize
#find number of cores
num_cores <- detectCores()

#make a cluster of the cores and parallize
cl <- makeCluster(num_cores)
registerDoParallel(cl)
##editing from here
#simulate all and run 100 times
# outputs a list of simulations, simulations_list[[i]][[j]], where i is the
##index corresponding to the ith risk and prob, and j is the jth simulation
simulations_list <- foreach(z_index = 1:length(all_risks_and_probs)) %dopar% {
  #run the simulation for the zth risk prob and risk vectors
  simul_list<- vector(mode = "list", length = 100)
  for (simulation_index in 1:100) {
    simul_list[[simulation_index]] <- single_sim_fct(
      all_risks_and_probs[[z_index]][[1]][, 1], 
      all_risks_and_probs[[z_index]][[1]][, 2]) 
  }
  simul_list
}

stopCluster(cl)
#testing failure vs success

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
  expanded_table <- matrix(data = NA, nrow = 1000, ncol = 3)
  #column names
  colnames(expanded_table)<- c("Success=1", "category", "midrank")
  #calc mid and nominal vectors
  midrank_vector <- midrank_fct(simul_table)
  nominal_vector <- nominal_fct(simul_table)
  #pull degree/ number of rows so we can index
  degree_val <- nrow(simul_table)
  #set initial previous index
  prev_index <- 0
  for (row_index_val in 1:degree_val) {
    #total number of entries in category
    n_occurences<- sum(simul_table[row_index_val,])
    #successes in category
    n_succ <- simul_table[row_index_val, 1]
    #failures in category
    n_fail <- simul_table[row_index_val, 2]
    #
    succ_vect<- rep(1, n_succ)
    fail_vect <- rep(0, n_fail)
    #total vector
    succ_fail <- append(succ_vect, fail_vect)
    mid_vect <- rep(midrank_vector[row_index_val], n_occurences)
    nom_vect <- rep(nominal_vector[row_index_val], n_occurences)
    #calculate last index
    end_index <- n_occurences+prev_index
    #first index
    init_index <- prev_index+1
    #writing in success failure etc when there is a vector of length greater 
    ##than 0
    if(length(succ_fail)>0){
      expanded_table[init_index:end_index, 1]<- succ_fail
      #category
      expanded_table[init_index:end_index, 2] <- nom_vect
      #midrank
      expanded_table[init_index:end_index, 3] <- mid_vect
    }
    #iterate index to find ends
    prev_index <- end_index
  }
  return(expanded_table)
}
#####-------#####
#checked to here!!!!
#expand all so logistic work
cl <- makeCluster(num_cores)
registerDoParallel(cl)

#expand all simulations
expanded_simulations_list <- foreach(w_index = 1:length(simulations_list)) %dopar% {
  expand_list<- vector(mode = "list", length = 100)
  for (expand_index in 1:100) {
    #run the simulation for the wth simulation
    expand_list[[expand_index]] <- expand_fct(simulations_list[[w_index]]
                                              [[expand_index]])
  }
  expand_list
}

stopCluster(cl)


#build the models and predictions
#logistic per category
#for each category, odds of success vs not success given level
#does all of the function for the expanded, needs corresponding distrib
logistic_fct <- function(expanded, distrib_index){
  expanded<- as.data.frame(expanded)
  #models
  nom_mod <- glm(`Success=1`~ category, data = expanded)
  mid_mod <- glm(`Success=1` ~ midrank, data = expanded)
  
  
  #makes vectors of levels for confint/predic
  newdatnom <- data.frame( category = unique(expanded$category))
  newdatmidrank <- data.frame(midrank = unique(expanded$midrank))
  
  
  #predictions--i.e. P(success | level)
  predict_nom <- predict(nom_mod, newdata = newdatnom, type = "response")
  predict_mid <- predict(mid_mod, newdata = newdatmidrank, type = "response")
  
  #within score
  contained_nom <- rep(NA, length(unique(expanded$category)))
  contained_mid <- rep(NA, length(unique(expanded$category)))
  for (category_index in 1:length(unique(expanded$category))) {
    #pull values
    true<-all_risks_and_probs[[distrib_index]][[1]][category_index ,2]
    #number of entries in that category
    nval<- length(which(expanded$category == category_index))
    #score interval w/95% conf
    score_nom <- score_interval(predict_nom[category_index], nval, .05)
    score_mid <- score_interval(predict_mid[category_index], nval, .05)
    #is contained
    contained_nom[category_index] <- true >= score_nom[1] & 
      true <= score_nom[2]
    contained_mid[category_index] <- true >= score_mid[1] & 
      true <= score_mid[2]
  }
  #is nominal contained, 0 contained 1 not
  nom_cont <- ifelse(any(contained_nom == FALSE), 0,1)
  #mid
  mid_cont <- ifelse(any(contained_mid == FALSE), 0,1)
  return(c(nom_cont, mid_cont))
}

cl <- makeCluster(num_cores)
registerDoParallel(cl)

#expand all simulations
model_fails_list <- foreach(b_index = 1:length(simulations_list)) %dopar% {
  model_list<- vector(mode = "list", length = 100)
  for (model_index in 1:100) {
    #run the models
    model_list[[model_index]] <- logistic_fct(expanded_simulations_list[[b_index]]
                                              [[model_index]], b_index)
  }
  model_index
}

stopCluster(cl)

#I'm having the code abort here. Likely too large or some other issue. At this
##point I need to get the true false coverage and the distribution characteristics 
#into the same level as the rest of the expanded distributions so that I can run 
#a random forest and split. 

# I forgot to preserve the earlier code here when it was running
train_set <- sample(1:nrow(coverage_tab), .30*nrow(coverage_tab))
rf_mod <- randomForest(coverage_tab[,1]~., subset = train_set, mtry = 2, importance = TRUE)