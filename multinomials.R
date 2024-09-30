#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#Score interval
score_interval <- function(p, n, confidence){
  z_ahalf <- qnorm(confidence/2) #for desired sig level
  zterm <- z_ahalf^2/n
  firstsqrtterm <- (2*p + zterm)^2 #first term under square root
  secondsqrtterm <- 4*p^2*(1+zterm) #second
  denom <- 2*(1+zterm) #denominator
  sumpart <- 2*p+zterm # initial part that is summed
  sqrtpart <- sqrt(firstsqrtterm-secondsqrtterm)
  
  
  limit2 <- (sumpart+sqrtpart)/denom
  limit1 <- (sumpart-sqrtpart)/denom
  
  limits <- cbind(limit1, limit2)
  return(limits)
}



#define how many degrees of each
degrees <- c(2, 3, 4, 5, 6, 7)
#make an empty list for multiomials
multinomials <- vector(mode = "list", length = length(degrees))

#distribution labels
distribution_labels <- c("normal", "bimodal", "left_skew", "right_skew", 
                         "uniform")
risk_distrib_labels <- c("addition", "multiplication", "power")

#start functions
#these are the functions that generate discritized versions of set distributions

#normal fct
normal_fct<- function(degree){
  set.seed(1002)
  # Define parameters for the distribution
  mean <- 0
  std_dev <- 1
  num_samples <- 100000  # Increase the number of samples for better accuracy
  
  # Generate samples from the normal distribution
  samples <- rnorm(num_samples, mean = mean, sd = std_dev)
  
  # Discretize the samples into four equal-sized intervals
  breaks <- seq(min(samples), max(samples), length.out = degree+1)
  
  # using hist to calculate number in each
  hist_counts <- hist(samples, breaks = breaks, plot = FALSE)$counts
  
  # Normalize frequencies to get probabilities
  probabilities <- hist_counts / sum(hist_counts)
  
  return(probabilities)
}

#bimodal with peaks at extremes
bimodal_fct<- function(degree){
  set.seed(1002)
  # Define parameters for the distributions
  mean_low <- 0
  mean_high <- 1
  std_dev <- 0.1  # Standard deviation for both distributions
  num_samples <- 100000  # Increase the number of samples for better accuracy
  
  # Generate samples from the two normal distributions
  samples_low <- rnorm(num_samples, mean = mean_low, sd = std_dev)
  samples_high <- rnorm(num_samples, mean = mean_high, sd = std_dev)
  
  # Combine the samples into a single vector
  samples <- c(samples_low, samples_high)
  
  # Discretize the samples into four equal-sized intervals
  breaks <- seq(min(samples), max(samples), length.out = degree+1)
  
  # using hist to calculate number in each
  hist_counts <- hist(samples, breaks = breaks, plot = FALSE)$counts
  
  # Normalize frequencies to get probabilities
  probabilities <- hist_counts / sum(hist_counts)
  
  return(probabilities)
}

#left skew fct
left_skew_fct <- function(degree){
  #set seed
  set.seed(1005)
  #shape params, alpha less 1 for left skew
  alpha <- 0.5  
  beta <- 2   
  
  # Generate random samples from a beta distribution
  n <- 1000  # Number of samples
  samples <- rbeta(n, alpha, beta)
  
  # Discretize the samples into equal-sized intervals
  breaks <- seq(min(samples), max(samples), length.out = degree+1)
  
  # using hist to calculate number in each
  hist_counts <- hist(samples, breaks = breaks, plot = FALSE)$counts
  
  # Normalize frequencies to get probabilities
  probabilities <- hist_counts / sum(hist_counts)
  return(probabilities)
}

#right skew fct
right_skew_fct <- function(degree){
  #set seed for always same
  set.seed(1003)
  #shape params. beta less than 1 for r skew
  alpha <- 2  
  beta <- 0.5
  
  # Generate random samples from a beta distribution
  n <- 1000
  samples <- rbeta(n, alpha, beta)
  # Discretize the samples into equal-sized intervals
  breaks <- seq(min(samples), max(samples), length.out = degree+1)
  
  # using hist to calculate number in each
  hist_counts <- hist(samples, breaks = breaks, plot = FALSE)$counts
  
  # Normalize frequencies to get probabilities
  probabilities <- hist_counts / sum(hist_counts)
  return(probabilities)
}

#uniform fct
uniform_fct <- function(degree){
  #prob for each
  prob  <- 1/degree
  
  #returns vector thereof
  probabilities <- rep(prob, degree)
  return(probabilities)
}

#distribution indexing etc-- take in which distribution and degree, run the 
##appropriate function
distribution_fct <- function(distribution_id, degree){
  if(distribution_id ==1){
    return(normal_fct(degree))
  }else if(distribution_id ==2){
    return(bimodal_fct(degree))
  }else if(distribution_id == 3){
    return(left_skew_fct(degree))
  }else if(distribution_id == 4){
    return(right_skew_fct(degree))
  }else if(distribution_id == 5){
    return(uniform_fct(degree))
  }
}
#create the multinomials indexed by dgree then distrib
for (degree_index in 1:length(degrees)) {
  #initialize a list for each distribution
  multinomials[[degree_index]] <- vector(mode = "list", 
                                         length = length(distribution_labels))
  #fill in current degree
  curr_degree <- degrees[degree_index]
  #for each kind of distribution, run and fill in
  for (distribution_index in 1:length(distribution_labels)) {
    multinomials[[degree_index]][[distribution_index]] <- 
      distribution_fct(distribution_index, curr_degree)
  }
}


##
##graphing distributions
titles_multinom <- c("Degree 2", "Degree 3", "Degree 4", "Degree 5", "Degree 6", "Degree 7")
color_vector <- c("#ccfdff", "#99F8FF","#66F0FF", "#33E4FF", "#00AACC", 
                  "#007A99")
maintitles <- c("Constant added", "Constant multiplied", "Raised to constant")

#multinomials graphs
for(m_index in 1:length(titles_multinom)){
  n_graphs <- length(multinomials[[m_index]])
  par(mfrow = c(n_graphs/2, n_graphs/2))
  for(g_index in 1:n_graphs){
    if(g_index == 1){
      barplot(multinomials[[m_index]][[g_index]], main = titles_multinom[m_index], 
              ylab = "Probabilities", col = color_vector[m_index], 
              border = "black", space = c(0,0), xlab = "Ranked groups")
    }else{
      barplot(multinomials[[m_index]][[g_index]], main = NULL, 
              ylab = "Probabilities", col = color_vector[m_index], 
              border = "black", space = c(0,0), xlab = "Ranked groups")
    }
  }
}
#warnings here??
##risks

#initial risk and final risk array
risk_i <- seq(from= 0, to = 1, length.out = 20)
risk_f <- seq(from = 0, to = 1, length.out = 20)
risk_i <- risk_i[-c(1,20)]
risk_f <- risk_f[-c(1,2)]

#risk increases by constant at each level
risk_add_constant <- function(n, r_f, r_i){
  constant_out <- (r_f-r_i)/(n-1)
  return(constant_out)
} 

risk_add_next <- function(r_prev, constant_in){
  r_new <- r_prev +constant_in
  return(r_new)
}

#risk multiplied by a constant at each level
risk_mult_constant <- function(n, r_f, r_i){
  constant_out <- (r_f/r_i)^(1/(n-1))
  return(constant_out)
}

risk_mult_next <- function(r_prev, constant_in){
  r_new <- r_prev*constant_in
  return(r_new)
}

#risk raised to a constant at each level
risk_power_constant <- function(n, r_f, r_i){
  constant_out <- ((log(r_f))/(log(r_i)))^(1/(n-1))
  return(constant_out)
}  

risk_power_next <- function(r_prev, constant_in){
  r_new <- r_prev^constant_in
  return(r_new)
}

#all distributions combined into single fcts
#calculates the risk constant for a given kind of risk distribution, number of 
#multinomial categories (degree), final risk, and initial risk. 
risk_constant <- function(distribution_index, n, r_f, r_i){
  if(distribution_index == 1){
    risk_output <- risk_add_constant(n, r_f, r_i)
  }
  if(distribution_index == 2){
    risk_output <- risk_mult_constant(n, r_f, r_i)
  }
  if(distribution_index == 3){
    risk_output <- risk_power_constant(n, r_f, r_i)
  }
  return(risk_output)
}
#calculates the next risk value based on the previous risk value, the kind of 
#risk distribution, and the risk constant
risk_next <- function(distribution_index, r_prev, constant_in){
  if(distribution_index == 1){
    risk_output <- risk_add_next(r_prev, constant_in)
  }
  if(distribution_index == 2){
    risk_output <- risk_mult_next(r_prev, constant_in)
  }
  if(distribution_index == 3){
    risk_output <- risk_power_next(r_prev, constant_in)
  }
  return(risk_output)
}

#calculates all the risks with the kind of distribution, the number of groups
#the final and the inital risk. 
risks_fct <- function(distribution_index, n, r_f, r_i){
  constant <- risk_constant(distribution_index, n, r_f, r_i)
  #create vector
  risks_vect <- rep(NA, n)
  #iterate over each group in the multinomial
  for (iterate_deg_index in 1:n) {
    #if first, initial
    if(iterate_deg_index == 1){
      risk_current_val <- r_i
      #if last, final
    }else if(iterate_deg_index == n){
      risk_current_val <- r_f
      #otherwise calculate from the risk iterated fct
    } else {
      risk_current_val <- risk_next(distribution_index, previous_risk, constant)
    }
    #write into vector
    risks_vect[iterate_deg_index] <- risk_current_val
    #write out previous risk so the iteration works
    previous_risk <- risk_current_val
    
  }
  #return completed risks
  return(risks_vect)
}






#produces a list object indexed by risk distribution, initial risk level,  
#working final risk level, degree of multinomial, then with one entry for 
#each multinom category
#making list for risks and constants. 1 is additive, 2 multiplicative, 3 power
risks <- vector(mode = "list", length = 3)

#iterate over each kind of risk distribution
for (risk_dist_index in 1:3) {
  #preallocate for initial risk
  risks[[risk_dist_index]] <- vector(mode = "list", length = length(risk_i))
  #iterate over each inital risk
  for (r_i_index in 1: length(risk_i)) {
    #find number of workable final risks
    num_final_risks <- length(which(risk_f>risk_i[r_i_index]))
    #preallocate
    risks[[risk_dist_index]][[r_i_index]] <- vector(mode = "list", 
                                                    length = num_final_risks)
    #set indext for final risks before we go into the intial iteration, so it 
    #resets each loop
    counter_risk_f <- 1
    #now only run for workable final risks (larger than initial)
    for (r_f_index in 1:length(risk_f)) {
      #test that this is a working pair
      if(risk_f[r_f_index]> risk_i[r_i_index]){
        #preallocate for degree of multinomial
        risks[[risk_dist_index]][[r_i_index]][[counter_risk_f]]<- 
          vector(mode = "list", length(multinomials))
        #iterate for each different degree of multinomial
        for (multi_deg_index in 1:length(multinomials)) {
          #calculate
          risks[[risk_dist_index]][[r_i_index]][[counter_risk_f]][[multi_deg_index]]<- 
            risks_fct(risk_dist_index, degrees[multi_deg_index], 
                      risk_f[r_f_index], risk_i[r_i_index])
          
        }
        #move counter up one
        counter_risk_f <- counter_risk_f+1
      }
      
    }
  }
  
}



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
  simul_list<- vector(mode = "list", length = 10000)
  for (simulation_index in 1:10000) {
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