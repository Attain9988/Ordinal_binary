
#import parameters

load("./Data/labels.Rdata")

#---
#this generates the multinomials-- i.e. how many events are in each level
#import labels and such

#--functions for multinomials 
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
#---multinomial generation

#make an empty list for multiomials
multinomials <- vector(mode = "list", length = length(degrees))





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

#-- this section calculates the probability of success given level

#functions input
#risks functions
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


##risks

#initial risk and final risk array
#initial risk is the  condit prob of success at the lowest level, risk f at the highest 
risk_i <- seq(from= 0, to = 1, length.out = 20)
risk_f <- seq(from = 0, to = 1, length.out = 20)

#because of multiplicative and power, need to remove 0 from initial
#further need to remove one more from the initial so that there is at least one left for final
#similarly, the initial needs to be less than the final risk, so we remove the second for final
risk_i <- risk_i[-c(1,20)]
risk_f <- risk_f[-c(1,2)]

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


#--- combine into distributions

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
#for risk increase type
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
saveRDS(all_risks_and_probs, "./Data/risk_prob.rds")
