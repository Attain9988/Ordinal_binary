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