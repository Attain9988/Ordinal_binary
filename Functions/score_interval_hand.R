#Score interval
#takes in the proportion of successes, p, number of trials, n, confidence level, confidence, 
## and the correction, true/ False (defaults to TRUE)
score_interval <- function(p, n, confidence, correction = TRUE){
  if(correction == FALSE){
  z_ahalf <- qnorm(1-(1-confidence)/2) #for desired sig level
  zterm <- z_ahalf^2/n
  firstsqrtterm <- (2*p + zterm)^2 #first term under square root
  secondsqrtterm <- 4*p^2*(1+zterm) #second
  denom <- 2*(1+zterm) #denominator
  sumpart <- 2*p+zterm # initial part that is summed
  sqrtpart <- sqrt(firstsqrtterm-secondsqrtterm)
   
  
  limit2 <- (sumpart+sqrtpart)/denom
  limit1 <- (sumpart-sqrtpart)/denom
  
  limits <- cbind(limit1, limit2)
  return(limits)}else{
    #wants upper
    z <- qnorm(1-(1-confidence)/2)
    q <- 1-p
    denom <- 2*(n+z^2)
    sqrt_term<- z*sqrt(z^2-(1/n)+4*p*n*(1-p)+(4*p-2))+1
    first_two_terms <- 2*n*p+z^2
    lower_bound <- max(0, (first_two_terms-sqrt_term)/denom)
    upper_bound <- min(1, (first_two_terms+sqrt_term)/denom)
    #bind together
    limits <- cbind(lower_bound, upper_bound)
    return(limits)
  }
}


