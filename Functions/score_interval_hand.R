#Score interval
score_interval <- function(p, n, confidence, correction = TRUE){
  if(correction == FALSE){
  z_ahalf <- qnorm((1-confidence)/2) #for desired sig level
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
    lower_sqrt<- sqrt(z^2-2-(1/n)+4*p*(n*q+1))
    upper_sqrt <- sqrt(z^2+2-(1/n)+4*p*(n*q-1))
    lower_sum <- 2*n*p+z^2-1-z*lower_sqrt
    upper_sum <- 2*n*p+z^2+1+z*upper_sqrt
    #bind together
    limits <- cbind(lower_sum, upper_sum)
    limits <- limits/denom
    return(limits)
  }
}