#Score interval
score_interval <- function(p, n, confidence){
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
  return(limits)
}