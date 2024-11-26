#Score interval
score_interval <- function(p, n, confidence){
  
  #number of successes for estimated probability, number of trials, continuity 
  #correction
  wilson_score<- prop.test(x = p*n, n = n, conf.level = confidence, 
                           alternative = "two.sided", correct = TRUE)
  
  limit2 <- wilson_score[[6]][2]
  limit1 <- wilson_score[[6]][1]
  
  limits <- cbind(limit1, limit2)
  return(limits)
}