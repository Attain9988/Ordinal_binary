all_risks_and_probs <- readRDS("./Data/risk_prob.rds")

#function to retrieve degree from distribution index alone
find_degree_fct <- function(distribution_index){
  result<- as.numeric(all_risks_and_probs[[distribution_index]][[2]][5])
  return(result)
}

find_initial_risk_fct <- function(distribution_index){
  result <- as.numeric(all_risks_and_probs[[distribution_index]][[2]][1])
  return(result)
}

find_final_risk_fct <- function(distribution_index){
  result <- as.numeric(all_risks_and_probs[[distribution_index]][[2]][2])
  return(result)
}
find_from_dist_index <- function(distribution_index, what_find){
  if(what_find=="degree"){
    return(find_degree_fct(distribution_index))
  }else if(what_find =="init_risk"){
    return(find_initial_risk_fct(distribution_index))
  }else if(what_find =="final_risk"){
    return(find_final_risk_fct(distribution_index))
  }else{
    stop("invalid input")
  }
}
