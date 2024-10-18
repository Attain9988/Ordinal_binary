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


