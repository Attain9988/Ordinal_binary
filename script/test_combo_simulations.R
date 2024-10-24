#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/simulations_fcts.R")
source("./Functions/simul_expand_fcts.R")
load("./Data/labels.Rdata")


single_simulation_fct <- function(current_risk_prob){
  
  simul_list<- vector(mode = "list", length = num_sims)
  for (simulation_index in 1:num_sims) {
    simul_list[[simulation_index]] <- single_sim_fct(
      current_risk_prob[[1]][, 1], 
      current_risk_prob[[1]][, 2]) 
  }
    expand_list<- vector(mode = "list", length = num_sims)
    for (expand_index in 1:num_sims) {
      #expand the simulation for the wth simulation
      expand_list[[expand_index]] <- expand_fct(simul_list[[expand_index]])
    }
  
  return(expand_list)
}
