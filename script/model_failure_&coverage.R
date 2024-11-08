#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/model_fcts.R")
source("./Functions/simulations_fcts.R")
source("./Functions/simul_expand_fcts.R")

load("./Data/labels.Rdata")
#simulations_list <- readRDS("./Data/simulations.rds")
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")
expanded_simulation_list <- readRDS("./Data/expanded_simulations.rds")


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
  


num_cores <- detectCores()

cl <- makeCluster(num_cores)
registerDoParallel(cl)

#determine whether the model captures or not for each simulation, 1= capture
#for each distribution
#[[distribution]][[simulation]]dataframe of fail/success with category, nom, mid
model_fails_list <- foreach(b_index = 1:length(all_risks_and_probs)) %dopar% {
  
  
  #cur_expanded_sim <- single_simulation_fct(all_risks_and_probs[[b_index]])
  #initialize
  model_list<- vector(mode = "list", length = num_sims)
  #for each trial
  for (model_index in 1:num_sims) {
    #run the models
    #for each distribution and then each trial of each, write in 1s for 
    
    model_list[[model_index]] <- logistic_fct(expanded_simulation_list[[model_index]], 
                                                b_index)
    
    #model_list[[model_index]] <- logistic_fct(cur_expanded_sim[[model_index]], 
                                              #b_index)
  }
  model_list
}

saveRDS(model_fails_list, file = "./Data/model_fails.rds")
stopCluster(cl)