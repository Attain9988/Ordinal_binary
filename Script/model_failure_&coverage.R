#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/model_fcts.R")
source("./Functions/simulations_fcts.R")
source("./Functions/simul_expand_fcts.R")

load("./Data/labels.Rdata")
#simulations_list <- readRDS("./Data/simulations.rds")
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")



#fully running through simulation_gen and expanded simulations for one distribution
full_single_simulation_fct <- function(current_risk_prob){
  
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
  
#Goes through all of the simulations and returns the average for cat/nom/mid
average_contained <- function(model_list){
  #num_sims gives the dimensions of model_list
  cat_sum = 0
  nom_sum = 0
  mid_sum = 0
  #dumb solution TODO fix it(look up R indexing)
  for(i in 1:num_sims){
    cat_sum = cat_sum + model_list[[i]][[1]]
    nom_sum = nom_sum + model_list[[i]][[2]]
    mid_sum = mid_sum + model_list[[i]][[3]]
  }
  

  cat_sum = cat_sum / num_sims
  nom_sum = nom_sum / num_sims
  mid_sum = mid_sum / num_sims

  return(c(cat_sum, nom_sum, mid_sum))
}




num_cores <- detectCores()

cl <- makeCluster(num_cores)
registerDoParallel(cl)

#determine whether the model captures or not for each simulation, 1= capture
#for each distribution
#[[distribution]][[simulation]]dataframe of fail/success with category, nom, mid
model_fails_list <- foreach(b_index = 1:length(all_risks_and_probs)) %dopar% {
  
  library("epitools")
  #cur_expanded_sim <- single_simulation_fct(all_risks_and_probs[[b_index]])
  #initialize
  model_list<- vector(mode = "list", length = num_sims)
  #for each trial
  for (model_index in 1:num_sims) {
    #run the models
    #for each distribution and then each trial of each, write in 1s for 
    exp_sim = full_single_simulation_fct(all_risks_and_probs[[b_index]])
    model_list[[model_index]] <- logistic_fct(exp_sim[[model_index]], b_index)
    

  }
  #averages the cat/con/mid for all sims
  averaged_model_list <- average_contained(model_list)
  averaged_model_list
}

saveRDS(model_fails_list, file = "./Data/model_fails.rds")
stopCluster(cl)


