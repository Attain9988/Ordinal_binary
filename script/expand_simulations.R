#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/simul_expand_fcts.R")
load("./Data/labels.Rdata")
simulations_list <- readRDS("./Data/simulations.rds")


#find number of cores
num_cores <- detectCores()
#expand all so logistic work
cl <- makeCluster(num_cores)
registerDoParallel(cl)


#expand all simulations. [[w_index]][[expand_index]] where w index is the 
#wth distribution and expand is the simulation number
expanded_simulations_list <- foreach(w_index = 1:15390) %dopar% {
  #this length needs to be equal to the number of simulations. 
  expand_list<- vector(mode = "list", length = num_sims)
  for (expand_index in 1:num_sims) {
    #expand the simulation for the wth simulation
    expand_list[[expand_index]] <- expand_fct(simulations_list[[w_index]]
                                              [[expand_index]])
  }
  expand_list
}

stopCluster(cl)


saveRDS(expanded_simulations_list, "./Data/expanded_simulations.rds")