#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/simulations_fcts.R")
load("./Data/labels.Rdata")


multinomials <- readRDS("./Data/multinomials.rds")
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")

#now do the simulations

all_risks_and_probs[0]

#code for simulation

#parallelize
#find number of cores
num_cores <- detectCores()

#make a cluster of the cores and parallize
cl <- makeCluster(num_cores)
registerDoParallel(cl)
##editing from here
#simulate all and run nsims times
# outputs a list of simulations, simulations_list[[i]][[j]], where i is the
##index corresponding to the ith risk and prob, and j is the jth simulation
simulations_list <- foreach(z_index = 1:length(all_risks_and_probs)) %dopar% {
  #run the simulation for the zth risk prob and risk vectors
  simul_list<- vector(mode = "list", length = num_sims)
  for (simulation_index in 1:num_sims) {
    simul_list[[simulation_index]] <- single_sim_fct(
      all_risks_and_probs[[z_index]][[1]][, 1], 
      all_risks_and_probs[[z_index]][[1]][, 2]) 
  }
  simul_list
}

stopCluster(cl)

saveRDS(simulations_list, "./Data/simulations.rds")
