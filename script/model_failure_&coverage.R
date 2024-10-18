#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/model_fcts.R")
load("./Data/labels.Rdata")
simulations_list <- readRDS("./Data/simulations.rds")
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")




cl <- makeCluster(num_cores)
registerDoParallel(cl)

#determine whether the model captures or not for each simulation
#for each distribution
model_fails_list <- foreach(b_index = 1:length(simulations_list)) %dopar% {
  #initialize
  model_list<- vector(mode = "list", length = nSims)
  #for each trial
  for (model_index in 1:nSims) {
    #run the models
    #for each distribution and then each trial of each, write in 1s for 
    model_list[[model_index]] <- logistic_fct(expanded_simulations_list[[b_index]]
                                              [[model_index]], b_index)
  }
  model_index
}

saveRDS(model_fails_list, file = "./Data/model_fails.rds")
stopCluster(cl)