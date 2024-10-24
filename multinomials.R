#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions
source("./Functions/finding_fcts.R")
load("./Data/labels.Rdata")
simulations_list <- readRDS("./Data/simulations.rds")
all_risks_and_probs <- readRDS("./Data/risk_prob.rds")
model_fails_list <- readRDS("./Data/model_fails.rds")


#want to find the coverage rate. For each distribution each way this will be the 
#mean of the trials. 

#first get the trials all collated. input 1 for cat, 2 for nom, 3 for mid
coverage_fct <- function(distribution, ranking){
  #preallocate
  #find degree
  degree <- find_from_dist_index(distribution, "degree")
  #set up empty for 
  coverage_list<- rep(NA, nSims)
  #pull each in
  for (sim_ind in 1:nSims) {
    coverage_list[sim_ind]<-
      model_fails_list[[distribution]][[sim_ind]][1,ranking]     
  }
  #calculate coverage via mean
  return(mean(coverage_list))
}

#now output the coverage rates and make final tables

cl <- makeCluster(num_cores)
registerDoParallel(cl)

coverage_rates <- foreach(d_index = 1:length(all_risks_and_probs))%dopar%{
  coverage_fct()
  #for each kind of ranking find the coverage
  for (ranking_ind in 1:length(rankings)) {
    coverage_list[[ranking_ind]] <- coverage_fct(distribution, ranking_ind)
  }
  coverage_list
}

saveRDS(coverage_rates, file = "./Data/coverage_rates.rds")
stopCluster(cl)


# random forest fragment stuff
train_set <- sample(1:nrow(coverage_tab), .30*nrow(coverage_tab))
rf_mod <- randomForest(coverage_tab[,1]~., subset = train_set, mtry = 2, importance = TRUE)