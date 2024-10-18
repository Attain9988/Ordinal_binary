#functions input
source("./Functions/risk_distrib_fcts.R")
#read in multinomials
multinomials <- readRDS("./Data/multinomials.rds")
#read in labels etc
load("./Data/labels.Rdata")
##risks

#initial risk and final risk array
risk_i <- seq(from= 0, to = 1, length.out = 20)
risk_f <- seq(from = 0, to = 1, length.out = 20)
risk_i <- risk_i[-c(1,20)]
risk_f <- risk_f[-c(1,2)]

#produces a list object indexed by risk distribution, initial risk level,  
#working final risk level, degree of multinomial, then with one entry for 
#each multinom category
#making list for risks and constants. 1 is additive, 2 multiplicative, 3 power
risks <- vector(mode = "list", length = 3)

#iterate over each kind of risk distribution
for (risk_dist_index in 1:3) {
  #preallocate for initial risk
  risks[[risk_dist_index]] <- vector(mode = "list", length = length(risk_i))
  #iterate over each inital risk
  for (r_i_index in 1: length(risk_i)) {
    #find number of workable final risks
    num_final_risks <- length(which(risk_f>risk_i[r_i_index]))
    #preallocate
    risks[[risk_dist_index]][[r_i_index]] <- vector(mode = "list", 
                                                    length = num_final_risks)
    #set indext for final risks before we go into the intial iteration, so it 
    #resets each loop
    counter_risk_f <- 1
    #now only run for workable final risks (larger than initial)
    for (r_f_index in 1:length(risk_f)) {
      #test that this is a working pair
      if(risk_f[r_f_index]> risk_i[r_i_index]){
        #preallocate for degree of multinomial
        risks[[risk_dist_index]][[r_i_index]][[counter_risk_f]]<- 
          vector(mode = "list", length(multinomials))
        #iterate for each different degree of multinomial
        for (multi_deg_index in 1:length(multinomials)) {
          #calculate
          risks[[risk_dist_index]][[r_i_index]][[counter_risk_f]][[multi_deg_index]]<- 
            risks_fct(risk_dist_index, degrees[multi_deg_index], 
                      risk_f[r_f_index], risk_i[r_i_index])
          
        }
        #move counter up one
        counter_risk_f <- counter_risk_f+1
      }
      
    }
  }
  
}

saveRDS(risks, "./Data/risks.rds")
