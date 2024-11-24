#import libs
lapply(c("foreach", "doParallel", "epitools", "randomForest"), library, character.only = T)
#import functions

load("./Data/labels.Rdata")
multinomials <- readRDS("./Data/multinomials.rds")
risks <- readRDS("./Data/risks.rds")


#risks and probability generation. 
#need to pull in the degrees, 

#make a single list so can apply. [[unique distribution id]][[risk/prob =1, 
#vector of stuff =2]]
#initialize list with enough room
all_risks_and_probs<- vector(mode = "list", length = 1000000)
#set counter for each entry to 1
counter_for_distribs <- 1
#for every kind of risk distribution
#risks in second column, probs in first, both in matrix in [[]][[1]]
#[[]][[2]] holds various other needed--r_i, r_f, type of risk distrib, 
##prob distrib, degree of multinomial
for (l_index in 1:length(risks)) {
  #for every initial rist
  for (o_index in 1:length(risks[[l_index]])){
    #for every final risk
    for (p_index in 1:length(risks[[l_index]][[o_index]])){
      #for degree of multinom
      for (n_index in 1:length(risks[[l_index]][[o_index]][[p_index]])) {
        risk_temp <- risks[[l_index]][[o_index]][[p_index]][[n_index]]
        #for multinom distribution
        for (m_index in 1:length(distribution_labels)) {
          #initialize positions for matrix and vector
          all_risks_and_probs[[counter_for_distribs]]<- 
            vector(mode= "list", length =2)
          
          #write in matrix for one column of risks, one of probs
          all_risks_and_probs[[counter_for_distribs]][[1]]<- 
            matrix(nrow = degrees[n_index], 
                   ncol = 2)
          all_risks_and_probs[[counter_for_distribs]][[1]][,2]<- 
            risks[[l_index]][[o_index]][[p_index]][[n_index]]
          all_risks_and_probs[[counter_for_distribs]][[1]][,1]<- 
            multinomials[[n_index]][[m_index]]
          #write in important characteristics
          #initial and final from the risks vector
          risk_temp_i <- min(risks[[l_index]][[o_index]][[p_index]][[n_index]])
          risk_temp_f <- max(risks[[l_index]][[o_index]][[p_index]][[n_index]])
          #pull the kind of risk distribution
          risk_temp_distrib <- risk_distrib_labels[l_index]
          #pull the probability distribution for the multinom
          prob_temp_distrib <- distribution_labels[m_index]
          #pull the degree
          degree_temp <- degrees[n_index]
          all_risks_and_probs[[counter_for_distribs]][[2]] <- 
            c(risk_temp_i, risk_temp_f, risk_temp_distrib, prob_temp_distrib, 
              degree_temp)
          #update counter
          counter_for_distribs<- counter_for_distribs +1
        }
      }
      
    }
    
  }
}
#remove nulls
all_risks_and_probs <- Filter(Negate(is.null), all_risks_and_probs)

#write to file
#initialize and open
saveRDS(all_risks_and_probs, "./Data/risk_prob.rds")
