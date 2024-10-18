
#this generates the multinomials
#import functions
source("./Functions/multinomial_distrib_fcts.R")
#import labels and such
load("./Data/labels.Rdata")

#make an empty list for multiomials
multinomials <- vector(mode = "list", length = length(degrees))





#create the multinomials indexed by dgree then distrib
for (degree_index in 1:length(degrees)) {
  #initialize a list for each distribution
  multinomials[[degree_index]] <- vector(mode = "list", 
                                         length = length(distribution_labels))
  #fill in current degree
  curr_degree <- degrees[degree_index]
  #for each kind of distribution, run and fill in
  for (distribution_index in 1:length(distribution_labels)) {
    multinomials[[degree_index]][[distribution_index]] <- 
      distribution_fct(distribution_index, curr_degree)
  }
}

saveRDS(multinomials, "./Data/multinomials.rds")