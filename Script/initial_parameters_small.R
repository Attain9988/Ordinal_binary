#degrees and labels
#define how many degrees of each
degrees <- c(2, 3, 4, 5, 6, 7)
#distribution labels
distribution_labels <- c("normal", "bimodal", "left_skew", "right_skew", 
                         "uniform")
risk_distrib_labels <- c("addition", "multiplication", "power")
#set how large finely the risks are calculated
risks_by <- .1
#set the number of simulations and the number of data points in each simulated 
##trial
num_sims <- 100
num_data_per_sim <- 100
#fraction of categories that need to pass to be considered a success
contained_thres <- 0.6
#vector of rankings
rankings <- c("categorical", "nominal", "midrank")
save(degrees, distribution_labels, risk_distrib_labels, num_data_per_sim, 
     num_sims, rankings, contained_thres,  file = "./Data/labels_small.Rdata")
