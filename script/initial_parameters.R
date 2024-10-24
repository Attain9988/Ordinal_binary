#degrees and labels
#define how many degrees of each
degrees <- c(2, 3, 4, 5, 6, 7)
#distribution labels
distribution_labels <- c("normal", "bimodal", "left_skew", "right_skew", 
                         "uniform")
risk_distrib_labels <- c("addition", "multiplication", "power")
#set the number of simulations and the number of data points in each simulated 
##trial
num_sims <- 2
num_data_per_sim <- 100
#vector of rankings
rankings <- c("categorical", "nominal", "midrank")
save(degrees, distribution_labels, risk_distrib_labels, num_data_per_sim, 
     num_sims, rankings,  file = "./Data/labels.Rdata")
