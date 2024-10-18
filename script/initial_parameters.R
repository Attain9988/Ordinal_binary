#degrees and labels
#define how many degrees of each
degrees <- c(2, 3, 4, 5, 6, 7)
#distribution labels
distribution_labels <- c("normal", "bimodal", "left_skew", "right_skew", 
                         "uniform")
risk_distrib_labels <- c("addition", "multiplication", "power")
save(degrees, distribution_labels, risk_distrib_labels, file = "./Data/labels.Rdata")
