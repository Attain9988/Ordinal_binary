#produces a list object indexed by risk distribution, initial risk level,  
#working final risk level, degree of multinomial, then with one entry for 
#each multinom category
distrib_matrix <- matrix(data = NA, nrow=15390, ncol = 5)
row_index <- 1
for (risk_dist in 1:3) {
  lengths_risk_type<-length(risks)
  gl()
}