## this just graphs
#import multinomials and risks
multinomials<- readRDS("/Data/multinomials.rds")
risks <- readRDS("/Data/risks.rds")

##graphing distributions
titles_multinom <- c("Degree 2", "Degree 3", "Degree 4", "Degree 5", "Degree 6", "Degree 7")

maintitles <- c("Constant added", "Constant multiplied", "Raised to constant")

color_vector <- c("#ccfdff", "#99F8FF","#66F0FF", "#33E4FF", "#00AACC", 
                  "#007A99")
#multinomials graphs
for(m_index in 1:length(titles_multinom)){
  n_graphs <- length(multinomials[[m_index]])
  par(mfrow = c(n_graphs/2, n_graphs/2))
  for(g_index in 1:n_graphs){
    if(g_index == 1){
      barplot(multinomials[[m_index]][[g_index]], main = titles_multinom[m_index], 
              ylab = "Probabilities", col = color_vector[m_index], 
              border = "black", space = c(0,0), xlab = "Ranked groups")
    }else{
      barplot(multinomials[[m_index]][[g_index]], main = NULL, 
              ylab = "Probabilities", col = color_vector[m_index], 
              border = "black", space = c(0,0), xlab = "Ranked groups")
    }
  }
}
#warnings here??
##risks
