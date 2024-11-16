lapply(c("./Functions/simul_expand_fcts.R", "./Functions/score_interval.R"), 
       source)
#for a single expanded output contained in
# the expanded version of simulations
#build the models and predictions
#need the expanded simulation and the true risks
#logistic per category
#for each category, odds of success vs not success given level
#does all of the function for the expanded, needs corresponding distrib index 
#returns whether or not enough of the categories were captured by the score
logistic_fct <- function(expanded, true_risks){
  
  expanded<- as.data.frame(expanded)
  #force as factor
  expanded$categoric <- as.factor(expanded$categoric)
  #models
  cat_mod <- glm(`Success=1` ~ categoric, data = expanded)
  nom_mod <- glm(`Success=1`~ nominal, data = expanded)
  mid_mod <- glm(`Success=1` ~ midrank, data = expanded)
  
  
  
  #makes vectors of levels for confint/predic
  newdat <- data.frame( categoric = as.factor(unique(expanded$categoric)), 
                        nominal = unique(expanded$nominal),
                        midrank = unique(expanded$midrank))
  
  
  #predictions--i.e. P(success | level)
  predict_cat <- predict(cat_mod, newdata = newdat, type = "response")
  predict_nom <- predict(nom_mod, newdata = newdat, type = "response")
  predict_mid <- predict(mid_mod, newdata = newdat, type = "response")
  
  #within score
  contained_cat <- rep(NA, length(unique(expanded$categoric)))
  contained_nom <- rep(NA, length(unique(expanded$categoric)))
  contained_mid <- rep(NA, length(unique(expanded$categoric)))
  #fore each category (multinomial level)
  for (category_index in 1:length(unique(expanded$categoric))) {
    #pull values
    true_val<-true_risks[category_index]
    #number of entries in that category
    nval<- length(which(expanded$categoric == category_index))
    #score interval w/95% conf
    score_cat <- score_interval (predict_cat[category_index], nval, .05)
    score_nom <- score_interval(predict_nom[category_index], nval, .05)
    score_mid <- score_interval(predict_mid[category_index], nval, .05)
    #is contained, true if true value within bounds
    contained_cat[category_index] <- true_val >= score_cat[1] & 
      true_val <= score_cat[2]
    contained_nom[category_index] <- true_val >= score_nom[1] & 
      true_val <= score_nom[2]
    contained_mid[category_index] <- true >= score_mid[1] & 
      true_val <= score_mid[2]
  }
  #is nominal contained, 0 not contained 1 contained. 
  ##At least 60% of the categories must contain
  cat_cont <- ifelse(mean(contained_cat) < contained_thres, 0,1)
  #is nominal contained, 0 not contained 1 contained
  nom_cont <- ifelse(mean(contained_nom) < contained_thres, 0,1)
  #mid
  mid_cont <- ifelse(mean(contained_mid) < contained_thres, 0,1)
  #return list/vect with 
  return(c(cat_cont, nom_cont, mid_cont))
}


# expanded = expanded_simulation_list[[6]][[1]]
# distrib_index = 6
# 
# expanded<- as.data.frame(expanded)
# #force as factor
# expanded$categoric <- as.factor(expanded$categoric)
# #models
# cat_mod <- glm(`Success=1` ~ categoric, data = expanded)
# nom_mod <- glm(`Success=1`~ nominal, data = expanded)
# mid_mod <- glm(`Success=1` ~ midrank, data = expanded)
# 
# 
# #makes vectors of levels for confint/predic
# newdat <- data.frame( categoric = as.factor(unique(expanded$categoric)), 
#                       nominal = unique(expanded$nominal),
#                       midrank = unique(expanded$midrank))
# 
# 
# #predictions--i.e. P(success | level)
# predict_cat <- predict(cat_mod, newdata = newdat, type = "response")
# predict_nom <- predict(nom_mod, newdata = newdat, type = "response")
# predict_mid <- predict(mid_mod, newdata = newdat, type = "response")
# 
# #within score
# contained_cat <- rep(NA, length(unique(expanded$categoric)))
# contained_nom <- rep(NA, length(unique(expanded$categoric)))
# contained_mid <- rep(NA, length(unique(expanded$categoric)))
# #fore each category (multinomial level)
# for (category_index in 1:length(unique(expanded$categoric))) {
#   #pull values
#   true<-all_risks_and_probs[[distrib_index]][[1]][category_index ,2]
#   #number of entries in that category
#   nval<- length(which(expanded$categoric == category_index))
#   #score interval w/95% conf
#   score_cat <- score_interval (predict_cat[category_index], nval, .05)
#   score_nom <- score_interval(predict_nom[category_index], nval, .05)
#   score_mid <- score_interval(predict_mid[category_index], nval, .05)
#   #is contained, true if true within bounds
#   contained_cat[category_index] <- true >= score_cat[1] & 
#     true <= score_cat[2]
#   contained_nom[category_index] <- true >= score_nom[1] & 
#     true <= score_nom[2]
#   contained_mid[category_index] <- true >= score_mid[1] & 
#     true <= score_mid[2]
# }
# #is nominal contained, 0 not contained 1 contained. 
# ##At least 60% of the categories must contain
# cat_cont <- ifelse(mean(contained_cat < .6), 0,1)
# #is nominal contained, 0 not contained 1 contained
# nom_cont <- ifelse(mean(contained_nom < .6), 0,1)
# #mid
# mid_cont <- ifelse(mean(contained_mid < .6), 0,1)
# 
# lil_variable = data.frame(cat_cont, nom_cont, mid_cont)
# print(expanded_simulation_list[[6]][[1]])
# print(lil_variable)
