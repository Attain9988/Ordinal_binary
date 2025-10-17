coverage<-readRDS("./Data/coverage_small.rds")



#libraries 
lapply(c("MASS", "doParallel", "epitools", "fastglm", "Rmpi"), library, character.only = T)


#models
linear_prediction_cat <- lm((mean_cat) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_nom <- lm((mean_nom) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_mid <- lm((mean_mid) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)

#positive responses
#models
linear_prediction_cat_pos <- lm((mean_cat+.0001) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_nom_pos <- lm((mean_nom+.0001) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_mid_pos <- lm((mean_mid+.0001) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)

#repeat models 
repeat_models<- c(linear_prediction_cat, linear_prediction_nom, 
                   linear_prediction_mid)
repeat_labels<- c("cat", "nom", "mid")


#plots for cat
png("./Plots/Resids_cat.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_cat, main = "Categorical")
dev.off()

#plots for mid 
png("./Plots/Resids_mid.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_mid, main = "Midrank")
dev.off()

#plots for nom
#opens png and 
png("./Plots/Resids_nom.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_nom, main = "Nominal")
dev.off()


#boxcox
png("./Plots/box_cat.png")
par(mfrow= c(2,1))
boxCox(linear_prediction_cat, family = "yjPower", 
       main= "Categorical Yeo-Johnson")
boxCox(linear_prediction_cat_pos, family = "bcPower", 
       main= "Categorical standard")
dev.off()

png("./Plots/box_nom.png")
par(mfrow= c(2,1))
boxCox(linear_prediction_nom, family = "yjPower", 
       main= "Nominal Yeo-Johnson")
boxCox(linear_prediction_nom_pos, family = "bcPower", 
       main= "Nominal standard")
dev.off()

png("./Plots/box_mid.png")
par(mfrow= c(2,1))
boxCox(linear_prediction_mid, family = "yjPower", 
       main= "Nominal Yeo-Johnson")
boxCox(linear_prediction_mid_pos, family = "bcPower", 
       main= "Nominal standard")
dev.off()

#applicable yeoJohnson transform
yj_trans_fct <- function(response_var, lambda){
  num <- (response_var +1)^lambda-1
  denom <- lambda
  return(num/denom)
}

yj_cat <- yj_trans_fct(coverage$mean_cat, -2)
yj_nom <- yj_trans_fct(coverage$mean_nom, -2)
yj_mid <- yj_trans_fct(coverage$mean_mid, -2)


#post boxcox suggestion
linear_prediction_cat_bc <- lm(log(mean_cat+.0001) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_nom_bc <- lm(log(mean_nom+.0001) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_mid_bc <- lm(log(mean_mid+.0001) ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)

linear_prediction_cat_yj <- lm(yj_cat ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_nom_yj <- lm(yj_nom ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)
linear_prediction_mid_yj <- lm(yj_mid ~ risk_initial*risk_final*risk_distrib*prob_distrib*degree, data = coverage)

#plots for cat
png("./Plots/Resids_cat_bc.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_cat_bc, main = "Categorical boxcox")
dev.off()

#plots for mid 
png("./Plots/Resids_mid_bc.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_mid_bc, main = "Midrank boxcox")
dev.off()

#plots for nom
#opens png and 
png("./Plots/Resids_nom_bc.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_nom, main = "Nominal boxcox")
dev.off()


#plots for cat
png("./Plots/Resids_cat_yj.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_cat_yj, main = "Categorical Yeo-Johnson")
dev.off()

#plots for mid 
png("./Plots/Resids_mid_yj.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_mid_yj, main = "Midrank Yeo-Johnson")
dev.off()

#plots for nom
#opens png and 
png("./Plots/Resids_nom_yj.png")
par(mfrow = c(2, 2))
#fitted vs resid
plot(linear_prediction_nom_yj, main = "Nominal Yeo-Johnson")
dev.off()


#also called a bunch of summaries of the models, did not record

#values
cols_for_mids <- c(coverage[,1], coverage[,2], coverage[,3])
means_of_mods <- apply(coverage[,1:3], 2, mean)
medians_of_mods <- apply(coverage[,1:3], 2, median)

#plots for cat
png("./Plots/scatterplot_cat.png")

#fitted vs resid
pairs(coverage[, c(1,4:8)], col = as.integer(coverage$mean_cat), 
      pch = 18)
dev.off()
