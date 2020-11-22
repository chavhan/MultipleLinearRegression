## Toyota corolla car prediction 
dim(ToyotaCorolla)
ncol(ToyotaCorolla)
nrow(ToyotaCorolla)
names(ToyotaCorolla)
ToyotaCorolla[,'Price']
ToyotaCorolla[,3]

CarWorkingData <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
head(CarWorkingData)

str(CarWorkingData)

## removing N/A values 
library('tidyr')
CarWorkingData %>% drop_na()
nrow(CarWorkingData)

plot(CarWorkingData)
pairs.panels(CarWorkingData)
attach(CarWorkingData)
reg_sim <- lm(Price~.,data = CarWorkingData)
summary(reg_sim)

reg_cc <- lm(Price~cc,data = CarWorkingData)
summary(reg_cc)

reg_doors <- lm(Price~Doors, data = CarWorkingData)
summary(reg_doors)

reg_ccdoors <- lm(Price~cc+Doors,data = CarWorkingData)
summary(reg_ccdoors)

library(corpcor)
cor2pcor(cor(CarWorkingData))
View(CarWorkingData)

library(car)
plot(reg_sim)

influenceIndexPlot(reg_sim)
influencePlot(reg_sim)

reg_impSim <- lm(Price~.,data=CarWorkingData[-81,])
summary(reg_impSim)

vif(reg_sim)
avPlots(reg_sim,id.n=5,id.cex=100,col="red")

library(MASS)
stepAIC(reg_sim)
stepAIC(reg_impSim)

reg_final <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = CarWorkingData[-81,])
summary(reg_final)

rm(total_result)
total_result <- data.frame(
  'algoname' =NULL,
  'algo-exprs' = NULL,
  'rsquared' = NULL,
  'corValue' = NULL
)

pred_sim <- predict(reg_sim,interval = 'predict')
pred_sim <- as.data.frame(pred_sim)
head(pred_sim)
dim(pred_sim)
cor_val_sim <- cor(pred_sim$fit,Price)
cor_val_sim

pred_impSim <- predict(reg_impSim,interval = 'predict')
pred_impSim <- as.data.frame(pred_impSim)
head(pred_impSim)
dim(pred_impSim)
cor_val_impsim <- cor(pred_impSim$fit,CarWorkingData[-81,]$Price)
cor_val_impsim

pred_final <- predict(reg_final,interval = 'predict')
pred_final <- as.data.frame(pred_final)
head(pred_final)
cor_val_final <- cor(pred_final$fit,CarWorkingData[-81,]$Price)
cor_val_final

cor_val_sim
cor_val_impsim
cor_val_final

temp.data <- data.frame('reg_final','lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
    Weight, data = CarWorkingData[-81, ])',
                        '0.8632',cor_val_final)
names(temp.data) <- c('algoname','algo-exprs','rsquared','corValue')
total_result <- rbind(total_result,temp.data)
View(total_result)



avPlots(reg_final,id.n=5,id.cex=100,col="red")

reg_testMod <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax, data = CarWorkingData)
summary(reg_testMod)
pred_testMod <- predict(reg_testMod,interval = 'predict')
pred_testMod <- as.data.frame(pred_testMod)
cor(pred_testMod$fit,Price)

