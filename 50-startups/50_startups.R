## Multi Linear Regression for 50 startups 
library('psych')
plot(startups)
pairs.panels(startups)
attach(startups)
reg_sim <- lm(Profit~.,data = startups)
summary(reg_sim)
pred_sim <- predict(reg_sim,interval = 'predict')
pred_sim <- as.data.frame(pred_sim)
cor_val_sim <- cor(pred_sim$fit,startups$Profit)
cor_val_sim

install.packages('dummies')
library('dummies')
dummy_startups <- dummy.data.frame(startups, sep = ".")
head(dummy_startups)

names(startups)
names(dummy_startups)
str(dummy_startups)
dim(dummy_startups)

dummy_startups <- dummy_startups[rowSums(dummy_startups['R.D.Spend'] == 0) == 0, ]
dummy_startups <- dummy_startups[rowSums(dummy_startups['Marketing.Spend'] == 0) == 0, ]
dummy_startups %>% drop_na()

View(dummy_startups)
names(dummy_startups)[names(dummy_startups) == 'State.New York'] <- 'State.NewYork'
dummy_startups <- dummy_startups[-6]

pairs.panels(startups)
reg_dummy <- lm(dummy_startups$Profit~., data = dummy_startups)
summary(reg_dummy)
pred_dummy <- predict(reg_dummy,interval = 'predict')
pred_dummy <- as.data.frame(pred_dummy)
cor_val_dummy <- cor(pred_dummy$fit,dummy_startups$Profit)
cor_val_dummy

reg_dummy_impv <- lm(dummy_startups$Profit~dummy_startups$Administration+dummy_startups$Marketing.Spend
                     + dummy_startups$State.California+dummy_startups$State.Florida, data = dummy_startups
)
summary(reg_dummy_impv)

reg_dummy_admin <- lm(dummy_startups$Profit~dummy_startups$Administration, data = dummy_startups)
summary(reg_dummy_admin)

reg_dummy_market <- lm(dummy_startups$Profit~dummy_startups$Marketing.Spend, data = dummy_startups)
summary(reg_dummy_market)


reg_dummy_impv1 <- lm(dummy_startups$Profit~dummy_startups$R.D.Spend+dummy_startups$Marketing.Spend
                     + dummy_startups$State.California+dummy_startups$State.Florida, data = dummy_startups
)
summary(reg_dummy_impv1)

library(car)
plot(reg_dummy)
influenceIndexPlot(reg_dummy)
influencePlot(reg_dummy)

vif(reg_dummy)
library(MASS)
stepAIC(reg_dummy)

reg_dummy_aic <- lm(dummy_startups$Profit ~ R.D.Spend + Administration, data = dummy_startups)
summary(reg_dummy_aic)
pred_dummy_aic <- predict(reg_dummy_aic,interval = 'predict')
pred_dummy_aic <- as.data.frame(pred_dummy_aic)
cor_val_dummy_aic <- cor(pred_dummy_aic$fit,dummy_startups$Profit)
cor_val_dummy_aic

rm(total_result)
total_result <- data.frame(
  'algoname' =NULL,
  'algo-exprs' = NULL,
  'rsquared' = NULL,
  'corValue' = NULL
)

temp.data <- data.frame('reg_dummy_aic','lm(formula = dummy_startups$Profit ~ R.D.Spend + Administration, 
    data = dummy_startups)',
                        '0.9592',cor_val_dummy_aic)
names(temp.data) <- c('algoname','algo-exprs','rsquared','corValue')
total_result <- rbind(total_result,temp.data)
View(total_result)