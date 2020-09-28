# From Intro to Statistical Learning 6.5

# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(leaps)
library(car)

options(scipen = 999)

# Load data
df <- read_csv(here::here("data - raw", "Life Expectancy Data.csv"))

# Clean up column names
df_clean <- janitor::clean_names(df)

##Visualize data
head(df_clean)
str(df_clean)
summary(df_clean)
colnames(df_clean)

#visualize distribution of life expectancy
hist(df_clean$life_expectancy)
qqnorm(df_clean$life_expectancy)


# Data Cleanup ------------------------------------------------------------

# Convert country and status to factors
df_clean$country <- as_factor(df_clean$country)
df_clean$status  <- as_factor(df_clean$status)


# Best Subset Selection ---------------------------------------------------

# Check for na's
colSums(is.na(df_clean))

#Check for correlation
library(GGally)
ggcorr(df_clean, label = TRUE, label_alpha = TRUE, layout.exp = 2)

#remove redundant variables - pick ones with less NA

life_1 <- select(df_clean, -c(under_five_deaths, gdp, thinness_1_19_years))
colnames(life_1)
ggcorr(life_1, label = TRUE, label_alpha = TRUE, layout.exp = 2)

colSums(is.na(life_1))

#remove variables with zero or very minimal correlation to life_exp
life_1 <- select(life_1, -c(population))
ggcorr(life_1, label = TRUE, label_alpha = TRUE, layout.exp = 2)

#remove all rows with NA life expectancy and limit scope to not include those countries
life_1 <- life_1 %>%
  drop_na(life_expectancy)
colSums(is.na(life_1))

#for the interpretable model - remove all other NA's
life_1 <- life_1 %>%
  drop_na()
colSums(is.na(life_1))


#further limit to just three years - summarize years
life_1 %>% 
  group_by(year) %>% 
  summarize(count = n())

#removing all NA's reduced our sample size for years 2001 and 2015 significantly

#Let's go with most recent with good sample size, 2011-2014
life_y <- filter(life_1, year == "2014" | year == "2013" | year== "2012" | year == "2011")


# Maximum number of variables to consider - set to 4 to keep interpretable
consider <- 4

# Fit model - remove country, but keep in data set for interpretation
regfit_full <- regsubsets(life_expectancy ~ .,life_y[,-1], method = "forward", nvmax = consider)

# Examine regression summary
reg_summary <- summary(regfit_full)
names(reg_summary)
reg_summary$rsq

# Plot RSS, adjusted $R^2$ $C_p$ and BIC for all models
par(mfrow = c(2, 2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg_summary$adjr2)
points(consider, reg_summary$adjr2[consider], col = "red", cex = 2, pch = 20)
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg_summary$cp)
points(consider, reg_summary$cp[consider], col = "red", cex = 2, pch = 20)
which.min(reg_summary$bic)
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(consider, reg_summary$bic[consider], col = "red", cex = 2, pch = 20)

# Display selected variables for the best model with a given number of predictors
plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")
coef(regfit_full, consider)

# Forward and Backward Stepwise Selection ---------------------------------

# Forward
regfit_fwd <- regsubsets(life_expectancy ~ ., data = life_y[,-1], method = "forward")
summary(regfit_fwd)

# Backward
regfit_bwd <- regsubsets(life_expectancy ~ ., data = life_y[,-1], method = "backward")
summary(regfit_bwd)

# Compare coefficients
coef(regfit_full, consider)
coef(regfit_fwd, consider)
coef(regfit_bwd, consider)

#All models agree on best 4 - adult_mortality, total_expend, hiv_aids, income_comp_of_resources

# Choosing Among Models ---------------------------------------------------

#I couldn't get this to work as I wanted, and was not sure of the intent so I skipped this and went with
#the model with the four predictors above#
#-------------#
# Run a loop, and for each size `i`, extract the coefficients from `regfit_best`
# for the best model of that size, multiply them into the appropriate columns of
# the test model matric to form the predictions, and compute the test MSE.
#val_errors <- rep(NA, consider)
#for (i in 1:consider) {
#  coefi <- coef(regfit_best, id = 1)
#  pred <- test[, names(coefi)] %*% coefi
#  val_errors[1] <- mean((test$life_expectancy - pred)^2)
#}

# Find the best model
#val_errors
#which.min(val_errors)
#coef(regfit_best, consider)


# Write a prediction fuction
predict_regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# Perform best subset selection on the full data set, and select the best model.
regfit_best <- regsubsets(life_expectancy ~ ., data = life_y[,-1])
coef(regfit_best, consider)

# Use cross-validation to choose among models
#k <- 10
#set.seed(1)
#folds <- sample(1:k, nrow(life_2015), replace = TRUE)
#cv_errors <- matrix(NA, k, consider, dimnames = list(NULL, paste(1:consider)))

#predict.regsubsets = function(object, newdata, id, ...) {
#  form = as.formula(object$call[[2]])
#  mat = model.matrix(form, newdata)
#  coefi = coef(object, id = id)
#  mat[, names(coefi)] %*% coefi
#}

# Perform cross-validation
#for (j in 1:k) {
#  best_fit <- regsubsets(life_expectancy ~ ., data = life_2015[folds != j, ])
#  for (i in 1:consider) {
#    pred <- predict(best_fit, df_clean[folds == j, ], id = i)
#    cv_errors[j, i] <- mean((df_clean$life_expectancy[folds == j] - pred)^2)
#  }
#}

# Use the apply function to average over the columns of the matrix in order to
# obtain a vector for which the jth element is the cross-validation error for
# the j-variable model.
#mean_cv_errors <- apply(cv_errors, 2, mean)
#mean_cv_errors
#par(mfrow = c(1, 1))
#plot(mean_cv_errors, type = "b")

# Perform best subset selection on the full data set in order to obtain the 8-variable model
#reg_best <- regsubsets(life_expectancy ~ ., data = df_clean)
#best_model <- tidy(coef(reg_best, consider))

# Build the final model using the best subset selection results
final_model <-
  lm(
    life_expectancy ~ adult_mortality +
      total_expenditure +
      hiv_aids +
      income_composition_of_resources,
    data = life_y
)

# Final model summary
summary(final_model)
#Call:
#  lm(formula = life_expectancy ~ adult_mortality + total_expenditure + 
#       hiv_aids + income_composition_of_resources, data = life_y)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.6104  -1.7314  -0.1997   1.5992  11.1313 

#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)    
#(Intercept)                     47.105815   0.861644  54.670 < 0.0000000000000002 ***
#  adult_mortality                 -0.012747   0.001567  -8.132  0.00000000000000218 ***
#  total_expenditure                0.289133   0.047527   6.084  0.00000000201996317 ***
#  hiv_aids                        -0.910925   0.076112 -11.968 < 0.0000000000000002 ***
#  income_composition_of_resources 36.648522   1.035874  35.379 < 0.0000000000000002 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.972 on 642 degrees of freedom
#Multiple R-squared:  0.8669,	Adjusted R-squared:  0.8661 
#F-statistic:  1045 on 4 and 642 DF,  p-value: < 0.00000000000000022

#check diagnostics
par(mfrow = c(1, 1))
plot(final_model$fitted.values,life_y$life_expectancy,xlab="Predicted",ylab="Life Expectancy")
lines(c(0,90),c(0,90),col="red")

par(mfrow=c(2,2))
plot(final_model)

#check log transform if that helps with normality of life_expectancy
par(mfrow = c(1, 1))
hist(life_y$life_expectancy)
life_y$log_le <- log(life_y$life_expectancy)
hist(life_y$log_le)
#nope

######################################################
###Now let's proceed with just the best model overall###
#########################################################

#fit a model on our life_1 data set (already down to 18 vars instead of 22)
par(mfrow=c(1,2))
test.model<-lm(life_expectancy ~., life_1[,-1])
plot(test.model$fitted.values,test.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(life_1$life_expectancy,test.model$residuals,xlab="Life Expectancy",ylab="Residuals")

#make test/train splits
library(caret)
# Build test and training data sets - I updated this because I was getting errors on the predict loop
set.seed(1)
# create the training partition that is 75% of total obs
inTraining <- createDataPartition(life_1$life_expectancy, p=0.75, list=FALSE)
# create training/testing dataset
train <- life_1[inTraining,]   
test <- life_1[-inTraining,] 

#try forward selection but with no limit on predictors and all years now

# Forward
regfit_fwd_2 <- regsubsets(life_expectancy ~ ., data = train[,-1], method = "forward", nvmax = 16)

summary(regfit_fwd_2)$adjr2
summary(regfit_fwd_2)$rss
summary(regfit_fwd_2)$bic

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE<-c()
for (i in 1:16){
  predictions<-predict.regsubsets(object=regfit_fwd_2,newdata=test,id=i) 
  testASE[i]<-mean((test$life_expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:16,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(regfit_fwd_2)$rss
lines(1:16,rss/518,lty=3,col="blue")  #Dividing by n since ASE=RSS/sample size

# Backward
regfit_bwd_2 <- regsubsets(life_expectancy ~ ., data = life_1[,-1], method = "backward", nvmax = 16)
summary(regfit_bwd_2)

testASE_b<-c()
for (i in 1:16){
  predictions<-predict.regsubsets(object=regfit_bwd_2,newdata=test,id=i) 
  testASE[i]<-mean((test$life_expectancy-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:16,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(regfit_bwd_2)$rss
lines(1:16,rss/518,lty=3,col="blue")  #Dividing by n since ASE=RSS/sample size

#ASE for both seems to be around 15 at lowest... worse than our other model... 

