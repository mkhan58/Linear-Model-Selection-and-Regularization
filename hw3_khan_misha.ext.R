#Misha Khan
#BANA 288 Predictive Analytics
#Spring 2021

#Homework #3: Linear Model Selection and Regularization

################################################################################
#1. Prepare the data for analysis.  The data set provided with this assignment 
#has had columns removed from the 128, so that it begins with 103 variables.  
#It has also had one row removed leaving 1993.  After reading in the data, 
#examine the data’s structure.  Note the definitions for columns 1, 2, and 3.  
#These could remain in, but for this homework remove them.  Why?  Use this 
#set of columns as the “prepared data set” for the remainder of the assignment.  

#Load data
pre.dat <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 3/hw3_vio_crime_updated.csv")
str(pre.dat, list.len = 103)
names(pre.dat)
dim(pre.dat)

#Drop columns state, community, and fold (rename to dat)
dat <- pre.dat[-c(1:3)]
dat
str(dat)
names(dat)
dim(dat)

################################################################################
#2. Use the entire prepared data set for this part.  Can we perform best 
#subsets regression to pick a “good” model?   Why or why not?  
#Explain what is happening. 

#Performing best subset regression on this data set would not be the best
#way to find a good model. Since the dataset is so large, it would
#take a long time to run because of the number of possible subsets. 

#Attempt of best subsets
#install.packages("leaps")
#library(leaps)

#regfit.full <- regsubsets(VioCrime~., data = dat, nvmax = 7, really.big = T)
#summary(regfit.full)
#1 var: PctIlleg
#2 var: racePctWhite, PctKids2Par
#3 var: racePctWhite, PctKids2Par, HousVacant
#4 var: racepctblack, PctKids2Par, PctPersDenseHous, HousVacant
#5 var: racepctblack, pctUrban, PctKids2Par, PctPersDenseHous, HousVacant
#6 var: racepctblack, PctPopUnderPov, PctKids2Par, PctWorkMom, PctPersDenseHous, HousVacant
#7 var: racepctblack, PctPopUnderPov, PctKids2Par, PctWorkMom, PctPersDenseHous,HousVacant, NumStreet

#reg.summary <- summary(regfit.full)
#names(reg.summary)
#reg.summary$rsq
#is.atomic(reg.summary)
#is.recursive(reg.summary)
#par(mar=c(1,1,1,1))
#plot(reg.summary$rsq)
 
################################################################################
#3. Use the entire prepared data set for this part.  Perform forward stepwise 
#regression to pick a “good” model by evaluating adjusted R-squared, 
#Mallows Cp, and BIC.  

regfit.fwd <- regsubsets(VioCrime ~ ., data=dat, nvmax = 100, method = "forward")
summary(regfit.fwd)
reg.summary <- summary(regfit.fwd)
#1 var: PctIlleg
#2 var: PctIlleg, FemalePctDiv
#3 var: PctIlleg, FemalePctDiv, racePctWhite
#4 var: PctIlleg, FemalePctDiv, racePctWhite, HousVacant
#5 var: PctIlleg, FemalePctDiv, racePctWhite, HousVacant, PctKids2Par
#6 var: PctIlleg, FemalePctDiv, racePctWhite, HousVacant, PctKids2Par, PctWorkMom
#7 var: PctIlleg, FemalePctDiv, racePctWhite, HousVacant, PctKids2Par, PctWorkMom, pctUrban

#Comparing best sub and forward
names(regfit.fwd)
summary(regfit.fwd)$rsq
is.atomic(regfit.fwd)
is.recursive(regfit.fwd)
plot(summary(regfit.fwd)$rsq)
#bestsub has a slightly higher rsq than forward stepwise

#Comparison Bestsubset vs Stepwise
#coef(regfit.full, 7) #racepctblack, PctPopUnderPov, PctKids2Par, PctWorkMom, PctPersDenseHous, HousVacant, NumStreet
#coef(regfit.fwd, 7) #racePctWhite, pctUrban, FemalePctDiv, PctKids2Par, PctWorkMom, PctIlleg, HousVacant
#Coefficients  are different

#Adj R-squared
par(mfrow=c(2,2))
plot(reg.summary$rsq, xlab = "Number of Variables", ylab ="R-squared")
plot(reg.summary$adjr2, xlab = "Number of Variables", 
     ylab ="Adj R-squared")
which.max(reg.summary$adjr2)  #64

#Mallows Cp
maxar2 <- which.max(reg.summary$adjr2)
points(maxar2,reg.summary$adjr2[maxar2], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(reg.summary$cp) #51

#BIC
mincp <- which.min(reg.summary$cp)
points(mincp,reg.summary$cp[mincp], col = "blue", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab ="Bayesian Info Crit")
which.min(reg.summary$bic) #15

minbic <- which.min(reg.summary$bic)
points(minbic,reg.summary$bic[minbic], col = "green", cex = 2, pch = 20)

#How many variables were in the models was selected using these criteria?  

#Forward:
#Adj R squared: 64 variables
#Mallows CP: 51 variables
#BIC: 15 variables

################################################################################
#4. Repeat question 3 for backward stepwise regression.  Is a “good” model 
#in backward different from forward stepwise?  Explain.  

regfit.bwd <- regsubsets(VioCrime ~ ., data=dat, nvmax = 100, method = "backward")
summary(regfit.bwd)
reg.summary2 <- summary(regfit.bwd)

#Adj R-squared
par(mfrow=c(2,2))
plot(reg.summary2$rsq, xlab = "Number of Variables", ylab ="R-squared")
plot(reg.summary2$adjr2, xlab = "Number of Variables", 
     ylab ="Adj R-squared")
which.max(reg.summary2$adjr2)  #58

#Mallows Cp
maxar2 <- which.max(reg.summary2$adjr2)
points(maxar2,reg.summary2$adjr2[maxar2], col = "red", cex = 2, pch = 20)
plot(reg.summary2$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(reg.summary2$cp) #49

#BIC
mincp <- which.min(reg.summary2$cp)
points(mincp,reg.summary2$cp[mincp], col = "blue", cex = 2, pch = 20)
plot(reg.summary2$bic, xlab = "Number of Variables", 
     ylab ="Bayesian Info Crit")
which.min(reg.summary2$bic) #14

minbic <- which.min(reg.summary2$bic)
points(minbic,reg.summary2$bic[minbic], col = "green", cex = 2, pch = 20)

#Backwards
#Adj R squared: 58 variables
#Mallows CP: 49 variables
#BIC: 14 variables

#Summarize what stepwise regression is providing.
#Forward stepwise starts off with adding variables with high r-squared to the model while
#backward stepwise takes off variables with the lowest r-squared to the model from the entire dataset.
#Essentially, stepwise regression tells us the ideal amount of variables in a model until
#the R-squared value goes down.
#Forward and backward generate relatively the same amount of variables in the model
#from running Adj R-squared, Mallows, and BIC.

################################################################################
#5. Use your own 6-digit identification number as a seed.  
#Randomly select approximately 50% of the rows for a training data set 
#and include the rest of the observations in a test data set.  
#Run the “all-variables-in” regression model on the training data.  
#What is the R-squared for this model on the training data?  What is the RMSE?  
#What is the RMSE when using this model to predict on the test data?  
#Is overfitting an issue? 

set.seed(123456)
train <- sample(1:nrow(dat), nrow(dat) * 0.5)
dat.train <- dat[train,]
dat.test <- dat[-train,]

reg.train <- lm(VioCrime ~ ., , data = dat.train)
summary(reg.train) 
#R-squared is 72.46% for the training data model

yhat.train <- predict(reg.train, dat.train)
yhat.train
RSS.train <- sum((dat.train$VioCrime - yhat.train)^2)
RSS.train 
MSE.train <- RSS.train/nrow(dat.train)
MSE.train 
RMSE.train <- MSE.train^0.5
RMSE.train 

#RMSE training data is 0.1212442

yhat.test <- predict(reg.train, dat.test)
yhat.test
RSS.test <- sum((dat.test$VioCrime - yhat.test)^2)
RSS.test 
MSE.test <- RSS.test/nrow(dat.test)
MSE.test 
RMSE.test <- MSE.test^0.5
RMSE.test 

#RMSE testing data is 0.1419937 

table <- matrix (c(RSS.train, MSE.train, RMSE.train,
                    RSS.test, MSE.test, RMSE.test), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table) <- c("Training", "Testing")
rownames(table) <- c("RSS", "MSE", "RMSE")
table

#From the table, we can see that the RMSE for training is 0.12 while for
#testing it is 0.14. The values are relatively close but there might be a possible
#chance of overfitting since the training data has a lower RMSE than testing data 
#(meaning that there is more bias towards training). 

################################################################################
#6. Using the all-in model from question 5 run 5-Fold and 10-Fold 
#cross-validation.  Save the MSEs and RMSEs?  Compare to question 5.   

install.packages("boot")
library(boot)

#Run glm
glm.1 <- glm(VioCrime ~ ., data = dat.train)
summary(glm.1)

RMSE.glm.1 <- ((sum(glm.1$residuals^2)/glm.1$df.residual))^0.5
RMSE.glm.1 #0.1280421

#5-Fold
cv.err.5 <- cv.glm(dat.train, glm.1, K = 5)
MSE.5 <- cv.err.5$delta[2]
MSE.5
#MSE: ~0.018
RMSE.5 <- MSE.5^0.5
RMSE.5
#RMSE: ~0.13

#10-Fold
cv.err.10 <- cv.glm(dat.train, glm.1, K = 10)
MSE.10 <- cv.err.10$delta[2]
MSE.10
#MSE: ~0.018
RMSE.10 <- MSE.5^0.5
RMSE.10 #RMSE: ~0.13

table6 <- matrix (c(MSE.5, RMSE.5,
                   MSE.10, RMSE.10), nrow = 2, ncol = 2)
options(scipen = 100)
colnames(table6) <- c("5-Fold", "10-Fold")
rownames(table6) <- c("MSE", "RMSE")
table6

#Question 5 training: 0.014 (MSE), 0.121 (RMSE)
#Question 6 5-Fold: 0.018 (MSE), 0.137 (RMSE)
#Question 6 10-Fold: 0.018(MSE), 0.137 (RMSE)

#Compared to question 5, both the RMSE and MSE increased for the training
#set when we run 5-fold and 10-fold cv. 5-Fold overall has the highest 
#RMSE and MSE values.

#Why don’t we run LOOCV here? 
#LOOCV is not recommended again because of the large data set. The process
#would be quite lengthy to compute.

################################################################################
#7.Repeat the process described on page 253 of James et al.  First set up the 
#data appropriate.  Then set up the lambda grid.  Then run “glmnet” using the 
#training data and the lambda grid.  Then use the best lambda from the glmnet 
#run to fit a ridge regression model on the training data.   Then predict y 
#for the test data using this model and compute MSE/RMSE.   

install.packages("glmnet")
library(glmnet)
install.packages("dplyr")
library(dplyr)

#Set up Y and X as matrices not dataframes
y <- dat$VioCrime
X <- model.matrix(VioCrime~., dat)[,-1]
dim(X)
head(X)

#Create grid (runs 100 RR models)
grid <- 10^seq(10,-2,length=100)

set.seed(123456)
train7 <- sample(1:nrow(X), nrow(X)/2)
X.train7 <- X[train7,]
y.train7 <- y[train7]
X.test7 <- X[-train7,]
y.test7 <- y[-train7]
dat.train7 <- dat[train7,]
dat.test7 <- dat[-train7,]

#Run  Ridge Regression on the grid set up earlier
#Run glm 
ridge.mod <- glmnet(X.train7, y.train7, alpha = 0, 
                    lambda = grid, thresh = 1e-12)

#Set up matrices to store the coefficients, predictions and errors
ridge.coeff <- matrix(0, nrow = ncol(X.train7), ncol = 100) #Coefficents for all 100 models
ridge.pred <- matrix(0,nrow = length(y.test7), ncol = 100)  #Predictions for 100 models
testerr <- matrix(0, nrow = 100, ncol = 1) #Test error

#Save values for 100 models
for (j in 1:100) {
  ridge.coeff[,j] <- ridge.mod$beta[,j] 
  ridge.pred[,j] <- predict(ridge.mod, s = grid[j],  #Predict on the test set/validation set predictions
                            newx = X.test7)
  testerr[j] <- mean((ridge.pred[,j] - y.test7)^2)  #Calculate error (difference) = aka the MSE
}

#Create best_lambda variable to assign value
best_lambda = grid[which.min(testerr)]
best_lambda #0.01

#Since we found the best lambda value for the training set (0.01)
#Rerun RR on training data with the best lambda
ridge.mod2 <- glmnet(X.train7, y.train7, alpha = 0, lamdba = best_lambda, thresh = 1e-12)
ridge.pred2 <- predict(ridge.mod2, s = best_lambda, newx = X.test7) #Predict y to find MSE/RMSE

#Find MSE and RMSE
MSE.7 <- mean((ridge.pred2 - y.test7)^2)
MSE.7 #0.01979188

RMSE.7 <- MSE.7 ^0.5
RMSE.7 #0.1406836

table7 <- matrix (c(MSE.7, RMSE.7), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table7) <- c("MSE", "RMSE")
rownames(table7) <- c("Ridge Regression")
table7
################################################################################
#8. Now run cv.glmnet to perform cross validation on the training data using 
#ridge regression.  Note you do not need “grid” here, cv.glmnet chooses that 
#automatically.  Once again pick the best lambda from glmnet to fit a ridge 
#regression model on the training data.  Then predict y for the test data 
#and compute MSE/RMSE. 

#Perform CV on training
cv.out <- cv.glmnet(X.train7, y.train7, alpha = 0)
best_lambda2 <- cv.out$lambda.min
best_lambda2 #0.02786359

#Run CV that uses the best lambda on all training data
ridge.mod.CV <- glmnet(X.train7, y.train7, alpha = 0, lambda = best_lambda2)
ridge.pred.CV <- predict(ridge.mod.CV, s= best_lambda2, newx = X.test7)

#Find MSE and RMSE
MSE.R.CV <- mean((ridge.pred.CV-y.test7)^2)
MSE.R.CV #0.01987111

RMSE.R.CV <- MSE.R.CV^0.5
RMSE.R.CV #0.1409649

table8 <- matrix (c(MSE.R.CV, RMSE.R.CV), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table8) <- c("MSE", "RMSE")
rownames(table8) <- c("CV Ridge Regression")
table8

################################################################################
#9.  Repeat question 8 using LASSO.

#Change alpha from 0 to 1 for LASSO
cv.outLAS <- cv.glmnet(X.train7, y.train7, alpha = 1)
best_lambda3 <- cv.outLAS$lambda.min
best_lambda3 #0.0006588316

ridge.mod.LAS <- glmnet(X.train7, y.train7, alpha = 1, lambda = best_lambda3)
ridge.pred.LAS <- predict(ridge.mod.LAS, s= best_lambda3, newx = X.test7)
MSE.R.LAS <- mean((ridge.pred.LAS - y.test7)^2)
MSE.R.LAS #0.02020426
RMSE.R.LAS <- MSE.R.LAS^0.5
RMSE.R.LAS #0.1421417

table9 <- matrix (c(MSE.R.LAS, RMSE.R.LAS), ncol = 2, byrow = FALSE)
options(scipen = 100)
colnames(table9) <- c("MSE", "RMSE")
rownames(table9) <- c("CV Lasso")
table9

################################################################################
#10. Based on the work in questions 5-9, what is a “fair” estimate of the MSE/RMSE. 

#MSE 0.02 - 0.04 .01899 avg

table.all <- matrix (c(MSE.train, RMSE.train, MSE.test, RMSE.test,
                       MSE.5, RMSE.5, MSE.10, RMSE.10,
                       MSE.7, RMSE.7,
                       MSE.R.CV, RMSE.R.CV,
                       MSE.R.LAS, RMSE.R.LAS
                       ), ncol = 12, nrow = 2)
options(scipen = 100)
colnames(table.all) <- c("Training - Train", "Testing - Train", "Training - Test", "Testing- Test", 
                         "5-Fold", "10-Fold",
                         "RR", "RR",
                         "CV", "CV",
                         "Lasso", "Lasso")
rownames(table.all) <- c("MSE", "RMSE")
table.all

#From the table, we can see that for all the models, the MSE have a range of 0.014 to 0.20
#whereas RMSE has a range of 0.12 to 0.14 (depending on the seed). 
#So, a fair estimate would be within these bounds.

################################################################################
#Extra (ignore)
#7. which.min(testerr) #where in the tester in the df position in tester min value is at 
#matrix grid, find element which.min testerr index of grid?
#ridge.mod$lambda[100] # position of 100, best lambda 0.01
#grid tells us lambda
#testerr tells us mse
#testerr[100]
#grid[100] #0.01
#RMSE.R.60 <- testerr[60]^0.5
#RMSE.R.60
#RMSE
#MSE 0.019 gud
#RMSE 0.14 gud
#testerr[100]
#testerr[100]^0.5 #very close to --- RMSE?
#RMSE
