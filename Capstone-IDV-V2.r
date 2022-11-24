# Obtaining the Data
#remotes::install_github("jdmR-packages/czerlinski1999")
if (!require(remotes)) install.packages('remotes')
library(remotes)
if (!require(czerlinski1999)) install.packages('czerlinski1999')
library("czerlinski1999")
if (!require(glmnet)) install.packages('glmnet')
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(tinytex)

data("fuel")
head(fuel)

# Dropping redundant response variable
fuel <- fuel[,-7]

# Creating Pairs Plot
pairs(fuel[,c(2,3,4,5,6,7,8)])


# Defining the new variable
fuel$fuel_tax_income <- fuel$fuel_pc * (fuel$tax/100) / (1000*fuel$income)

head(arrange(fuel, fuel$fuel_tax_income))
tail(arrange(fuel, fuel$fuel_tax_income))



## Clustering

fuelmatrix <- as.matrix(fuel[,c(2,9)])
k <- kmeans(fuelmatrix, centers = 2, nstart = 25)
plot(fuelmatrix,col=k$cluster+1,main = "K-Means Clustering Results with K=2",xlab = "Population",ylab = "Income Spent on Fuel Taxes")


## Outliers



# Dropping Texas and California
fuel <- fuel[-c(37,48),]
fuel


# Modeling

## Multiple Linear Regression


# Validation set will be 20% of fuel data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = fuel$fuel_pc, times = 1, p = 0.2, list = FALSE)
trainset <- fuel[-test_index,]
temp <- fuel[test_index,]
validation <- temp

removed <- anti_join(temp, validation)
trainset <- rbind(trainset, removed)

# Cross Validation function
rmse <- function(realfuel,predictedfuel){
  dif <- sqrt(mean((realfuel - predictedfuel)^2))
  return(dif)
}

# Full Model
lm1 <- lm(fuel_pc ~ ( tax + lic_n + income + road + lic_pc),data=trainset)
summary(lm1)

# Reduced Model
lmred <- lm(fuel_pc ~ ( tax + income + lic_pc),data=trainset)
summary(lmred)


# Cross Validation statistic for Multiple Linear Regression Model
pred <- predict(lmred,newdata = validation) 
rmse(validation$fuel_pc,pred)



# Residual Plot
plot(fitted(lmred),lmred$residuals,xlab = "Fitted Values",ylab = "Residuals",main = "Residual VS Fitted Plot")
abline(0,0)


# QQ-norm plot
qqnorm(lmred$residuals,main = "Q-Q Plot for Reduced Model")



## Regularization

# Optimal Lambda Search
lambda_seq <- 10^seq(2, -2, by = -.01)
x <- data.matrix(trainset[,3:7])
y <- data.matrix(trainset[,8])

lm2 <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)

# Optimal Value of Lambda
lm2$lambda.min

# Create Model With Optimal Lambda Value
lm3 <- glmnet(x, y, alpha = 0, lambda = 12.58)

# Regularization Model Predictions
lmridgepred <- lm3$a0 + lm3$beta[1]*validation$tax + lm3$beta[2]*validation$lic_n + lm3$beta[3]*validation$income + lm3$beta[4]*validation$road +lm3$beta[5]*validation$lic_pc

# Cross Validation statistic for Regularization Model
rmse(validation$fuel_pc,lmridgepred)

