df <-read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header=T)
attach(df)
View(df)

################
## LINEAR REG ##
################

## Partitioning for testing linear model ##
library(caret)
set.seed(1234) 
inTrain <- createDataPartition(y=df$Weekly_Sales, p=.70, list = FALSE) 
Training<-df[inTrain,]  
Testing<-df[-inTrain,]

## Linear Model ##
M_Linear<-lm(Weekly_Sales~MarkDown+CPI,Training)
summary(M_Linear)

## Testing ##
predictions<-predict(M_Linear, Testing)
View(predictions)

RMSE=sqrt(sum((predictions-Testing$Weekly_Sales)^2)/(length(Testing$Weekly_Sales)-3))
RMSE

################
##LOGISTIC REG##
################

## Building dummy var for logistic regression ##
summary(df)
df$GreatWeek[df$Weekly_Sales>=127654]<-1
df$GreatWeek[df$Weekly_Sales<127654]<-0
##created dummy variable "Great Week" if Weekly Sales was in 75th percentile or higher
View(df)

## Partitioning ##
library(caret)
set.seed(1234) 
inTrain <- createDataPartition(y=df$GreatWeek, p=.70, list = FALSE) 
Training<-df[inTrain,]
Testing<-df[-inTrain,]

M_Logistic<-glm(GreatWeek~Temperature+CPI,data=Training,family='binomial')
summary(M_Logistic)

## Testing ##

## convert coefficients to base e for odds-ratio interpretation
exp(cbind(M_Logistic$coefficients, confint(M_Logistic)))
## accuracy on training data
confusionMatrix(table(predict(M_Logistic, Training, type="response") >= 0.5, Training$GreatWeek == 1))
## accuracy on testing data
confusionMatrix(table(predict(M_Logistic, Testing, type="response") >= 0.5, Testing$GreatWeek == 1))

