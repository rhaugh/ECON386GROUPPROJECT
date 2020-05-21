
####################### LINEAR REGRESSION #############################################################

Sales <- read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header=T)

M5 <- lm(Weekly_Sales~Temperature + Fuel_Price + MarkDown, Sales)
summary (M5)

confint(M5) #lower and upper bounds on 95% confidence interval

# residual analysis#
mean(M5$residuals)

hist(M5$residuals, prob=TRUE)
curve(dnorm(x, mean = 0, sd = sd(M5$residuals)), col = "darkblue", lwd = 2, add = TRUE)
plot(density(M5$residuals)) #a non-parametric density plot of residuals
library(tseries) #loads library for the J-B test
jarque.bera.test(M5$residuals)  #test for normality

######### TEST/ VALIDATION ############
Training<-subset(Sales, Test!='Test') #partition training data, E_IN
Testing<-subset(Sales, Test=='Test')#partition Testing data, E_OUT

M7<-lm(Weekly_Sales~Temperature+Fuel_Price+MarkDown, Training)
summary(M7)

confint(M7)

predictions <-predict(M7,Testing)

RMSE=sqrt(sum((predictions-Testing$Weekly_Sales)^2)/(length(Testing$Weekly_Sales)-4))
RMSE



############################## LOGISTIC REGRESSION ###################################################


Sales2 <- read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header=T)

#Logistic Regression Model
M9<- glm(MarkDown ~ Unemployment+ IsHoliday, data = Sales2, family = "binomial")
summary(M9)

library(caret) #data partioning library and other machine learning tools
library(rpart) #CART library
library(e1071) #svm library
library(randomForest) #random forest

Sales2$MarkDown<-factor(Sales2$MarkDown) #convert admit to factor (categorical) variable
Sales2$IsHoliday<-factor(Sales2$IsHoliday)#convert rank to factor (categorical) variable
Sales2$Date<-factor(Sales2$Date)
class(Sales2$Date)
View(Sales2)

### Partitioning ###
set.seed(123)

inTrain <- createDataPartition(y=Sales2$IsHoliday, p=.70, list = FALSE) 
Training2<-Sales2[inTrain,]
Testing2<-Sales2[-inTrain,]

#Training/Testing#
M_LOG<-glm(MarkDown ~ Unemployment + IsHoliday, data = Training2, family = "binomial")
summary(M_LOG)

#Confidence 
exp(cbind(M_LOG$coefficients, confint(M_LOG)))

#E_IN
confusionMatrix(table(predict(M_LOG, Training2, type="response") >= 0.5,
                      Training2$MarkDown == 1))

#E_OUT
confusionMatrix(table(predict(M_LOG, Testing2, type="response") >= 0.5,
                      Testing2$MarkDown == 1))





