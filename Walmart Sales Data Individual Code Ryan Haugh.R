###Importing Data###

df <-read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header=T)

attach(df)
### using attach allow you to just type in the variables in the code and not use identifiers

View(df)
###opens up visual data table

####   MODEL 1 Temperature    #####

Model1<- lm(Weekly_Sales ~ Temperature)
###generates the linear model for weekly sales as the Y-variable and temperature as the X-variable

summary(Model1)
###provies the summary for Model1

plot(Temperature, Weekly_Sales, main = "Temperature vs Sales")
###creates a scatter plot for a quick view of the relationship between temperature and sales

abline((lm(Weekly_Sales ~ Temperature)))
###adds a regression line to the previous scatter plot

cor(Temperature,Weekly_Sales)
###provides the correlation values for temperature and weekly sales

####    Model 2 Fuel Price    ####

Model2<- lm(Weekly_Sales ~ Fuel_Price)
###generates the linear model for weekly sales as the Y-variable and Fuel Price as the X-variable

summary(Model2)
###provides the summary for Model2

plot(Fuel_Price, Weekly_Sales, main = "Fuel Price vs Sales")
###creates a scatter plot for a quick view of the relationship between fuel price and sales

abline((lm(Weekly_Sales ~ Fuel_Price)))
###adds a regression line to the previous scatter plot

cor(Fuel_Price,Weekly_Sales)
###provides the correlation values for fuel price and weekly sales


####    MODEL 3 TEMPERATURE AND FUEL PRICE    ####

Model3<- lm(Weekly_Sales ~ Temperature+Fuel_Price)
###generates the linear model for weekly sales as the Y-variable and temperature and fuel price as the X-variables

summary(Model3)
###provides the summary for Model3

####    Model 4 CPI   ####

Model4<- lm(Weekly_Sales ~ CPI)
###generates a linear model for Weekly Sales and CPI

summary(Model4)
###provides the summary stats for Model4

cor(CPI,Weekly_Sales)
###provides the correlation value for CPI and weekly sales

####    Model 5 Unemployment Rate    ####

Model5<- lm(Weekly_Sales ~ Unemployment)
###generates a linear model for Weekly Sales and Unemployment

summary(Model5)
###provides the summary stats for Model5

cor(Unemployment,Weekly_Sales)
###provides the correlation value for CPI and Weekly Sales

####    Model 6 Unemployment Rate + CPI   ####

Model6<- lm(Weekly_Sales ~ Unemployment+CPI)
### generates a liner model for Weekly Sales and Unemployment & CPI

summary(Model6)
###provides the summary stats for Model 6

####    Model 7 Temperature Fuel Price CPI Unemployment Rate   ####

Model7<- lm(Weekly_Sales ~ Temperature+Fuel_Price+CPI+Unemployment)
###generates a linear model for four variables

summary(Model7)
###provides the summary stats for Model 7

####    Partitioning Data    ####

Training<-subset(df, Test!='Test')
###generates the training dataset

Testing<-subset(df, Test=='Test')
###generates the testing dataset

####    Rebuilding a Model with Training Data    ####

TrainModel<- lm(Weekly_Sales ~ Temperature+Fuel_Price, Training)
###generates a linear model using only training data THESE VARIABLES CAN BE CHANGED 

summary(TrainModel)
###generates the summary stats for the training model

####    Evaluating TrainModel on Test Data    ####

predictions<-predict(TrainModel, Testing)
###generates predictions for TrainModel

View(predictions)
###shows predictions for last 30 observations

RMSE=sqrt(sum((predictions-Testing$Weekly_Sales)^2)/(length(Testing$Weekly_Sales)-2))
###generates the RMSE for the predicted values: difference between true value and predicted value
RMSE
###shows the value of RMSE for the the predicted values


########### LOGISITIC REGRESSION #############

###Importing Data###

df <-read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header=T)

attach(df)
### using attach allow you to just type in the variables in the code and not use identifiers

View(df)
###opens up visual data table

####         LOGISTIC REGRESSION        ####

Model1.1<- glm(MarkDown ~ Temperature+Fuel_Price+Weekly_Sales, data = df, family = "binomial")
###generates a logistic regression model to determin if there was a markdown

summary(Model1.1)
###generates the summary stats for the Logistic Regression

signal<-predict(Model1.1, df)
###generates the predicted signals from logistic model

pred_prob<-(1/(1+exp(-signal)))
###performs a log transformation on the signal

View(pred_prob)
###allows tab view of predicted probabilites  

confint(Model1.1)
###constructs confidence intervals using log likelihood

point_conf_table<-cbind(Model1.1$coefficients, confint(Model1.1))
point_conf_table
###shows confidence intervals next to point estimates

exp(point_conf_table)
###converts the values to odds ratios using base e

####    Partitioning Data    ####

library(caret)

set.seed(1234)
###locks the see for random partitions

inTrain<- createDataPartition(y=df$MarkDown, p=.70, list = FALSE)
###creates index of random 70% observations

Training<-df[inTrain,]
###stores rows in the training set

Testing<-df[-inTrain,]
###stores rows in the testing set

####    Prediction with Regression Model    ####

M_Log<-glm(MarkDown ~ Temperature+Fuel_Price+Weekly_Sales, data = Training, family = "binomial")
###generates the Logistic Regression Equation

summary(M_Log)
###provides summary for M_Log

exp(cbind(M_Log$coefficients, confint(M_Log)))
###converts coefficients to base e for odds-ratio interpretation

confusionMatrix(table(predict(M_Log, Training, type = "response") >= 0.5,Training$MarkDown ==1))
###creates confusion matrix to look at accuracy in training data
###0.5 is the thresehold for being considered a positive 

confusionMatrix(table(predict(M_Log, Testing, type = "response") >= 0.5,Testing$MarkDown ==1))
###creates confusion matrix to look at accuracy in testing data
###0.5 is the thresehold for being considered a positive 
