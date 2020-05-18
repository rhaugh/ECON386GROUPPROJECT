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



