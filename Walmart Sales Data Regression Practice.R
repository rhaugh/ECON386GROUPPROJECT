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
###provides the summary for Model2



