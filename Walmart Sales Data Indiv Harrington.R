##Linear Regressions Group Project

##IMPORTING THE DATA##

df <-read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header=T)
View(df)
summary(df) 
cov(df[,3:9]) 
cor(df[,3:9]) 

##Basic stats
hist(df$Weekly_Sales, prob = TRUE)
#add calibrated normal density curve to histogram
curve(dnorm(x, mean = mean(df$Weekly_Sales), sd = sd(df$Weekly_Sales)), col = "darkblue", lwd = 2, add = TRUE)


plot(density(df$Weekly_Sales)) 
pairs(df[,3:8]) 
plot(df$Weekly_Sales~df$CPI) 

##Linear Model(s)
M1<-lm(Weekly_Sales~CPI, df)
summary(M1) 
confint(M1) 
plot(df$Weekly_Sales~df$CPI) 
abline(M1$coefficients[1], M1$coefficients[2], col='blue', lwd=2) #add regression line to plot

plot(M1$fitted.values~df$CPI)
abline(M1$coefficients[1], M1$coefficients[2], col='blue', lwd=2) #add regression line to plot

##Model 2
M2<-lm(Weekly_Sales~CPI+Unemployment)
summary(M2)
plot(M2)

##Model 3
M3<-lm(Weekly_Sales~Temperature)
summary(M3)
plot(df$Weekly_Sales~df$Temperature)
abline(M2$coefficients[1], M2$coefficients[2], col='blue', lwd=2)

##Model 4
M4<-lm(Weekly_Sales~Fuel_Price)
summary(M4)
plot(df$Weekly_Sales~df$Fuel_Price)

##Model 5
M5<-lm(Weekly_Sales~MarkDown)
summary(M5)
plot(df$Weekly_Sales~df$MarkDown)

##Model 6
M6<-lm(Weekly_Sales~IsHoliday)
summary(M6)
plot(df$Weekly_Sales~df$IsHoliday)

plot(df$Weekly_Sales~df$Temperature, col=factor(df$MarkDown))

legend("topleft", legend=c("Markdown", "No MarkDown"),d
       col=c("red", "black"), pch=1, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")


library(sm)
sm.density.compare(df$Weekly_Sales, df$MarkDown)
legend("topleft", legend=c("No Markdown", "Markdown"),
       col=c("red", "green"), lty=1:2, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")


M7<-lm(Weekly_Sales~MarkDown, df)  #builds the model: Sales = B_0+B_1(Promo)+e
summary(M7)  #returns summary output from the model M2

M7<-lm(Weekly_Sales~Temperature+Fuel_Price, df) #model: Sales = B_0+B_1(Temperature)+B2(Fuel_Price)+e
summary(M7)  #returns summary output for model M7

##Final model
M8<-lm(Weekly_Sales~Temperature+Fuel_Price+CPI, df) 
summary(M8) 


hist(M8$residuals, prob = TRUE)
curve(dnorm(x, mean = mean(M8$residuals), sd = sd(M8$residuals)), col = "darkblue", lwd = 2, add=TRUE)
summary(M8$residuals)

library(tseries) 
jarque.bera.test(M8$residuals)

#Training/Testing
Training<-subset(df, df$Test!='Test') #Training
Testing<-subset(df, df$Test=='Test') #Testing

dim(Training)
dim(Testing)
View(Testing)

#Model M8 w/ Training data
M9<-lm(Weekly_Sales~Temperature+Fuel_Price+CPI, Training)
summary(M9)

predictions<-predict(M9, Testing)

RMSE=sqrt(sum((predictions-Testing$Weekly_Sales)^2)/(length(Testing$Weekly_Sales)-4))
RMSE

##Residual Standard Error=7179
##Eout=8306.825

###Logistic Regression###

#it imports the data in the basket
df <- read.csv("https://raw.githubusercontent.com/rhaugh/ECON386GROUPPROJECT/master/Walmart%20Sales%20Data%20Store%201%20Raw.csv", header = TRUE)

head(df)
summary(df)
dim(df)
View(df)

#Basic Logistic regression (Temp, Sales, holiday, Markdown)
M1.1<- glm(MarkDown ~ Temperature + Weekly_Sales + IsHoliday , data = df, family = "binomial")
summary(M1.1)

#Signal
signal<-predict(M1.1, df)

#Transformation
pred_prob<-(1/(1+exp(-signal)))
View(pred_prob)

View(predict(M1.1, df, type="response"))

df$catIsHoliday <- factor(df$IsHoliday) 
head(df) 
class(df$IsHoliday)

##Categorical variable
M1.2<-glm(MarkDown ~ Temperature + Weekly_Sales + catIsHoliday, data = df, family = "binomial")
summary(M1.2)

##Conf. Int.
confint(M1.2)  
confint.default(M1.2)

##converting values to odds-ratio interpretation using base e
exp(point_conf_table)

##Partitioning Data With Caret, Random

library(caret) 

set.seed(1234)
inTrain <- createDataPartition(y=df$MarkDown, p=.70, list = FALSE) 

#Training/Testing sets
Training<-df[inTrain,]  

Testing<-df[-inTrain,]  

##Predicting with new model

#MarkDown as factor var
M_LOG<-glm(MarkDown ~ Temperature + Weekly_Sales + catIsHoliday, data = Training, family = "binomial")
summary(M_LOG)

#Base e
exp(cbind(M_LOG$coefficients, confint(M_LOG)))

#Training Data
confusionMatrix(table(predict(M_LOG, Training, type="response") >= 0.5,
                      Training$MarkDown == 1))
#Testing Data
confusionMatrix(table(predict(M_LOG, Testing, type="response") >= 0.5,
                      Testing$MarkDown == 1))

