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
