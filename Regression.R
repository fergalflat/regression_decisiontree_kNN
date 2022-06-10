#Regression
library(ggplot2)

#Read in csv to data frame
df <- read.csv("data/LifeExpectancy.csv")

par(mfrow=c(1, 1))

#Plot the data frame for Life expectancy(y) against Schooling(x)
plot(LifeExpectancy ~ Schooling, data = df)

#Create a model of Life expectany against Schooling
LifeEx.Schooling = lm(LifeExpectancy ~ Schooling, data=df)

#Draw line of best fit
abline(LifeEx.Schooling)

#Summary of model
summary(LifeEx.Schooling)

#Correlation coefficient - 0.8181594
cor(df$LifeExpectancy, df$Schooling)

#Create plot matrix of the data. (Remove country as it is not numerical)
df2 <- df[,-1]
rownames(df2) <- df[,1]
pairs(df2)

#Point Prediction
newdata = data.frame((Schooling = 12))
predict(LifeEx.Schooling,newdata)

#Interval Prediction (confidence)
predict(LifeEx.Schooling, newdata, interval ="confidence")

#Interval Prediction (confidence)
predict(LifeEx.Schooling, newdata, interval ="prediction")

#Splitting the data into trainig and test data
set.seed(1)
trainingRowIndex <- sample(1:nrow(df), 0.8*nrow(df))  # row indices for training data
trainingData <- df[trainingRowIndex, ]  # model training data
testData  <- df[-trainingRowIndex, ]   # test data

# Build the model on training data
lmMod <- lm(LifeExpectancy ~ Schooling, data=trainingData)  # build the model
LifeExPred <- predict(lmMod, testData)  # predict life expectancy

#Model Summary
plot(lmMod)
summary(lmMod)

#Using BoxPlot To Check For Outliers

par(mfrow=c(1, 2))
  boxplot(df$LifeExpectancy, main="Life Expectancy", sub=paste("Outlier rows: ", boxplot.stats(df$LifeExpectancy)$out))  
boxplot(df$Schooling, main="Schooling", sub=paste("Outlier rows: ", boxplot.stats(df$Schooling)$out))  

#install.packages("e1071", dep = TRUE) 

library(e1071)  # for skewness function
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(df$LifeExpectancy), main="Density Plot: Life Expectancy", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$LifeExpectancy), 2)))  # density plot for 'Life Expectancy'

polygon(density(df$LifeExpectancy), col="red")

plot(density(df$Schooling), main="Density Plot: Schooling", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$Schooling), 2)))  # density plot for 'Schooling'

polygon(density(df$Schooling), col="red")

#Model for multiple variables
LifeEx.All <- lm(LifeExpectancy ~ ., data=df2)
summary(LifeEx.All)

#Check for homoscedasticity between life expectancy and schooling
par(mfrow=c(2,2))
plot(LifeEx.Schooling)
par(mfrow=c(1,1))

#Check for homoscedasticity between life expectancy and all others
par(mfrow=c(2,2))
plot(LifeEx.All)
par(mfrow=c(1,1))

#Plot the graph 
LifeEx.graph<-ggplot(df, aes(x=Schooling, y=LifeExpectancy))+
  geom_point()
LifeEx.graph

  #Polynomial
x <- df$Schooling
y <- df$LifeExpectancy
xsq <- x^2
xcub <- x^3
xquar <- x^4
plot(x, y, xlab="Schooling", ylab="Life Expectancy")
fit1 <- lm(y~x)

#analysis of breakdown
anova(fit1)
abline(fit1)

#higher order model: y  to x and x squared
fit2 <- lm(y~x+xsq)
anova(fit2)
xv <- seq(min(x), max(x), 0.01)
yv <- predict(fit2, list(x = xv, xsq = xv^2))
lines(xv, yv, col = "green")

#higher order model: y  to x and x cubed
fit3 <- lm(y~x+xsq+xcub)
anova(fit3)
xv <- seq(min(x), max(x), 0.01)
yv <- predict(fit3, list(x = xv, xsq = xv^2, xcub = xv^3))
lines(xv, yv, col = "red")

#higher order model: y  to x and x quad
fit4 <- lm(y~x+xsq+xcub+xquar)
anova(fit4)
xv <- seq(min(x), max(x), 0.01)
yv <- predict(fit4, list(x = xv, xsq = xv^2, xcub = xv^3, xquar = xv^4))
lines(xv, yv, col = "blue")

anova(fit1, fit2, fit3, fit4)
