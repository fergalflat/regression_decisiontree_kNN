#Decision Trees
#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)

#https://www.kaggle.com/tejashvi14/employee-future-prediction

ER <- read.csv("data/EmployeeRetention.csv", stringsAsFactors = TRUE)
summary(ER)

#Refactor the Leave or Not variable to make it more readable
ER$LeaveOrNot <- factor(ER$LeaveOrNot,
                        levels = c("yes", "no"),
                        labels = c("yes", "no"))

str(ER)
#Tables of Payment Tier and Experience 
table(ER$PaymentTier)
table(ER$ExperienceInCurrentDomain)

#Histogram of the ages
hist(ER$Age)

# training and test split
# create a random sample for training and test data
set.seed(12345)
ER_rand <- ER[order(runif(4653)), ]

ER_train <- ER_rand[1:3653, ]
ER_test  <- ER_rand[3654:4653, ]

# check the proportion of class variable
prop.table(table(ER_train$LeaveOrNot))
prop.table(table(ER_test$LeaveOrNot))

# Build the model ( 6 variables or 3 variables. COmment out unused.)
# model <- C5.0(LeaveOrNot ~ PaymentTier+ExperienceInCurrentDomain+Age, data = ER_train)
model <- C5.0(LeaveOrNot ~ PaymentTier+ExperienceInCurrentDomain+Age, data = ER_train)

# display simple facts about the tree
model

# plot the model 
plot(model)

# display detailed information about the tree
summary(model)

## Evaluate the model
predictions <- predict(model, ER_test)

CrossTable(predictions, ER_test$LeaveOrNot,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Making some mistakes more costly than others
# create a error reduction matrix
error_reduction <- matrix(c(0, 2, 1, 0), nrow = 2)
error_reduction

# apply the  matrix to the tree
# Use this code for decision tree with 6 variables
#new_model <- C5.0(LeaveOrNot ~ PaymentTier+City+Gender+Age+Education+EverBenched, data = ER_train,
                  #costs = error_reduction)
new_model <- C5.0(LeaveOrNot ~ PaymentTier+ExperienceInCurrentDomain+Age, data = ER_train,
                  costs = error_reduction)

#Evaluate the new model
new_model_predictions <- predict(new_model, ER_test)

CrossTable(new_model_predictions, ER_test$LeaveOrNot,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#####################################################
# Model for 6 variables
#####################################################


# Build the model ( 6 variables or 3 variables. COmment out unused.)
# model <- C5.0(LeaveOrNot ~ PaymentTier+ExperienceInCurrentDomain+Age, data = ER_train)
model2 <- C5.0(LeaveOrNot ~ PaymentTier+City+Gender+Age+Education+EverBenched, data = ER_train)

# display simple facts about the tree
model2

# plot the model 
plot(model2)

# display detailed information about the tree
summary(model2)

## Evaluate the model
predictions2 <- predict(model2, ER_test)

CrossTable(predictions2, ER_test$LeaveOrNot,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Making some mistakes more costly than others
# create a error reduction matrix
error_reduction <- matrix(c(0, 1, 2, 0), nrow = 2)
error_reduction

# apply the  matrix to the tree
# Use this code for decision tree with 6 variables
new_model2 <- C5.0(LeaveOrNot ~ PaymentTier+City+Gender+Age+Education+EverBenched, data = ER_train,
costs = error_reduction)

#Evaluate the new model
new_model_predictions2 <- predict(new_model2, ER_test)

CrossTable(new_model_predictions2, ER_test$LeaveOrNot,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
