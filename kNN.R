#kNN
ER <- read.csv("data/EmployeeRetention.csv", stringsAsFactors = FALSE)
summary(ER)

#
table(ER$LeaveOrNot)

#Refactor the Leave or Not variable to make it more readable
ER$LeaveOrNot <- factor(ER$LeaveOrNot,
                         levels = c("yes", "no"),
                         labels = c("yes", "no"))

#table or proportions with more informative lables
prop.table(table(ER$LeaveOrNot))

#Summarise 3 numberic variables
summary(ER[c("PaymentTier", "Age", "ExperienceInCurrentDomain")])

# create normalization function
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))  

#Blank out unnecessary columns
ER$Education <- NULL
ER$City <- NULL
ER$JoiningYear <- NULL
ER$Gender <- NULL
ER$EverBenched <- NULL

str(ER)

#ER_n <- as.data.frame(lapply(ER[1:5], normalize))
ER_n <- as.data.frame(lapply(ER[1:3], normalize))

#get summary
summary(ER_n$ExperienceInCurrentDomain)

# create training and test data (no labels)
ER_train <- ER_n[1:3653, ]
ER_test <- ER_n[3654:4653, ]

# create labels for training and test data
ER_train_labels <- ER[1:3653, 4]
ER_test_labels <- ER[3654:4653, 4]

## Step 3: Training a model on the data ----
library(class)
predictions <- knn(train = ER_train, test =
                     ER_test, cl = ER_train_labels, k=27)

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(predictions, ER_test_labels,
           prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE)

## Try Improving model performance ----

# use the scale() function to z-score standardize a data frame
ER_z <- as.data.frame(scale(ER[-4]))

# confirm that the transformation was applied correctly
summary(ER_z$Age)

# create training and test datasets
ER_train <- ER_z[1:3653, ]
ER_test <- ER_z[3654:4653, ]

# re-classify test cases
predictions <- knn(train = ER_train, test = ER_test,
                   cl = ER_train_labels, k=27)

# Create the cross tabulation of predicted vs. actual
CrossTable(predictions, ER_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# try several different values of k
ER_train <- ER_n[1:3653, ]
ER_test <- ER_n[3654:4653, ]

# k=1
predictions <- knn(train = ER_train, test = ER_test, 
                   cl = ER_train_labels, k=1)
CrossTable(predictions, ER_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=5
predictions <- knn(train = ER_train, test = ER_test, 
                   cl = ER_train_labels, k=5)
CrossTable(predictions, ER_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=11
predictions <- knn(train = ER_train, test = ER_test, 
                   cl = ER_train_labels, k=11)
CrossTable(predictions, ER_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=15
predictions <- knn(train = ER_train, test = ER_test, 
                   cl = ER_train_labels, k=15)
CrossTable(predictions, ER_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=21
predictions <- knn(train = ER_train, test = ER_test, 
                   cl = ER_train_labels, k=21)
CrossTable(predictions, ER_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=27
predictions <- knn(train = ER_train, test = ER_test, 
                   cl = ER_train_labels, k=27)
CrossTable(predictions, ER_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

