getwd()
setwd("C:/Users/pragn/Desktop")

# read csv file
data <- read.csv("Training.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

Test_data <-  read.csv("Testing.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
# generate summary of data
summary(data)
colnames(data)

# drop columns
data <- subset(data,select=-c(Loan_ID))

# factor variables
data$Gender = as.factor(data$Gender)
data$Married = as.factor(data$Married)
data$Education = as.factor(data$Education)
data$Self_Employed = as.factor(data$Self_Employed)
data$Property_Area = as.factor(data$Property_Area)
data$Loan_Status = as.factor(data$Loan_Status)

# Load packages required for random forest:
library(randomForest)

# drop NA rows
data <- data[complete.cases(data),]
table(is.na(data))
summary(data)

## Create the training and test data:
n = nrow(data) # n will be ther number of obs. in data
 # We take the remaining 30% as the testing data

summary(data)
summary(Test_data)
?randomForest()
# build random forest model
rf <- randomForest(Loan_Status~., data=data, ntree=95, na.action=na.exclude, importance=T,proximity=T) 
print(rf)

ncol(data)
#optimal mtry
mtry <- tuneRF(data[-12], data$Loan_Status, ntreeTry=90,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

# build random forest model 
rf <- randomForest(Loan_Status~.,mtry=best.m, data=data, ntree=90, na.action=na.exclude, importance=T,proximity=T) 
print(rf)


# use the classifier to make predictions and create the confusion matrix:
library(caret)
predicted_values <- predict(rf, Test_data,type= "prob")
head(predicted_values)

threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 'Y', 'N') )


final_data <- cbind(Test_data, predicted_values)
final_data
colnames <- c(colnames(Test_data),"Loan_Status_N","Loan_Status_Y") # Add the new column names to the original column names 
colnames
final_data
final_data$Loan_Status <- ifelse(final_data$Y <0.5,'N' , 'Y')
write.table(final_data, file="Sample2.csv", sep=",", row.names=F, col.names = colnames(final_data)) # write the csv file of the output




