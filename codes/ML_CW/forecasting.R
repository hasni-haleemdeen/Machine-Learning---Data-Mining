####task 2####

##subtask 1##
#Load required libraries
library("readxl")
library("tidyverse")
library("neuralnet")
library("Metrics")
library("rsq")

#Read the Excel file containing power consumption data into a variable named 'Data'
Data <- readxl::read_excel("C:/Users/DELL/Desktop/codes(ml)/ML_CW/uow_consumption.xlsx")

#Rename the column names of Data
colnames(Data) <- c("date","18:00","19:00","20:00")

#Display the Data dataset
Data

#Display the first few rows of the Data dataset
head(Data)

#Check the data types of the columns in the Data dataset
glimpse(Data)

#Get the dimensions of the Data dataset
dim(Data)

#Check for any null values in the Data dataset
which(is.na.data.frame(Data))

#Define a function to normalize the data using min-max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Extract the power consumption values for the 20th hour
X_label <- as.data.frame(Data[,4])

#Calculate the minimum and maximum values of the power consumption data
Type_min <- min(X_label)
Type_max <- max(X_label)

#Normalize the power consumption data
X_labelNorm <- as.data.frame(lapply(X_label, normalize))

#Split the data into train and test datasets
train <- X_labelNorm[1:380,]
test <- X_labelNorm[381:470,]

#Define a function to unnormalize the data using the min-max normalization formula
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

#Define input-output matrices for t1, t2, and t3
#t1 Train and Test I/O matrix (1 input and 1 output)
T1train <- bind_cols(D_previous = lag(train,1),
                     D_current = train)

T1test <- bind_cols(D_previous = lag(test,1),
                    D_current = test)

#t2 Train and Test I/O matrix (2 input and 1 output)
T2train <- bind_cols(D_previous2 = lag(train,2),
                     D_previous = lag(train,1),
                     D_current = train)

T2test <- bind_cols(D_previous2 = lag(test,2),
                    D_previous = lag(test,1),
                    D_current = test)

#t3 Train and Test I/O matrix (3 input and 1 output)
T3train <- bind_cols(D_previous3 = lag(train,3),
                     D_previous2 = lag(train,2),
                     D_previous = lag(train,1),
                     D_current = train)

T3test <- bind_cols(D_previous3 = lag(test,3),
                    D_previous2 = lag(test,2),
                    D_previous = lag(test,1),
                    D_current = test)

#t4 Train and Test I/O matrix (4 input and 1 output)
T4train <- bind_cols(D_previous4 = lag(train, 4),
                     D_previous3 = lag(train, 3),
                     D_previous2 = lag(train, 2),
                     D_previous = lag(train, 1),
                     D_current = train)

T4test <- bind_cols(D_previous4 = lag(test, 4),
                    D_previous3 = lag(test, 3),
                    D_previous2 = lag(test, 2),
                    D_previous = lag(test, 1),
                    D_current = test)

#t7 Train and Test I/O matrix (7 input and 1 output)
T7train <- bind_cols(D_previous6 = lag(train, 7),
                     D_previous6 = lag(train, 6),
                     D_previous5 = lag(train, 5),
                     D_previous4 = lag(train, 4),
                     D_previous3 = lag(train, 3),
                     D_previous2 = lag(train, 2),
                     D_previous = lag(train, 1),
                     D_current = train)

T7test <- bind_cols(D_previous6 = lag(test, 7),
                    D_previous6 = lag(test, 6),
                    D_previous5 = lag(test, 5),
                    D_previous4 = lag(test, 4),
                    D_previous3 = lag(test, 3),
                    D_previous2 = lag(test, 2),
                    D_previous = lag(test, 1),
                    D_current = test)

#Remove missing values from T1train
T1train <- T1train[complete.cases(T1train),]

#Check the dimensions of T1train
dim(T1train)

#Remove missing values from T1test
T1test <- T1test[complete.cases(T1test),]

#Check the dimensions of T1test
dim(T1test)

#Remove missing values from T2train
T2train <- T2train[complete.cases(T2train),]

#Remove missing values from T2test
T2test <- T2test[complete.cases(T2test),]

#Remove missing values from T3train
T3train <- T3train[complete.cases(T3train),]

#Remove missing values from T3test
T3test <- T3test[complete.cases(T3test),]

#Check the contents of T3test
T3test

#Remove missing values from T4train
T4train <- T4train[complete.cases(T4train),]

#Remove missing values from T4test
T4test <- T4test[complete.cases(T4test),]

#Remove missing values from T7train
T7train <- T7train[complete.cases(T7train),]

#Remove missing values from T7test
T7test <- T7test[complete.cases(T7test),]

#Check the contents of T7test
T7test

#Rename columns for T1train and T1test
colnames(T1train) <- c("previousDay","currentDay")
colnames(T1test) <- c("previousDay","currentDay")

#Print the currentDay column of T1train
T1train$currentDay

#Rename columns for T2train and T2test
colnames(T2train) <- c("DayBefore2","previousDay","currentDay")
colnames(T2test) <- c("DayBefore2","previousDay","currentDay")

#Print the currentDay column of T2train
T2train$currentDay

#Rename columns for T3train and T3test
colnames(T3train) <- c("DayBefore3","DayBefore2","previousDay","currentDay")
colnames(T3test) <- c("DayBefore3","DayBefore2","previousDay","currentDay")

#Print the currentDay column of T3test
T3test$currentDay

#Rename columns for T4train and T4test
colnames(T4train) <- c("DayBefore4","DayBefore3","DayBefore2","previousDay","currentDay")
colnames(T4test) <- c("DayBefore4","DayBefore3","DayBefore2","previousDay","currentDay")

#Print the currentDay column of T4test
T4test$currentDay

#Rename columns for T7train and T7test
colnames(T7train) <- c("DayBefore7","DayBefore6", "DayBefore5","DayBefore4","DayBefore3","DayBefore2","previousDay","currentDay")
colnames(T7test) <- c("DayBefore7","DayBefore6", "DayBefore5","DayBefore4","DayBefore3","DayBefore2","previousDay","currentDay")

#Print the DayBefore6 column of T7test
T7test$DayBefore6

#Define and train neural network models for I/O Matrix t1
NNmodel1 <- neuralnet(currentDay ~ previousDay, hidden = 2,
                            data = T1train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t2
NNmodel2 <- neuralnet(currentDay ~ DayBefore2 + previousDay, hidden = 2,
                            data = T2train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t3
NNmodel3 <- neuralnet(currentDay ~ DayBefore3 + DayBefore2 + previousDay, hidden = 2,
                            data = T3train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t4
NNmodel4 <- neuralnet(currentDay ~ DayBefore4 + DayBefore3 + DayBefore2 + previousDay, hidden = 2,
                            data = T4train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t7
NNmodel7 <- neuralnet(currentDay ~ DayBefore7 + DayBefore6 + DayBefore5 + DayBefore4 + DayBefore3 + DayBefore2 + previousDay,
                            hidden = 2,
                            data = T7train, linear.output = TRUE)

#Define and train neural network models with changes in the hidden layer size and linear output
#Define and train neural network models for I/O Matrix t1
alterhiddenmodel1 <- neuralnet(currentDay ~ previousDay, hidden = c(3, 4),
                                 data = T1train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t2
alterhiddenmodel2 <- neuralnet(currentDay ~ DayBefore2 + previousDay, hidden = c(3, 4),
                                 data = T2train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t3
alterhiddenmodel3 <- neuralnet(currentDay ~ DayBefore3 + DayBefore2 + previousDay, hidden = c(3, 4),
                                 data = T3train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t4
alterhiddenmodel4 <- neuralnet(currentDay ~ DayBefore4 + DayBefore3 + DayBefore2 + previousDay, hidden = c(3, 4),
                                 data = T4train, linear.output = TRUE)

#Define and train neural network models for I/O Matrix t7
alterhiddenmodel7 <- neuralnet(currentDay ~ DayBefore7 + DayBefore6 + DayBefore5 + DayBefore4 + DayBefore3 + DayBefore2 + previousDay,
                                 hidden = c(3, 4),
                                 data = T7train, linear.output = TRUE)

#Define and train neural network models with changes in the linear output
#Define and train neural network models for I/O Matrix t1
alterlinearmodel1 <- neuralnet(currentDay ~ previousDay, hidden = 4,
                                 data = T1train, linear.output = FALSE)

#Define and train neural network models for I/O Matrix t2
alterlinearmodel2 <- neuralnet(currentDay ~ DayBefore2 + previousDay, hidden = 4,
                                 data = T2train, linear.output = FALSE)

#Define and train neural network models for I/O Matrix t3
alterlinearmodel3 <- neuralnet(currentDay ~ DayBefore3 + DayBefore2 + previousDay,hidden = 4, 
                                 data = T3train, linear.output = FALSE)

#Define and train neural network models for I/O Matrix t4
alterlinearmodel4 <- neuralnet(currentDay ~ DayBefore4 + DayBefore3 + DayBefore2 + previousDay,hidden = 4, 
                                 data = T4train, linear.output = FALSE)

#Define and train neural network models for I/O Matrix t7
alterlinearmodel7 <- neuralnet(currentDay ~ DayBefore7 + DayBefore6 + DayBefore5 + DayBefore4 + DayBefore3 + DayBefore2 + previousDay,
                                 hidden = 4, 
                                 data = T7train, linear.output = FALSE)

#Model for the input/output matrix of time period 1
plot(NNmodel1)

#Model for the input/output matrix of time period 2
plot(NNmodel2)

#Model for the input/output matrix of time period 3
plot(NNmodel3)

#Model for the input/output matrix of time period 4
plot(NNmodel4)

#Model for the input/output matrix of time period 7
plot(NNmodel7)

#Plot the changed models with modified hidden layers
plot(alterhiddenmodel1)
plot(alterhiddenmodel2)
plot(alterhiddenmodel3)
plot(alterhiddenmodel4)
plot(alterhiddenmodel7)

#Plot the models with linear output set to false
plot(alterlinearmodel1)
plot(alterlinearmodel2)
plot(alterlinearmodel3)
plot(alterlinearmodel4)
plot(alterlinearmodel7)

#Output the result matrix for time period 1
NNmodel1$result.matrix

#Output the result matrix for time period 2
NNmodel2$result.matrix

#Output the result matrix for time period 3
NNmodel3$result.matrix

#Output the result matrix for time period 4
NNmodel4$result.matrix

#Output the result matrix for time period 7
NNmodel7$result.matrix

#t1: Compute and store the prediction results using NNmodel1 on T1test data
dim(T1test)
T1test[1:2]
modifiedT1 <- compute(NNmodel1, T1test[1])
predict1 <- modifiedT1$net.result

#t2: Compute and store the prediction results using NNmodel2 on T2test data
dim(T2test)
T2test[1:3]
modifiedT2 <- compute(NNmodel2, T2test[1:2])
predict2 <- modifiedT2$net.result

#t3: Compute and store the prediction results using NNmodel3 on T3test data
dim(T3test)
T3test[1:4]
modifiedT3 <- compute(NNmodel3, T3test[1:3])
predict3 <- modifiedT3$net.result

#t4: Compute and store the prediction results using NNmodel4 on T4test data
dim(T4test)
T4test[1:5]
modifiedT4 <- compute(NNmodel4, T4test[1:4])
predict4 <- modifiedT4$net.result

#t7: Compute and store the prediction results using NNmodel7 on T7test data
dim(T7test)
T7test[1:8]
modifiedT7 <- compute(NNmodel7, T7test[1:7])
predict7 <- modifiedT7$net.result

#Define a function to evaluate the model predictions
evaluation <- function(pred, test, Type_min, Type_max){
  
  #Un-normalize the predictions
  prediction <- unnormalize(pred, Type_min, Type_max)
  prediction <- round(prediction, 1)
  
  #Un-normalize the test data for comparison
  expected <- unnormalize(test, Type_min, Type_max)
  expected <- round(expected, 1)
  
  #Combine the predicted and expected results in a table
  result <- cbind(prediction, expected[-1])
  colnames(result) <- c("Prediction", "Expected")
  
  #Evaluation
  rmse_result <- rmse(result$Expected, result$Prediction)
  mae_result <- mae(result$Expected, result$Prediction)
  mape_result <- mape(result$Expected, result$Prediction)
  smape_result <- smape(result$Expected, result$Prediction)
  
  
  cat("MAE",mae_result,",")
  cat("RMSE", rmse_result,",")
  cat("MAPE", mape_result,",")
  cat("SMAPE",smape_result,",")
  
}

#Evaluating results of the changed model
# Get the dimensions of the test set
dim(T1test)
# Print out the first two rows of the test set
T1test[1:2]
# Compute the model result using the changed hidden model and the first row of the test set
modifiedT1 <- compute(alterhiddenmodel1, T1test[1])
# Get the predicted normalized result from the computed model result
predict1 <- modifiedT1$net.result
# Evaluate the predicted result against the test set using the evaluation function
evaluation(predict1, T1test, Type_min, Type_max)

dim(T2test)
T2test[1:3]
modifiedT2 <- compute(alterhiddenmodel2, T2test[1:2])
predict2 <- modifiedT2$net.result
evaluation(predict2, T2test, Type_min, Type_max)

dim(T3test)
T3test[1:4]
modifiedT3 <- compute(alterhiddenmodel3, T3test[1:3])
predict3 <- modifiedT3$net.result
evaluation(predict3, T3test, Type_min, Type_max)

dim(T4test)
T4test[1:5]
modifiedT4 <- compute(alterhiddenmodel4, T4test[1:4])
predict4 <- modifiedT4$net.result
evaluation(predict4, T4test, Type_min, Type_max)

dim(T7test)
T7test[1:8]
modifiedT7 <- compute(alterhiddenmodel7, T7test[1:7])
predict7 <- modifiedT7$net.result
evaluation(predict7, T7test, Type_min, Type_max)

#Evaluating results of the changed linear output model
dim(T1test)
T1test[1:2]
modifiedT1 <- compute(alterlinearmodel1, T1test[1])
predict1 <- modifiedT1$net.result
evaluation(predict1, T1test, Type_min, Type_max)

dim(T2test)
T2test[1:3]
modifiedT2 <- compute(alterlinearmodel2, T2test[1:2])
predict2 <- modifiedT2$net.result
evaluation(predict2, T2test, Type_min, Type_max)

dim(T3test)
T3test[1:4]
modifiedT3 <- compute(alterlinearmodel3, T3test[1:3])
predict3 <- modifiedT3$net.result
evaluation(predict3, T3test, Type_min, Type_max)

dim(T4test)
T4test[1:5]
modifiedT4 <- compute(alterlinearmodel4, T4test[1:4])
predict4 <- modifiedT4$net.result
evaluation(predict4, T4test, Type_min, Type_max)

dim(T7test)
T7test[1:8]
modifiedT7 <- compute(alterlinearmodel7, T7test[1:7])
predict7 <- modifiedT7$net.result
evaluation(predict7, T7test, Type_min, Type_max)


#Prediction and comparison of the expected and predicted values for t1
# Normalized predicted values for t1
predict1
# Un-normalize predicted values for t1
unpredt1 <- unnormalize(predict1, min(X_label), max(X_label))
# Round the predicted values for t1 to one decimal place
unpredt1 <- round(unpredt1, 1)
# Display the un-normalized and rounded predicted values for t1
unpredt1

# Un-normalize expected values for t1
expectt1 <- unnormalize(T1test, min(X_label), max(X_label))
# Round the expected values for t1 to one decimal place
expectt1 <- round(expectt1, 1)
# Display the un-normalized and rounded expected values for t1
expectt1

# Combine the expected and predicted values for t1
endresult1 <- cbind(expectt1[2], unpredt1)
# Add column names to the final result for t1
colnames(endresult1) <- c("ExpectingResult", "pred")
# Display the final result for t1
endresult1

#Prediction and comparison of the expected and predicted values for t2
predict2
unpredt2 <- unnormalize(predict2, min(X_label), max(X_label))
unpredt2 <- round(unpredt2, 1)
unpredt2

expectt2 <- unnormalize(T2test, min(X_label), max(X_label))
expectt2 <- round(expectt2, 1)
expectt2

endresult2 <- cbind(expectt2[3], unpredt2)
colnames(endresult2) <- c("ExpectingResult", "pred")
endresult2

#Prediction and comparison of the expected and predicted values for t3
predict3
unpredt3 <- unnormalize(predict3, min(X_label), max(X_label))
unpredt3 <- round(unpredt3, 1)
unpredt3

expectt3 <- unnormalize(T3test, min(X_label), max(X_label))
expectt3 <- round(expectt3, 1)
expectt3

endresult3 <- cbind(expectt3[4], unpredt3)
colnames(endresult3) <- c("ExpectingResult", "pred")
endresult3

#Prediction and comparison of the expected and predicted values for t4
predict4
unpredt4 <- unnormalize(predict4,min(X_label),max(X_label))
unpredt4 <- round(unpredt4,1)
unpredt4

expectt4 <- unnormalize(T4test,min(X_label),max(X_label))
expectt4 <- round(expectt4,1)
expectt4

endresult4 <- cbind(expectt4[5],unpredt4)
colnames(endresult4) <- c("ExpectingResult","pred")
endresult4

#Prediction and comparison of the expected and predicted values for t7
predict7
unpredt7 <- unnormalize(predict7,min(X_label),max(X_label))
unpredt7 <- round(unpredt7,1)
unpredt7

expectt7 <- unnormalize(T7test,min(X_label),max(X_label))
expectt7 <- round(expectt7,1)
expectt7

endresult7 <- cbind(expectt7[8],unpredt7)
colnames(endresult7) <- c("ExpectingResult","pred")
endresult7
summary(NNmodel7$result.matrix)

#summarizing data frames
library(dplyr)
sqrt(mean((endresult4$ExpectingResult - endresult4$pred)^2))

#Root Mean Square Error (RMSE) calculations
#t1
rmse(endresult1$ExpectingResult, endresult1$pred)

#t2
rmse(endresult2$ExpectingResult, endresult2$pred)

#t3
rmse(endresult3$ExpectingResult, endresult3$pred)

#t4
rmse(endresult4$ExpectingResult, endresult4$pred)

#t7
rmse(endresult7$ExpectingResult, endresult7$pred)

#Mean Absolute Error (MAE) calculations
#t1
mae(endresult1$ExpectingResult, endresult1$pred)

#t2
mae(endresult2$ExpectingResult, endresult2$pred)

#t3
mae(endresult3$ExpectingResult, endresult3$pred)

#t4
mae(endresult4$ExpectingResult, endresult4$pred)

#t7
mae(endresult7$ExpectingResult, endresult7$pred)

#Mean Absolute Percentage Error (MAPE) calculations
#t1
mape(endresult1$ExpectingResult, endresult1$pred)

#t2
mape(endresult2$ExpectingResult, endresult2$pred)

#t3
mape(endresult3$ExpectingResult, endresult3$pred)

#t4
mape(endresult4$ExpectingResult, endresult4$pred)

#t7
mape(endresult7$ExpectingResult, endresult7$pred)

#Symmetric Mean Absolute Percentage Error (SMAPE) calculations
#t1
smape(endresult1$ExpectingResult, endresult1$pred)

#t2
smape(endresult2$ExpectingResult, endresult2$pred)

#t3
smape(endresult3$ExpectingResult, endresult3$pred)

#t4
smape(endresult4$ExpectingResult, endresult4$pred)

#t7
smape(endresult7$ExpectingResult, endresult7$pred)

library("caret")
#R-squared (R2) calculations
#t1
R2(endresult1$ExpectingResult, endresult1$pred)

#t2
R2(endresult2$ExpectingResult, endresult2$pred)

#t3
R2(endresult3$ExpectingResult, endresult3$pred)

#t4
R2(endresult4$ExpectingResult, endresult4$pred)

#t7
R2(endresult7$ExpectingResult, endresult7$pred)

#create a scatter plot to visualize predicted vs. actual values
ggplot(endresult7, aes(x = ExpectingResult, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  ggtitle("Predicted vs. Actual Values")



##subtask 2##

#Loading necessary packages
library("readxl") # For reading Excel files
library("tidyverse") # For data manipulation and visualization
library(neuralnet) # For building neural network models
library(dplyr)

#Storing the vehicle data from an Excel file
Data <- readxl::read_excel("C:/Users/DELL/Desktop/codes(ml)/ML_CW/uow_consumption.xlsx")
colnames(Data) <- c("date","18:00","19:00","20:00")

head(Data)

#Function for normalizing data using min-max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Function for un-normalizing data using min-max normalization
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

#Selecting the relevant features for input data
X_Label <- as.data.frame(Data[2:4])
X_Label

#Normalizing the input data
X_LabelNorm <- as.data.frame(lapply(X_Label, normalize))

#Splitting data into training and testing sets
train <- X_LabelNorm[1:380,]
train

test <- X_LabelNorm[381:470,]
test

#T1 I/O matrix
T1train <- bind_cols(D_previous = lag(train,1),
                     D_current = train)
dim(T1train)
T1train

T1test <- bind_cols(D_previous = lag(test,1),
                    D_current = test)
dim(T1test)

#T2 I/O matrix
T2train <- bind_cols(D_previous1 = lag(train,2),
                     D_previous = lag(train,1),
                     D_current = train)
dim(T2train)
T2train

T2test <- bind_cols(D_previous1 = lag(test,2),
                    D_previous = lag(test,1),
                    D_current = test)

dim(T2test)

#T3 I/O matrix
T3train <- bind_cols(D_previous2 = lag(train,3),
                     D_previous1 = lag(train,2),
                     D_previous = lag(train,1),
                     D_current = train)
dim(T3train)
T3train

T3test <- bind_cols(D_previous2 = lag(test,3),
                    D_previous1 = lag(test,2),
                    D_previous = lag(test,1),
                    D_current = test)

dim(T3test)

#T4 I/O matrix
T4train <- bind_cols(D_previous3 = lag(train,4),
                     D_previous2 = lag(train,3),
                     D_previous1 = lag(train,2),
                     D_previous = lag(train,1),
                     D_current = train)
dim(T4train)
T4train

T4test <- bind_cols(D_previous3 = lag(test,4),
                    D_previous2 = lag(test,3),
                    D_previous1 = lag(test,2),
                    D_previous = lag(test,1),
                    D_current = test)

dim(T4test)

#T7 I/O matrix
T7train <- bind_cols(D_previous7 = lag(train,7),
                     D_previous6 = lag(train,6),
                     D_previous5 = lag(train,5),
                     D_previous4 = lag(train,4),
                     D_previous3 = lag(train,3),
                     D_previous2 = lag(train,2),
                     D_previous = lag(train,1),
                     D_current = train)

T7test <- bind_cols(D_previous7 = lag(test,7),
                    D_previous6 = lag(test,6),
                    D_previous5 = lag(test,5),
                    D_previous4 = lag(test,4),
                    D_previous3 = lag(test,3),
                    D_previous2 = lag(test,2),
                    D_previous = lag(test,1),
                    D_current = test)

dim(T7test)

#Training and training each models
#T1
T1train <- T1train[complete.cases(T1train),]
T1train

T1test <- T1test[complete.cases(T1test),]
T1test

#T2
T2train <- T2train[complete.cases(T2train),]
T2train

T2test <- T2test[complete.cases(T2test),]
T2test


#T3
T3train <- T3train[complete.cases(T3train),]
T3train

T3test <- T3test[complete.cases(T3test),]
T3test

#T4
T4train <- T4train[complete.cases(T4train),]
T4train

T4test <- T4test[complete.cases(T4test),]
T4test

#T7
T7train <- T7train[complete.cases(T7train),]
T7train

T7test <- T7test[complete.cases(T7test),]
T7test



#adding column names
#T1
colnames(T1train) <- c("previousDay18t1","previousDay19t1","previousDay20t1",
                       "currentDay18","currentDay19","currentDay20")

colnames(T1test) <- c("previousDay18t1","previousDay19t1","previousDay20t1",
                      "currentDay18","currentDay19","currentDay20")

#removing current day 18 hr and 19 hr value
T1train <- subset(T1train, select = -c(currentDay18, currentDay19))
T1test <- subset(T1test, select = -c(currentDay18, currentDay19))

dim(T1train)

#T2
colnames(T2train) <- c("previousDay18t2","previousDay19t2","previousDay20t2",
                       "previousDay18t1","previousDay19t1","previousDay20t1",
                       "currentDay18","currentDay19","currentDay20")

colnames(T2test) <- c("previousDay18t2","previousDay19t2","previousDay20t2",
                      "previousDay18t1","previousDay19t1","previousDay20t1",
                      "currentDay18","currentDay19","currentDay20")
T2train$currentDay20

#removing current day 18 hr and 19 hr value
T2train <- subset(T2train, select = -c(currentDay18, currentDay19))
T2test <- subset(T2test, select = -c(currentDay18, currentDay19))

dim(T2train)

#T3
colnames(T3train) <- c("previousDay18t3","previousDay19t3","previousDay20t3",
                       "previousDay18t2","previousDay19t2","previousDay20t2",
                       "previousDay18t1","previousDay19t1","previousDay20t1",
                       "currentDay18","currentDay19","currentDay20")

colnames(T3test) <- c("previousDay18t3","previousDay19t3","previousDay20t3",
                      "previousDay18t2","previousDay19t2","previousDay20t2",
                      "previousDay18t1","previousDay19t1","previousDay20t1",
                      "currentDay18","currentDay19","currentDay20")

#removing current day 18 hr and 19 hr value
T3train <- subset(T3train, select = -c(currentDay18, currentDay19))
T3test <- subset(T3test, select = -c(currentDay18, currentDay19))

dim(T3train)


#T4
colnames(T4train) <- c("previousDay18t4","previousDay19t4","previousDay20t4",
                       "previousDay18t3","previousDay19t3","previousDay20t3",
                       "previousDay18t2","previousDay19t2","previousDay20t2",
                       "previousDay18t1","previousDay19t1","previousDay20t1",
                       "currentDay18","currentDay19","currentDay20")

colnames(T4test) <- c("previousDay18t4","previousDay19t4","previousDay20t4",
                      "previousDay18t3","previousDay19t3","previousDay20t3",
                      "previousDay18t2","previousDay19t2","previousDay20t2",
                      "previousDay18t1","previousDay19t1","previousDay20t1",
                      "currentDay18","currentDay19","currentDay20")

#removing current day 18 hr and 19 hr value
T4train <- subset(T4train, select = -c(currentDay18, currentDay19))
T4test <- subset(T4test, select = -c(currentDay18, currentDay19))

dim(T4train)

#T7
colnames(T7train) <- c("previousDay18t7","previousDay19t7","previousDay20t7",
                       "previousDay18t6","previousDay19t6","previousDay20t6",
                       "previousDay18t5","previousDay19t5","previousDay20t5",
                       "previousDay18t4","previousDay19t4","previousDay20t4",
                       "previousDay18t3","previousDay19t3","previousDay20t3",
                       "previousDay18t2","previousDay19t2","previousDay20t2",
                       "previousDay18t1","previousDay19t1","previousDay20t1",
                       "currentDay18","currentDay19","currentDay20")

colnames(T7test) <- c("previousDay18t7","previousDay19t7","previousDay20t7",
                      "previousDay18t6","previousDay19t6","previousDay20t6",
                      "previousDay18t5","previousDay19t5","previousDay20t5",
                      "previousDay18t4","previousDay19t4","previousDay20t4",
                      "previousDay18t3","previousDay19t3","previousDay20t3",
                      "previousDay18t2","previousDay19t2","previousDay20t2",
                      "previousDay18t1","previousDay19t1","previousDay20t1",
                      "currentDay18","currentDay19","currentDay20")

#removing current day 18 hr and 19 hr value
T7train <- subset(T7train, select = -c(currentDay18, currentDay19))
T7test <- subset(T7test, select = -c(currentDay18, currentDay19))

dim(T7train)

install.packages("neuralnet")
library(neuralnet)
#plotting NN
#NN for T1
NNmodel1 <- neuralnet(currentDay20 ~ previousDay18t1 + previousDay19t1+ previousDay20t1,
                            hidden = 4, data = T1train, linear.output = TRUE)

plot(NNmodel1)

#NN for T2
NNmodel2 <- neuralnet(currentDay20 ~ previousDay18t2+ previousDay19t2 + previousDay20t2
                            + previousDay18t1 + previousDay19t1+ previousDay20t1,
                            hidden = 4, data = T2train, linear.output = TRUE)

plot(NNmodel2)

#NN for T3
NNmodel3 <- neuralnet(currentDay20 ~ previousDay18t3 + previousDay19t3 + previousDay20t3 +
                              previousDay18t2+ previousDay19t2 + previousDay20t2 + 
                              previousDay18t1 + previousDay19t1+ previousDay20t1,
                            hidden = 4, data = T3train, linear.output = TRUE)

plot(NNmodel3)

#NN for T4
NNmodel4 <- neuralnet(currentDay20 ~ previousDay18t4 + previousDay19t4 + previousDay20t4 +
                              previousDay18t3 + previousDay19t3 + previousDay20t3 +
                              previousDay18t2+ previousDay19t2 + previousDay20t2 + 
                              previousDay18t1 + previousDay19t1+ previousDay20t1,
                            hidden = 4, data = T4train, linear.output = TRUE)

plot(NNmodel4)

#NN for T7
NNmodel7 <- neuralnet(currentDay20 ~ previousDay18t7 + previousDay19t7 + previousDay20t7 +
                              previousDay18t6 + previousDay19t6 + previousDay20t6 + 
                              previousDay18t5 + previousDay19t5 + previousDay20t5 + 
                              previousDay18t4 + previousDay19t4 + previousDay20t4 + 
                              previousDay18t3 + previousDay19t3 + previousDay20t3 + 
                              previousDay18t2 + previousDay19t2 + previousDay20t2 +
                              previousDay18t1 + previousDay19t1 + previousDay20t1,hidden = 4,
                            data = T7train, linear.output = TRUE)

plot(NNmodel7)

#create predicted results
#T1
modifiedT1 <- compute(NNmodel1,T1test[1:3])
predict1 <- modifiedT1$net.result

predict1

#T2
modifiedT2 <- compute(NNmodel2,T2test[1:6])
predict2 <- modifiedT2$net.result

predict2

#T3
modifiedT3 <- compute(NNmodel3,T3test[1:9])
predict3 <- modifiedT3$net.result

predict3

#T4
modifiedT4 <- compute(NNmodel4,T4test[1:12])
predict4 <- modifiedT4$net.result

predict4

#T7
modifiedT7 <- compute(NNmodel7,T7test[1:21])
predict7 <- modifiedT7$net.result

predict7


#Un-normalize
#T1
unpredt1 <- unnormalize(predict1,min(X_Label),max(X_Label))
unpredt1 <- round(unpredt1,1)
unpredt1

expectt1 <- unnormalize(T1test,min(X_Label),max(X_Label))
expectt1 <- round(expectt1,1)
expectt1

#T2
unpredt2 <- unnormalize(predict2,min(X_Label),max(X_Label))
unpredt2 <- round(unpredt2,1)
unpredt2

expectt2 <- unnormalize(T2test,min(X_Label),max(X_Label))
expectt2 <- round(expectt2,1)
expectt2

#T3
unpredt3 <- unnormalize(predict3,min(X_Label),max(X_Label))
unpredt3 <- round(unpredt3,1)
unpredt3

expectt3 <- unnormalize(T3test,min(X_Label),max(X_Label))
expectt3 <- round(expectt3,1)
expectt3

#T4
unpredt4 <- unnormalize(predict4,min(X_Label),max(X_Label))
unpredt4 <- round(unpredt4,1)
unpredt4

expectt4 <- unnormalize(T4test,min(X_Label),max(X_Label))
expectt4 <- round(expectt4,1)
expectt4

#T7
unpredt7 <- unnormalize(predict7,min(X_Label),max(X_Label))
unpredt7 <- round(unpredt7,1)
unpredt7

expectt7 <- unnormalize(T7test,min(X_Label),max(X_Label))
expectt7 <- round(expectt7,1)
expectt7


#Final result
#T1
endresult1 <- cbind(expectt1[4],unpredt1)
colnames(endresult1) <- c("ExpectingResult","pred")
endresult1

#T2
endresult2 <- cbind(expectt2[7],unpredt2)
colnames(endresult2) <- c("ExpectingResult","pred")
endresult2

#T3
endresult3 <- cbind(expectt3[10],unpredt3)
colnames(endresult3) <- c("ExpectingResult","pred")
endresult3

#T4
endresult4 <- cbind(expectt4[13],unpredt4)
colnames(endresult4) <- c("ExpectingResult","pred")
endresult4

#T7
endresult7 <- cbind(expectt7[22],unpredt7)
colnames(endresult7) <- c("ExpectingResult","pred")
endresult7



library(Metrics)

#RMSE
#T1
rmse(endresult1$ExpectingResult, endresult1$pred)
#T2
rmse(endresult2$ExpectingResult, endresult2$pred)
#T3
rmse(endresult3$ExpectingResult, endresult3$pred)
#T4
rmse(endresult4$ExpectingResult, endresult4$pred)
#T7
rmse(endresult7$ExpectingResult, endresult7$pred)

#MAE
#T1
mae(endresult1$ExpectingResult, endresult1$pred)
#T2
mae(endresult2$ExpectingResult, endresult2$pred)
#T3
mae(endresult3$ExpectingResult, endresult3$pred)
#T4
mae(endresult4$ExpectingResult, endresult4$pred)
#T7
mae(endresult7$ExpectingResult, endresult7$pred)

#MAPE
#T1
mape(endresult1$ExpectingResult, endresult1$pred)
#T2
mape(endresult2$ExpectingResult, endresult2$pred)
#T3
mape(endresult3$ExpectingResult, endresult3$pred)
#T4
mape(endresult4$ExpectingResult, endresult4$pred)
#T7
mape(endresult7$ExpectingResult, endresult7$pred)

#sMAPE
#T1
smape(endresult1$ExpectingResult, endresult1$pred)
#T2
smape(endresult2$ExpectingResult, endresult2$pred)
#T3
smape(endresult3$ExpectingResult, endresult3$pred)
#T4
smape(endresult4$ExpectingResult, endresult4$pred)
#T7
smape(endresult7$ExpectingResult, endresult7$pred)