# Author: 030007254.
#
# Load the data and the required packages.
load("imputed.RData")

library(e1071)
library(caret)
library(pROC)

##########################################################################
#
# Function implements out of the bag (OOB) validation for a 
# naive Bayes classifier (NBC) model. 
#
# Inputs:
#
#         data: Data set to fit our classifier too.
#         N: Number of bootstrap resamples for the test data.
#         n: Size of test data set for each resample.
#
# Output:
#
#         The average balanced accuracy, sensitivity, and specificity. 
#
#########################################################################

bayes.oob <- function(data, N = 100, n = 1000){
  
  # Initialise a matrix to hold the results 
  results <- matrix(NA, nrow = N, ncol = 3)
  
  for(i in 1:N){
  
    index <- sample(1:nrow(data),n)
  
    # Form the train and test data.
    test.data <- data[index,]
    train.data <- data[-index,]
    
    # Fit the NBC and obtain the confusion matrix.
    confu.matrix <- fit.nbc(train.data, test.data)
  
    # calculate the three measures of model quality.
    results[i,] <- confu.meas(confu.matrix)
    
    }
  
  results2 <- colSums(results)/N
  names(results2) <- c("balanced accuracy", "sensitivity", "specificity")
  
  return(results2)
  
}  
############################################################################



#########################################################################
#
# Function carries out k-fold cv for a NBC model.
#
# Inputs:
#
#         data: data set the NBC is fitted too.
#         k: number of folds.
#
# Output:
#
#        A vector of length 3, containing the average balanced accuracy, 
#        sensitivity, and specificity.
#
#######################################################################

bayes.cv <- function(data,k){
  
  # Initialise a matrix to hold results.
  results <- matrix(NA, nrow = k, ncol = 3)
  

  # Create the folds for our data.
  folds <- createFolds(1:nrow(data), k = k)

  for(i in 1:k){
  
    # Form the training data.
    train.data <- data[-folds[[i]],]
  
    # Form the test data.
    test.data <- data[folds[[i]],]
  
    # Fit the NBC and obtain the confusion matrix.
    confu.matrix <- fit.nbc(train.data, test.data)
  
    # calculate the three measures of model quality.
    results[i,] <- confu.meas(confu.matrix)
  
  }
  
  results2 <- colSums(results)/k
  names(results2) <- c("balanced accuracy", "sensitivity", "specificity")
  
  return(results2)
  
}
################################################################################



#############################################################################
#
# Auxillary function that calculates three measures of model quality.
#
# Inputs:
#
#         M: An object of type confusion matrix for a binary classifier.
#
# Output:
#
#         A vector of length 3, containing 
#         the balanced accuracy, sensitivity, and specificity.
#
################################################################################

confu.meas <- function(M){
  
  sens <- M$byClass[1]
  spec <- M$byClass[2]
  bal.acc <- M$byClass[11]
  
  return(c(bal.acc, sens, spec))
   
  
  return(results)
  
}
###############################################################################



######################################################################
#
# Auxillary function. Fits a NBC classifier and returns a confusion matrix.
#
# Inputs:
#
#        data1: data set NBC is fitted too.
#        data2: data set that predictions are made on.
#
# Output:
#
#        A confusion matrix
#
#########################################################################

fit.nbc <- function(data1, data2 = data1){
  
  # Fit the model to the training data.
  model <- naiveBayes(GOOD ~. , data = data1)
  
  # Obtain the predictions for test data.
  pred <- predict(model, newdata = data2)
  
  # Calculate the confusion matrix.
  confu.matrix <- confusionMatrix(pred, data2[,1], positive = "1")
  
  return(confu.matrix)
  
}
##########################################################################




# Load the imputed data sets into R. 
# data1 obtained using mean and mode imputation
# data.mice obtained using the mice package.
load("imputed.RData")

# The model using the data set obtianed using mean and mode imputation.

bayes.cv(data1,10)


# Now assess model quality using a ROC curve.

# Fit a Naive Bayes Classifier (NBC). 
model <- naiveBayes(GOOD~. , data = data1)

# Obtain model predictions.
pred.class <- predict(model, data1, type = "raw")[,1]

roc.curve <- roc(data1$GOOD, pred.class, ci = TRUE)

# Calculate the AUC.
auc(roc.curve)

# Calculate 95% confidence interval for AUC.
roc.curve$ci

# Caluclate 95% confidence interval for sensitivity and specificity.
ci(roc.curve, of="se")
ci(roc.curve, of="sp")

plot(roc.curve, print.auc = TRUE,
     identity.col = "red", legacy.axes = TRUE, xlab = "FPR", ylab = "TPR")


# Repeat the above for the imputed data set obtained using mice.
bayes.cv(data.mice,10)

# Now assess model quality using a ROC curve.

# Fit a Naive Bayes Classifier (NBC). 
model <- naiveBayes(GOOD~. , data = data.mice)

# Obtain model predictions.
pred.class <- predict(model, data.mice, type = "raw")[,1]

roc.curve <- roc(data.mice$GOOD, pred.class, ci = TRUE)

# Calculate the AUC.
auc(roc.curve)

# Calculate 95% confidence interval for AUC.
roc.curve$ci


plot(roc.curve, print.auc = TRUE,
     identity.col = "red", legacy.axes = TRUE, xlab = "FPR", ylab = "TPR")


