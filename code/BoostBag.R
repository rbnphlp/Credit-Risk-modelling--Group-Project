# Author: 030007254.

# Code carries out bagging and boosting using the adabag package.
# Bagging is carried out using decision trees. For boosting, 
# the Adaboost algorithm is used where the base learners are decision trees.

install.packages("adabag")

# Load the required packages and the imputed data sets.
library(rpart)
library(adabag)
library(pROC)

setwd("/Users/Will/St Andrews/S2/ID5059 Data Mining/P2/ID5059_P2/Workspace/")
load("imputed.RData")
load("data.mice.RData")



# Begin with bagging.
# Build a validate model using 50 trees in the ensemble.
# and 10-fold cross validation.

bag1 <- bagging(GOOD~. , data = data1, mfinal = 50)
bag.cv1 <- bagging.cv(GOOD~. , data = data1, mfinal = 50,
                    v = 10)
bag.cv1

# Calculate the predicted probabilities for each class.
pred1 <- predict.bagging(bag1, newdata = data1)

# Now calculate AUC.
roc.curve1 <- roc(data1$GOOD, pred1$prob[,2], ci = TRUE)
auc(roc.curve1)

# 95% confidence interval for AUC.
roc.curve1$ci


# Now consider the data set obtained by using the mice package.
bag2 <- bagging(GOOD~. , data = data.mice.1 , mfinal = 50)
bag.cv2 <- bagging.cv(GOOD~. , data = data.mice.1, mfinal = 50,
                      v = 10)
bag.cv2

# Calculate the predicted probabilities for each class.
pred2 <- predict.bagging(bag2, newdata = data.mice.1)

# Now calculate AUC.
roc.curve2 <- roc(data.mice.1$GOOD, pred2$prob[,2], ci = TRUE)
auc(roc.curve2)

# 95% confidence interval for AUC.
roc.curve2$ci

# Now repeat the above for boosting.
# Build and validate a model using 10 trees in the ensemble.
# Set the maximum depth for the base learner trees to be 1.
control <- rpart.control(maxdepth = 1)

boost1 <- boosting(GOOD~. , data = data1, mfinal = 10)

boost.cv1 <- boosting.cv(GOOD ~ . , data = data1,
                        v = 10, control = control)
boost.cv1

# Calculate the predicted probabilities for each class.
pred3 <- predict.boosting(boost1, newdata = data1)

# Calculate ROC curve and AUC.
roc.curve3 <- roc(data1$GOOD, pred3$prob[,2], ci = TRUE)
auc(roc.curve3)

# 95% confidence interval
roc.curve3$ci









# Now repeat for the mice imputed data set.
boost2 <- boosting(GOOD ~ . , data = data.mice.1)

control <- rpart.control(maxdepth = 1)
# Unable to validate model due to long run time.
boost.cv2 <- boosting.cv(GOOD ~ . , data = data.mice.1,
                         v = 2, control = control)

# Calculate the predicted probabilities for each class.
pred4 <- predict.boosting(boost2, newdata = data.mice.1)

# Calculate ROC curve and AUC.




