#install.packages("rpart.plot")
#install.packages("RColorBrewer")
#install.packages("rattle")
install.packages("e1071")
#install.packages("partykit")

library(partykit)
library(rpart)
require(rpart.plot)
require(RColorBrewer)
require(rattle)
require(caret)
require(e1071)

## use imputed dataset from steven 

load("imputed.RData")
data<-data1

# Remove the GOOD variable as it contains the same information as BAD
# Also remove app_id and app_date.
drops <- c("app_id", "app_date")
data <- data[ , !(names(data) %in% drops)]

## change the variables to the correct class type
data$time_emp <- as.numeric(data$time_emp)
data$I_01 <- as.numeric(data$I_01)
data$I_04 <- as.numeric(data$I_04)
data$ER_01 <- as.numeric(data$ER_01)
data$ER_02 <- as.numeric(data$ER_02)
data$I_05 <- as.numeric(data$I_05)
data$S_01 <- as.numeric(data$S_01)
data$CA_01 <- as.numeric(data$CA_01)
data$S_02 <- as.integer(data$S_02)
data$I_03 <- as.numeric(data$I_03)
data$I_06 <- as.integer(data$I_06)

str(data)
sapply(data, class)
sapply(data[,sapply(data, is.factor)], nlevels)

# Set disp_inc < 10 to NA
data$disp_income[data$disp_income<10 & data$disp_income>0] <- NA
# Set num_months since last credit seach < 0 equal to NA
data$S_02[data$S_02<0] <- NA

## split data into train and test

conf.mat.tree<-function(model=tree.fit,data=test_set,observedvals=test_set$GOOD,
                        type1=c("class","response")){
  #Input : A tree fitted model , and a test data sets
  #Purpose :To get a confusion matrix for the fitted model
  preds<-predict(model,newdata = data,type=type1)
  rparttab<-table(preds,observedvals)
  conf.mat<-confusionMatrix(rparttab,positive = "1")
  return(conf.mat)
}



prune.tree<-function(model=tree.fit){
  #Input: A fitted tree model
  #Purpose: Prune a tree
  #Output: Return a pruned tree using the min cp
  index <- which.min(model$cptable[ , "xerror"])
  tree_min <- model$cptable[index, "CP"]
  ptree <- prune(model, cp =tree_min)
  return(ptree)
}

##################
CV.function<-function(data=data){
  
  # Set seed of 567
  set.seed(567)
  
  # Store row numbers for training set: index_train
  index_train<-sample(1:nrow(data),2 / 3 * nrow(data)) ## 2/3 of the data set is used for cross validation
  # Create training set: training_set
  training_set <- data[index_train, ]
  
  # Create test set: test_set
  test_set<-data[-index_train,]
  
  
  return(list(training_set,test_set))
}



training_set<-CV.function(data=data)[[1]]
test_set<-CV.function(data=data)[[2]]

##Fit a tree using default settings###########################
default.fit<-rpart(training_set$GOOD~.,method="class",data=training_set)
fancyRpartPlot(default.fit)


##Confusion matrix for tree model with default settins
conf.mat.tree(default.fit,type1="class")

## Fit a rpart tree which is overfitted###############################
overfitted.tree<-rpart(training_set$GOOD~.,method="class",data=training_set,control=rpart.control(cp=0.001,
  xval=10,maxdepth=30))
fancyRpartPlot(overfitted.tree)
plotcp(overfitted.tree)

## Confusion matrix for tree model with
conf.mat.tree(overfitted.tree,type1="class")

# Prune the tree using smallest cp and check its confusion matrix #####################

pruned.tree<-prune.tree(model=overfitted.tree)
##confuion matrix for the pruned tree
conf.mat.tree(pruned.tree,type1="class")





#### Use mice to see if tree gets better results : ##################

setwd('./Workspace')
load("data.mice.RData")

## change the variables to the correct class type
data.mice.1$time_emp <- as.numeric(data.mice.1$time_emp)
data.mice.1$I_01 <- as.numeric(data.mice.1$I_01)
data.mice.1$I_04 <- as.numeric(data.mice.1$I_04)
data.mice.1$ER_01 <- as.numeric(data.mice.1$ER_01)
data.mice.1$ER_02 <- as.numeric(data.mice.1$ER_02)
data.mice.1$I_05 <- as.numeric(data.mice.1$I_05)
data.mice.1$S_01 <- as.numeric(data.mice.1$S_01)
data.mice.1$CA_01 <- as.numeric(data.mice.1$CA_01)
data.mice.1$S_02 <- as.integer(data.mice.1$S_02)
data.mice.1$I_03 <- as.numeric(data.mice.1$I_03)
data.mice.1$I_06 <- as.integer(data.mice.1$I_06)

# Set disp_inc < 10 to NA
data.mice.1$disp_income[data.mice.1$disp_income<10 & data.mice.1$disp_income>0] <- NA
# Set num_months since last credit seach < 0 equal to NA
data.mice.1$S_02[data.mice.1$S_02<0] <- NA

str(data.mice.1)

training_set_mice<-CV.function(data.mice.1)[[1]]
test_set_mice<-CV.function(data.mice.1)[[2]]

## Fit a tree using default settings
default.fit.mice<-rpart(training_set_mice$GOOD~.,method="class",data=training_set_mice)
fancyRpartPlot(default.fit.mice)

## Confusion matrix for tree model with default settins
conf.mat.tree(default.fit.mice,type1="class",data = test_set_mice)

## Fit a rpart tree which is overfitted 
overfitted.tree.mice<-rpart(training_set_mice$GOOD~.,
                            method="class",data=training_set_mice,
                            control=rpart.control(cp=0.001,xval=10,maxdepth=30))

fancyRpartPlot(overfitted.tree.mice)

## Confusion matrix for tree model with
conf.mat.tree(overfitted.tree.mice,type1="class",data=test_set_mice)
pruned.tree.mice<-prune.tree(model=overfitted.tree.mice)

## confuion matrix for the pruned tree
conf.mat.tree(pruned.tree.mice,type1="class",data=test_set_mice)
fancyRpartPlot(pruned.tree.mice)

