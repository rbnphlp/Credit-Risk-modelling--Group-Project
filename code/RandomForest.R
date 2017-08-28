#-------------------------------------
# Install & Load librarys
#-------------------------------------
install.packages('randomForest')
install.packages("Boruta")

library(randomForest) 
library(Boruta)       # feature selection 

#-------------------------------------
#Load Data
#-------------------------------------
setwd("FINALCODE")
getwd()

fulldata <- read.csv("dataset_modelling_nodots.csv", header=TRUE)
# MICE data - load Workspace
load("data.mice.rdata") 

#-------------------------------------
#Imputation Method
#-------------------------------------

#by mean/mode method from randomForest package

# select only rows with results, not to impute results 
mean_data <- fulldata[!is.na(fulldata$GOOD),]

mean_data$time_emp <- as.numeric(mean_data$time_emp)
mean_data$I_01 <- as.numeric(mean_data$I_01)
mean_data$I_04 <- as.numeric(mean_data$I_04)
mean_data$ER_01 <- as.numeric(mean_data$ER_01)
mean_data$ER_02 <- as.numeric(mean_data$ER_02)
mean_data$I_05 <- as.numeric(mean_data$I_05)
mean_data$S_01 <- as.numeric(mean_data$S_01)
mean_data$CA_01 <- as.numeric(mean_data$CA_01)
mean_data$S_02 <- as.numeric(mean_data$S_02)  

# actual imputation 
mean_data <- na.roughfix(mean_data)


data.mice.1$time_emp <- as.numeric(data.mice.1$time_emp)
data.mice.1$I_01 <- as.numeric(data.mice.1$I_01)
data.mice.1$I_04 <- as.numeric(data.mice.1$I_04)
data.mice.1$ER_01 <- as.numeric(data.mice.1$ER_01)
data.mice.1$ER_02 <- as.numeric(data.mice.1$ER_02)
data.mice.1$I_05 <- as.numeric(data.mice.1$I_05)
data.mice.1$S_01 <- as.numeric(data.mice.1$S_01)
data.mice.1$CA_01 <- as.numeric(data.mice.1$CA_01)
data.mice.1$S_02 <- as.numeric(data.mice.1$S_02)

# mice imputation 
mice_data <- data.mice.1

#-------------------------------------
#Split the data 
#-------------------------------------

# Creating test and train dataset 
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

mean_training_set<-CV.function(data=mean_data)[[1]]
mean_test_set<-CV.function(data=mean_data)[[2]]


mice_training_set<-CV.function(data=mice_data)[[1]]
mice_test_set<-CV.function(data=mice_data)[[2]]


#-------------------------------------
# Model 
#-------------------------------------

#run random forest on data with mean imputed data 
mean_random_forest <- randomForest(as.factor(GOOD) ~ disp_income + occ_code +	cust_age +	time_emp +
                                      res_indicator +	I_01 +	I_02 +	I_03 +	I_04 +
                                      D_01 +	ER_01 +	ER_02 +	I_05 +	D_02 +	I_06 +
                                      P_01 +	S_01 +	CA_03 +	CA_02 +	CA_01 +	S_02,
                                      data=mean_training_set,
                                      importance=TRUE,
                                      ntree=100,
                                      verbose=TRUE)

prediction <- predict(mean_random_forest, mean_test_set)
table(prediction, mean_test_set$GOOD)


#run random forest on data with mean imputed data 
mice_random_forest <- randomForest(as.factor(GOOD) ~ disp_income + occ_code +	cust_age +	time_emp +
                                     res_indicator +	I_01 +	I_02 +	I_03 +	I_04 +
                                     D_01 +	ER_01 +	ER_02 +	I_05 +	D_02 +	I_06 +
                                     P_01 +	S_01 +	CA_03 +	CA_02 +	CA_01 +	S_02,
                                   data=mice_training_set,
                                   importance=TRUE,
                                   ntree=100,
                                   verbose=TRUE)

prediction <- predict(mice_random_forest, mice_test_set)
table(prediction, mean_test_set$GOOD)


#-------------------------------------
# Feature Selection  
#-------------------------------------

# Mean data  
varImpPlot(mean_random_forest)
roughfixDataImportance <- importance(mean_random_forest)
View(roughfixDataImportance)

# Mice data  

varImpPlot(mice_random_forest)
roughfixDataImportance <- importance(mice_random_forest)
View(roughfixDataImportance)


# Boruta Algorithm 
set.seed(123)
boruta.train <- Boruta(as.factor(GOOD) ~ disp_income + occ_code +	cust_age +	time_emp +
                         res_indicator +	I_01 +	I_02 +	I_03 +	I_04 +
                         D_01 +	ER_01 +	ER_02 +	I_05 +	D_02 +	I_06 +
                         P_01 +	S_01 +	CA_03 +	CA_02 +	CA_01 +	S_02, data = fulldata.roughfix, doTrace = 2)
print(boruta.train)






