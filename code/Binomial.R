
install.packages("party")

# run the imputation code for steven####

#use data1 as the main data
str(data1)

data<-data1
##delete the unwanted columns
# Remove variables that are irrelevant to the analysis.
data$BAD <- NULL
data$app_id <- NULL
data$app_date<- NULL

as.integer()

## Change the variables into the right classes
# Change variables so they have the right data type.
data$S_01 <- factor(data$S_01)
data$S_02 <- factor(data$S_02)
data$P_01 <- as.numeric(data$P_01)
data$CA_01 <- factor(data$CA_01)
data$CA_02 <- as.numeric(data$CA_02)
data$I_02 <- as.numeric(data$I_02)
data$D_01 <- as.numeric(data$D_01)
data$D_02 <- as.numeric(data$D_02)
data$I_03<-as.numeric(data$I_03)
data$I_05<-as.numeric(data$I_05)
data$I_01<-as.numeric(data$I_01)
data$ER_02<-as.numeric(data$ER_02)
data$I_04<-as.numeric(data$I_04)
### Initial Data Prep :#######################

library(caret)
library(car)
library(MASS)
library(pROC)
library(ggplot2)
library(party)
#########################################

## Take a model and return its preditctions as confusion matrix
confusion.mat<-function(model=binommodel,testdata=test_set){
  # Input : A fitted binomial model using a training set ,and test set for predictions
  # Purpose : To look at sensitivity ,accuracy , and specifity of the model before and
  # after using the best cut off from ROC curves
  #Output : A confusion matrix using the mean cutoff and best_cutoff values
  
  predictions<-predict(model,testdata,type="response")
  predcutoff<-ifelse(predictions>mean(predictions),1,0)
  predtab<-table(testdata$GOOD,predcutoff)
  ## get a confision matrix
  confusionmatrix<-confusionMatrix(predcutoff,testdata$GOOD,positive="1")
  
  ##
  ## Get the ROC curves
  
  g <- roc(testdata$GOOD,predictions, data = testdata)
  df<- data.frame(t(coords(g, seq(0, 1, 0.01))))
  p <- ggplot(df)
  p <- p + geom_line(aes(1-specificity, sensitivity, colour=threshold), size=3) +
    theme_bw()
  p + geom_abline(intercept=0, slope=1)
  
  ## get the best values for the curve
  best<-coords(g,"best")
  
  ##Make predictions on te best cutoff
  pred_best_cutoff<-ifelse(predictions>=best[1],1,0)
  best.confusionmatrix<-confusionMatrix(pred_best_cutoff,testdata$GOOD,positive="1")
  ## GEt the Area under the Curve for the model
  AUC<-auc(g)
  return(list(confusionmatrix,best.confusionmatrix,AUC))  
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
  
  
  return(list(training_set,test_set))}



training_set<-CV.function(data=data)[[1]]
test_set<-CV.function(data=data)[[2]]

## Fit the binomial I_03 as numeric to get rid of warning glm.fit: fitted probabilities numerically 0 or 1 occurred is a warning

binommodel<-glm(training_set$GOOD~.,family=binomial(link="logit"),data=training_set)
summary(binommodel)

## Fit a model using an anova for significance of predictors#################
anova.binomodel<-glm(training_set$GOOD~disp_income+occ_code+I_02+
                       D_01+ER_01+ER_02+D_02+I_06
                     +P_01+S_01+CA_02+CA_01+S_02
                     ,family=binomial(link="logit"),data=training_set)
summary(anova.binomodel)
##Fit a quasi-binomial model function in R#####################
quasi.binom<-glm(training_set$GOOD~.,family=quasibinomial(link = "logit"),data=training_set)
summary(quasi.binom)

##confusion matrix for quasi binom 
confusion.mat(model=quasi.binom)


#### Fit a binomial wtih clolgclog
binom.log<-glm(training_set$GOOD~.,family=binomial(link="cloglog"),data=training_set)

confusion.mat(binom.log,test_set)
# Use step wise selection , to fit a model
reduced_model<-stepAIC(binommodel,direction = "both")

## fit a binomial probit model:

binom.probit<-glm(training_set$GOOD~.,family=binomial(link="probit"),data=training_set)

confusion.mat(binom.probit)

## AUC for binomodel
auc.binom<-confusion.mat()[[3]]
auc.anova<-confusion.mat(anova.binomodel,test_set)[[3]]
auc.reduced<-confusion.mat(reduced_model,test_set)[[3]]
auc.quasi<-confusion.mat(quasi.binom,test_set)[[3]]


confusion.mat(anova.binomodel,test_set)
auc.binom
auc.anova
auc.reduced
auc.quasi


## Reun the above with MICE Package dataset ##########################
str(data.mice.1)

data.mice.1$S_01 <- factor(data.mice.1$S_01)
data.mice.1$S_02 <- factor(data.mice.1$S_02)
data.mice.1$P_01 <- as.numeric(data.mice.1$P_01)
data.mice.1$CA_01 <- factor(data.mice.1$CA_01)
data.mice.1$CA_02 <- as.numeric(data.mice.1$CA_02)
data.mice.1$I_02 <- as.numeric(data.mice.1$I_02)
data.mice.1$D_01 <- as.numeric(data.mice.1$D_01)
data.mice.1$D_02 <- as.numeric(data.mice.1$D_02)
data.mice.1$I_03<-as.numeric(data.mice.1$I_03)
data.mice.1$I_05<-as.numeric(data.mice.1$I_05)
data.mice.1$I_01<-as.numeric(data.mice.1$I_01)
data.mice.1$ER_02<-as.numeric(data.mice.1$ER_02)
data.mice.1$I_04<-as.numeric(data.mice.1$I_04)
##change the structure to all numeric :

##split into training and test
training_set_mice<-CV.function(data.mice.1)[[1]]
test_set_mice<-CV.function(data.mice.1)[[2]]

str(training_set_mice)
str(test_set_mice)

## Fit the binomial I_03 as numeric to get rid of warning glm.fit: fitted probabilities numerically 0 or 1 occurred is a warning
str(data.mice.1)
binommodel_mice<-glm(training_set_mice$GOOD~.,family=binomial(link="logit"),data=training_set_mice)
summary(binommodel_mice)

## Fit a model using an anova for significance of predictors#################
anova.binomodel.mice<-glm(training_set_mice$GOOD~disp_income+occ_code+I_02+
                       D_01+ER_01+ER_02+D_02+I_06
                     +P_01+S_01+CA_02+CA_01+S_02
                     ,family=binomial(link="logit"),data=training_set_mice)
summary(anova.binomodel.mice)
##Fit a quasi-binomial model function in R#####################
quasi.binom.mice<-glm(training_set_mice$GOOD~.,family=quasibinomial(link = "logit"),data=training_set_mice)
summary(quasi.binom.mice)

##confusion matrix for quasi binom 
confusion.mat(model=quasi.binom,test_set_mice)


#### Fit a binomial wtih clolgclog
binom.log.mice<-glm(training_set_mice$GOOD~.,family=binomial(link="cloglog"),data=training_set_mice)

confusion.mat(binom.log.mice,test_set_mice)
# Use step wise selection , to fit a model
reduced_model.mice<-stepAIC(binommodel_mice,direction = "both")




## AUC for binomodel
auc.binom.mice<-confusion.mat(binommodel_mice,test_set_mice)[[3]]
auc.anova.mice<-confusion.mat(anova.binomodel.mice,test_set_mice)[[3]]
auc.reduced.mice<-confusion.mat(reduced_model.mice,test_set_mice)[[3]]
auc.quasi.mice<-confusion.mat(quasi.binom.mice,test_set_mice)[[3]]

auc.binom.mice
auc.anova.mice
auc.reduced.mice
auc.quasi.mice
confusion.mat(reduced_model.mice,test_set_mice)
