############################################################################################################
# Setup
############################################################################################################

library(nnet)
setwd('./')
load("data.mice.RData")





############################################################################################################
# Fix numeric columns that were mistakenly identified by R as factors with a high number of levels
############################################################################################################

data.mice.1$time_emp <- as.numeric(data.mice.1$time_emp)
data.mice.1$I_01 <- as.numeric(data.mice.1$I_01)
data.mice.1$I_04 <- as.numeric(data.mice.1$I_04)
data.mice.1$ER_01 <- as.numeric(data.mice.1$ER_01)
data.mice.1$ER_02 <- as.numeric(data.mice.1$ER_02)
data.mice.1$I_05 <- as.numeric(data.mice.1$I_05)
data.mice.1$S_01 <- as.numeric(data.mice.1$S_01)
data.mice.1$CA_01 <- as.numeric(data.mice.1$CA_01)
data.mice.1$S_02 <- as.numeric(data.mice.1$S_02)




############################################################################################################
# Check Column types
############################################################################################################

sapply(data.mice.1, class)
sapply(data.mice.1[,sapply(data.mice.1, is.factor)], nlevels)





############################################################################################################
# Remove remaining two factor columns
############################################################################################################

drops <- c("app_id", "app_date", "res_indicator", "occ_code")
data.mice.1 <- data.mice.1[ , !(names(data.mice.1) %in% drops)]







############################################################################################################
# Normalize data for nn
############################################################################################################
data.mice.1.norm <- data.mice.1
for (i in 2:length(data.mice.1.norm)) {
  data.mice.1.norm[,i] <- (  data.mice.1.norm[,i] - min(data.mice.1.norm[,i], na.rm=TRUE)  )  / (max(data.mice.1.norm[,i], na.rm=TRUE) - min(data.mice.1.norm[,i], na.rm=TRUE));
}







############################################################################################################
# Find optimal Neural Network
############################################################################################################

set.seed(567)

m <- matrix(0, ncol = 2, nrow = 0)
thisLevelOutput <<- data.frame(m)
output <<- data.frame(m)
names(thisLevelOutput)[1] <- "iteration"
names(thisLevelOutput)[2] <- "max.class.rate"

thisLevelOfComplexityMaxClassRate <<- 0

# Iterate through 10 levels of complexity
for(k in 1:10) {
    
    thisLevelOutput <<- 0 # See comment on line 81
  
    # Partition the data 30 times, as validation
    for (i in 1:30) {
          cat("i: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\ni: ", i, "\n")
    
          ## Partition data
          smp_size <- floor(0.7 * nrow(data.mice.1.norm))
          train_ind <- sample(seq_len(nrow(data.mice.1.norm)), size = smp_size)
          train <- data.mice.1.norm[train_ind, ]
          test <- data.mice.1.norm[-train_ind, ]
          
          # Must run using formula syntax as response variable
          # is a factor
          X <- train[,2:20]
          Y <- train[,1]
          
          maxClassRate <<- 0
          
          # 10 Neural Nets are made on each train/test partition for a given number of hidden layers. This is because the weights generated
          # by the Neural Net are random, therefore accuracy of the Neural Net varies. We take the highest perfoming net on each iteration of
          # train/test and store this in thisLevelOutput. thisLevelOutput will have 30 rows, one for each different train/test partition.
          # The average of these 30 is taken as the estimate of classification accuracy for a hidden layer.
          for (j in 1:10) {
            cat("j: ", j, "    i:" , i , "    k: " , k, "\n", "j: ", j, "    i:" , i , "    k: " , k, "\n", "j: ", j, "    i:" , i , "    k: " , k, "\n","j: ", j, "    i:" , i , "    k: " , k, "\n","j: ", j, "    i:" , i , "    k: " , k, "\n","j: ", j, "    i:" , i , "    k: " , k, "\n","j: ", j, "    i:" , i , "    k: " , k, "\n","j: ", j, "    i:" , i , "    k: " , k, "\n")
                # Neural Net size = 4
                nn = nnet(Y~.,
                           data=cbind(X, Y),
                           size=k,
                           MaxNWts = 10000,
                           maxit = 100)
                
                
                cfmx <- table(predict(nn, test[,2:20], type="class"),test$GOOD)
                vec <- as.vector(cfmx)
                class <- (vec[1] + vec[4]) / (vec[1] + vec[2] + vec[3] + vec[4])
                cat("class: ", class, "\n")
                if(!is.na(class) & class > maxClassRate) {
                  maxClassRate <<- class
                }
                cat("max correct classification rate this iteration: ", maxClassRate, "\n")
          }
          
          thisLevelOutput <<- rbind(thisLevelOutput, c(i, maxClassRate))
    }
          thisLevelOfComplexityMeanClassRate <<- mean(thisLevelOutput[,2])
          output <<- rbind(output, c(k, thisLevelOfComplexityMeanClassRate))
          
}

output <- data.frame(output)
names(output)[1] <- "num_hidden_layers"
names(output)[2] <- "avg_class_rate"

library(ggplot2)
ggplot(output, aes(num_hidden_layers, avg_class_rate)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks=seq(1,10,1))


