install.packages("e1071")
require(e1071)

attach(data.mice.1)
str(data.mice.1)
summary(data.mice.1)
data.mice.1$cust_age <- as.numeric(data.mice.1$cust_age)
data.mice.1$disp_income <- as.numeric(data.mice.1$disp_income)
data.mice.1$time_emp <- as.factor(data.mice.1$time_emp)
data.mice.1$ER_01 <- as.numeric(data.mice.1$ER_01)
data.mice.1$ER_02 <- as.numeric(data.mice.1$ER_02)
data.mice.1$I_01 <- as.integer(data.mice.1$I_01)
data.mice.1$I_04 <- as.integer(data.mice.1$I_04)
data.mice.1$I_05 <- as.integer(data.mice.1$I_05)

training_set_mice<-CV.function(data=data.mice.1)[[1]]
test_set_mice<-CV.function(data=data.mice.1)[[2]]

# Smoothing splines
install.packages("gam")
require(gam)
ss_reg<- gam(formula = GOOD~s(cust_age)+s(disp_income)
             +s(ER_01)+s(ER_02)+res_indicator+time_emp+I_01+
               I_02+I_03+I_04+D_01+D_02+I_05+I_06+P_01+CA_02+CA_01+S_02+CA_03
             +occ_code,family = binomial,data = training_set_mice)

confusion.mat(ss_reg,test_set_mice)

## pen reg 2D
require(mgcv)
#centering and scaling age and disposable income
pen2d.data<-data.mice.1
pen2d.data$cust_age <- scale(pen2d.data$cust_age, center = TRUE,scale=TRUE)
pen2d.data$disp_income <- scale(pen2d.data$disp_income, center = TRUE,scale=TRUE)


# different interaction term "Working Model"
pen_reg_2D_Int<- gam( GOOD~s(cust_age,disp_income,by=as.factor(res_indicator))+
                        s(ER_01)+ s(ER_02)+
                        s(I_02)+s(I_03)+I_05+s(I_06)+
                        s(D_01)+s(D_02)+s(P_01)+
                        s(CA_02)+CA_01+as.numeric(S_01)+S_02+
                        occ_code+time_emp
                      ,family = binomial,data = train_set_mice_2D)

confusion.mat(pen_reg_2D_Int,test_set_mice_2D)

# plotting ROC curve 
train_set_mice_2D$prob<-predict(object=pen_reg_2D_Int,data=train_set_mice_2D,type="response")
g<-roc(GOOD~prob,data=train_set_mice_2D)
df<- data.frame(t(coords(g,seq(0,1,0.001))))
best<-coords(g,"best")


require(ggplot2)
p <- ggplot(df)
p <- p + geom_line(aes(1-specificity, sensitivity, colour=threshold), size=3) +
  theme_bw()
p + geom_abline(intercept=0, slope=1) +
  geom_hline(yintercept=as.numeric(best[3]), colour='darkgrey', linetype='longdash') +
  
  
  geom_vline(xintercept = as.numeric(1-best[2]), colour='darkgrey', linetype='longdash') +
  scale_colour_gradient(high="red", low='white') +
  geom_line(aes(1-specificity, sensitivity), colour='blue', alpha=1/3) +
  xlab('1-Specificity (False Positive Rate)') + ylab('Sensitivity (True Positive Rate)') +
  labs(colour='Threshold')







load("holdout.RData")

#centering and scaling age and disposable income
pen2d.holdout<-data.holdout
str(pen2d.holdout)
pen2d.holdout$disp_income<-as.numeric(pen2d.holdout$disp_income)
pen2d.holdout$cust_age<-as.numeric(pen2d.holdout$cust_age)
pen2d.holdout$I_01<-as.integer(pen2d.holdout$I_01)
pen2d.holdout$I_04<-as.integer(pen2d.holdout$I_04)
pen2d.holdout$ER_01<-as.numeric(pen2d.holdout$ER_01)
pen2d.holdout$ER_02<-as.numeric(pen2d.holdout$ER_02)
pen2d.holdout$I_05<-as.integer(pen2d.holdout$I_05)

pen2d.holdout$cust_age <- scale(pen2d.holdout$cust_age, center = TRUE,scale=TRUE)
pen2d.holdout$disp_income <- scale(pen2d.holdout$disp_income, center = TRUE,scale=TRUE)

library(pROC)
train_set_mice_2D$prob<-predict(object=pen_reg_2D_Int,data=train_set_mice_2D,type="response")
g<-roc(GOOD~prob,data=train_set_mice_2D)
df<- data.frame(t(coords(g,seq(0,1,0.01))))
best<-coords(g,"best")
best
pred<-predict(object=pen_reg_2D_Int,newdata=pen2d.holdout,type="response")
str(pred)

pen2d.holdout$pred<-predict(object=pen_reg_2D_Int,newdata=pen2d.holdout,type="response")
pen2d.holdout$GOOD<-ifelse(pen2d.holdout$pred>=0.6885533,1,0)
str(pen2d.holdout$GOOD)
write.csv(pen2d.holdout,"pred.csv")

confusion.mat(pen_reg_2D_Int,pen2d.holdout)


