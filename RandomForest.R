library('tidyverse')
library('dplyr')
library('sas7bdat')
library('hms')
library('zoo')
library('highfrequency')
library('randomForest')
library('tree')
library('e1071')
if(!require(tbl2xts)) install.packages("tbl2xts")
if(!require(rmsfuns)) install.packages("rmsfuns")
rm(list=ls())
dev.off()
set.seed(123)

#Import data
load('Final.RData')

#Sort data by Tickr, Date then time
data <- data[order(data$SYM_ROOT, data$DATE, data$timeround), ]

###################################################################################################
#Model Preparation
###################################################################################################

#Filter out NA variables from initial K-rolling window
# data <- data %>% 
#   drop_na(JumpTest)

#Create y- variable
data$y <- data$JumpTest
data$y <- lead(data$y)
data$y <- as.factor(data$y)

#Drop unnecessary variables
data$TIME_M <- NULL
data$JumpTest <- NULL
data$n <- NULL


###################################################################################################
#Data Validation: Rolling Window
###################################################################################################
# #Alternative training partitioning : sequential ordering 
#Drop first 4 days
data <- data %>% 
  filter(DATE > as.Date('2019-01-04'))

#Drop after 180 days
data <- data %>% 
  filter(DATE < as.Date('2019-09-23'))


#Create list of trading days
dates <- as.data.frame(table(data$DATE))[,1]

#Create arrays to store training,validation, testing indicators 
training    <- array(dim=c(1,nrow(data),3))
validation  <- array(dim=c(1,nrow(data),3))
testing     <- array(dim=c(1,nrow(data),3))

#Creates 3 training sets: Days (1:40) & (61:100) will be training days
#First create indicators for training
t=1
for (i in list(1,61,121) ) {
  training[,,t] <- ifelse(data$DATE >= as.Date(dates[i]) & data$DATE < as.Date(dates[i+39]), 1,0)
  t=t+1
}

#Creates 3 validation sets: Days (41:48) & (100:107) will be training days
#First create indicators for validation
v=1
for (i in list(41,100,160) ) {
  validation[,,v] <-  ifelse(data$DATE >= as.Date(dates[i]) & data$DATE < as.Date(dates[i+7]), 1,0)
  v+1
}
#Creates 3 testing sets: Days (50:60) & (109:119) will be training days
#First create indicators for testing
s=1
for (i in list(50,109,168) ) {
  testing[,,s] <- ifelse(data$DATE >= as.Date(dates[i]) & data$DATE < as.Date(dates[i+10]), 1,0)
  s=s+1
}

#1st Window
train_1 <- subset(data, training[,,1] ==1)
val_1 <- subset(data,validation[,,1] ==1)
test_1 <- subset(data, testing[,,1] ==1)

#2nd Window
train_2 <- subset(data, training[,,2] ==1)
val_2 <- subset(data,validation[,,2] ==1)
test_2 <- subset(data, testing[,,2] ==1)

#3rd Window
train_3 <- subset(data, training[,,3] ==1)
val_3 <- subset(data,validation[,,3] ==1)
test_3 <- subset(data, testing[,,3] ==1)

rm(data)

###################################################################################################
## Random Forest Model
###################################################################################################
##TUNING DATA

# tune.out <- tune(randomForest, y~., data=train_1,na.action = na.omit, validation.x = val_1,
#                  ranges= list(mtry=c(11,14,19),
#                               ntree=c(100,250,500) ) )
# tune.out

#Fit model to training data
rf.best <- randomForest(y~. , data=train_1, mtry = 19, ntree=500, nodesize = 25,importance = TRUE, metric= 'Accuracy', na.action = na.omit )

#Out of sample prediction
yhat.best <- predict(rf.best, newdata=test_1, na.action = na.exclude)
#Format factors into time series numeric
test_1$yhat <- as.ts(yhat.best) - 1
test_1$y <- as.ts(test_1$y) - 1


#  Measurement statistics
true_pos_1 <- sum(ifelse(test_1$y == 1 & test_1$yhat == 1 , 1,0), na.rm = TRUE)
fals_pos_1 <- sum(ifelse(test_1$y == 0 & test_1$yhat == 1 , 1,0), na.rm = TRUE)
true_neg_1 <- sum(ifelse(test_1$y == 0 & test_1$yhat == 0 , 1,0), na.rm = TRUE)
false_neg_1<- sum(ifelse(test_1$y == 1 & test_1$yhat == 0 , 1,0),na.rm = TRUE)

# Precision 
precision_1 <- true_pos_1 / (true_pos_1 + fals_pos_1)

# Recall 
recall_1 <- true_pos_1 / (true_pos_1 + false_neg_1)

#F1 - Most important Criteria
F1_1 <- 2/ ((precision_1^-1) + (recall_1^-1)) 
F1_1
precision_1
recall_1

#Out of sample prediction
yhat.best <- predict(rf.best, newdata=test_2, na.action = na.exclude)
#Format factors into time series numeric
test_2$yhat <- as.ts(yhat.best) - 1
test_2$y <- as.ts(test_2$y) - 1


#  Measurement statistics true_pos_2 <- sum(ifelse(test_2$y == 1 & test_2$yhat == 1 , 1,0), na.rm = TRUE)
true_pos_2 <- sum(ifelse(test_2$y == 1 & test_2$yhat == 1 , 1,0), na.rm = TRUE)
fals_pos_2 <- sum(ifelse(test_2$y == 0 & test_2$yhat == 1 , 1,0), na.rm = TRUE)
true_neg_2 <- sum(ifelse(test_2$y == 0 & test_2$yhat == 0 , 1,0), na.rm = TRUE)
false_neg_2<- sum(ifelse(test_2$y == 1 & test_2$yhat == 0 , 1,0),na.rm = TRUE)

# Precision 
precision_2 <- true_pos_2 / (true_pos_2 + fals_pos_2)

# Recall 
recall_2 <- true_pos_2 / (true_pos_2 + false_neg_2)

#F1 - Most important Criteria
F1_2 <- 2/ ((precision_2^-1) + (recall_2^-1)) 
F1_2
precision_2
recall_2

F1_2
precision_2
recall_2


#Out of sample prediction
yhat.best <- predict(rf.best, newdata=test_3, na.action = na.exclude)
#Format factors into time series numeric
test_3$yhat <- as.ts(yhat.best) - 1
test_3$y <- as.ts(test_3$y) - 1

#Confusion Matrix
true_pos_3 <- sum(ifelse(test_3$y == 1 & test_3$yhat == 1 , 1,0), na.rm = TRUE)
fals_pos_3 <- sum(ifelse(test_3$y == 0 & test_3$yhat == 1 , 1,0), na.rm = TRUE)
true_neg_3 <- sum(ifelse(test_3$y == 0 & test_3$yhat == 0 , 1,0), na.rm = TRUE)
false_neg_3<- sum(ifelse(test_3$y == 1 & test_3$yhat == 0 , 1,0),na.rm = TRUE)

# Precision 
precision_3 <- true_pos_3 / (true_pos_3 + fals_pos_3)

# Recall 
recall_3 <- true_pos_3 / (true_pos_3 + false_neg_3)

#F1 - Most important Criteria
F1_3 <- 2/ ((precision_3^-1) + (recall_3^-1)) 
F1_3
precision_3
recall_3

F1_3
precision_3
recall_3



mean(precision_1,precision_2,precision_3)
mean(recall_1,recall_2,recall_3)
mean(F1_1,F1_2,F1_3)
###################################################################################################
#Random Classifier (Weighted Probability)
###################################################################################################
prob_jump <- sum(as.ts(test_1$y), na.rm=TRUE)/nrow(test_1)
test_1$benchmark <- rbinom(nrow(test_1), size=1, prob=prob_jump) 

test_1$true_pos_bench <- ifelse(test_1$y == 1 & test_1$benchmark == 1 , 1,0)
test_1$false_pos_bench <- ifelse(test_1$y == 0 & test_1$benchmark == 1 , 1,0)
test_1$true_neg_bench  <- ifelse(test_1$y == 0 & test_1$benchmark == 0 , 1,0)
test_1$false_neg_bench <- ifelse(test_1$y == 1 & test_1$benchmark == 0 , 1,0)


# Precision
precision_b <- sum(test_1$true_pos_bench,na.rm = TRUE) / ((sum(test_1$true_pos_bench,na.rm = TRUE) + sum(test_1$false_pos_bench, na.rm = TRUE)))
precision_b

#Recall
recall_b <- sum(test_1$true_pos_bench,na.rm = TRUE) / ((sum(test_1$true_pos_bench,na.rm = TRUE) + sum(test_1$false_neg_bench, na.rm = TRUE)))
recall_b 

#Accuracy
accuracy_b <- (sum(test_1$true_neg_bench,na.rm = TRUE) + sum(test_1$true_pos_bench,na.rm = TRUE)) / (sum(test_1$true_pos_bench,na.rm = TRUE) + sum(test_1$false_pos_bench, na.rm = TRUE) + sum(test_1$true_neg_bench,na.rm = TRUE) + sum(test_1$false_neg_bench, na.rm = TRUE))
accuracy_b

#Specificity 
specificity_b <- sum(test_1$true_neg_bench,na.rm = TRUE) / ((sum(test_1$true_neg_bench,na.rm = TRUE) + sum(test_1$false_neg_bench, na.rm = TRUE)))
specificity_b

#F1 - Most important Criteria
F1_b <- 2/ ((precision_b^-1) + (recall_b^-1)) 
F1_b




