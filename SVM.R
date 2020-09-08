library('tidyverse')
library('dplyr')
library('sas7bdat')
library('hms')
library('zoo')
library('highfrequency')
library('randomForest')
library('tree')
library('e1071')
library('DMwR')
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
#Model Preparation: NOrmalise Numeric
###################################################################################################
#Normalise using Z-score 

#Select numeric col. only
list <- names(data)
list <- list[5:23]
norm <- data %>% 
  select(all_of(list))

#Create normalise function
norm_z <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

#Normalise each function : list is each col. name specified NOTBYDAY
norm <- data %>%
  group_by(SYM_ROOT, DATE) %>%
  mutate_at(list, norm_z ) %>%
  ungroup()

#Create y- variable
data$y <- data$JumpTest
data$y <- lead(data$y)
data$y <- as.factor(data$y)
data$JumpTest <- NULL

data <- cbind(norm, data$y)
names(data)[names(data) == 'data$y'] <- 'y'

#Drop unnecessary variables
data$TIME_M <- NULL
data$JumpTest <- NULL
data$n <- NULL
data$date_time <- NULL
rm(norm)


#Dummy Variable identifiers
data <- data %>% 
  mutate(APPL = as.factor(ifelse(SYM_ROOT == 'AAPL',1 , 0)))

data <- data %>% 
  mutate(FB = as.factor(ifelse(SYM_ROOT == 'FB',1 , 0)))

data <- data %>% 
  mutate(MSFT = as.factor(ifelse(SYM_ROOT == 'MSFT',1 , 0)))

data <- data %>% 
  mutate(GOOG = as.factor(ifelse(SYM_ROOT == 'GOOG',1 , 0)))

data$SYM_ROOT <- NULL

#Dummy time features
data$char <- as.character(data$timeround)
data <- data %>% 
  mutate(hour09 = as.numeric(ifelse(substring(char,1,2) == '09',1,0)))
data <- data %>% 
  mutate(hour10 = as.numeric(ifelse(substring(char,1,2) == '10',1,0)))
data <- data %>% 
  mutate(hour11 = as.numeric(ifelse(substring(char,1,2) == '11',1,0)))
data <- data %>% 
  mutate(hour12 = as.numeric(ifelse(substring(char,1,2) == '12',1,0)))
data <- data %>% 
  mutate(hour13 = as.numeric(ifelse(substring(char,1,2) == '13',1,0)))
data <- data %>% 
  mutate(hour14 = as.numeric(ifelse(substring(char,1,2) == '14',1,0)))
data <- data %>% 
  mutate(hour15 = as.numeric(ifelse(substring(char,1,2) == '15',1,0)))
data <- data %>% 
  mutate(hour15 = as.numeric(ifelse(substring(char,1,2) == '16',1,)))
data$char <- NULL

###################################################################################################
#Data Training and Validation: Rolling Window
###################################################################################################
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
  v=v+1
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

rm(data, testing,training,validation)


train_1$DATE <- NULL
smote_1 <- as.data.frame(train_1)

train_2$DATE <- NULL
smote_2 <- as.data.frame(train_2)

train_3$DATE <- NULL
smote_3 <- as.data.frame(train_3)

smote_1$timeround <- NULL
smote_2$timeround <- NULL
smote_3$timeround <- NULL
train_1$timeround <- NULL
train_2$timeround <- NULL
train_3$timeround <- NULL




train_1 <- SMOTE(y ~ . , smote_1, perc.over = 300, perc.under = 400, k = 5)
train_2 <- SMOTE(y ~ . , smote_2, perc.over = 300, perc.under = 400, k = 5)
train_3 <- SMOTE(y ~ . , smote_3, perc.over = 300, perc.under = 400, k = 5)


rm(smote_1,smote_2,smote_3,dates)
###################################################################################################
#Tuning Parameters
###################################################################################################
# svm_tuning <- vector(mode = "list", length = 16)
# i = 1
# for (g in c(.001,.005,.01,0.5) ){
#   for (c in c(.01,.05,.1,0.5) ){
#     svm_tuning[[i]]  <- svm(y~ ., data=train_1, kernel="radial", gamma=g, cost=c, na.action = na.omit)
#     i= i+1
#   }
# }
# #Create empty dataframe to fill
# yhat <- data.frame(matrix(ncol = 16, nrow = nrow(val_1)))
# 
# for (i in c(1:16)){
#   yhat[,i]  <- predict(svm_tuning[[i]], newdata=val_1, na.action = na.exclude)
# }
# #Create data frame of form: y, yhat1,yhat2, ...
# validation <- cbind(val_1$y,yhat)
# 
# 
# #Create empty lists to fill
# F1  <- vector(mode = 'list', length = 16)
# precision <- vector(mode = 'list', length = 16)
# recall <- vector(mode = 'list', length = 16)
# true_pos  <- vector(mode = 'list', length = 16)
# fals_neg  <- vector(mode = 'list', length = 16)
# fals_pos  <- vector(mode = 'list', length = 16)
# true_neg <- vector(mode = 'list', length = 16)
# 
# 
# for (i in c(1:16)){
#   #Format factors into time series numeric
#   #  Measurement statistics
#   true_pos[[i]] <- sum(ifelse(validation[,1] == 1 & validation[,i] == 1 , 1,0), na.rm = TRUE)
#   fals_pos[[i]] <- sum(ifelse(validation[,1] == 0 & validation[,i] == 1 , 1,0), na.rm = TRUE)
#   true_neg[[i]] <- sum(ifelse(validation[,1] == 0 & validation[,i] == 0 , 1,0), na.rm = TRUE)
#   fals_neg[[i]] <-  sum(ifelse(validation[,1] == 1 & validation[,i] == 0 , 1,0), na.rm = TRUE)
# 
# 
#   # Precision
#   precision[[i]] <- true_pos[[i]] / (true_pos[[i]] + fals_pos[[i]])
#   #Recall
#   recall[[i]] <- true_pos[[i]] / (true_pos[[i]] + fals_neg[[i]])
#   #F1 - Most important Criteria
#   F1[[i]] <- 2/ ((precision[[i]]^-1) + (recall[[i]]^-1))
# }

###################################################################################################
#Support Vector Machine Model
###################################################################################################

#Fit and Predict Model
svm_tuning_1  <- svm(y~ ., data=train_1, kernel="radial", gamma=.001, cost=.1, na.action = na.omit)
yhat.best <- predict(svm_tuning_1, newdata=test_1, na.action = na.exclude)

#Format factors into time series numeric
test_1$y <- as.numeric(levels(test_1$y))[test_1$y]
test_1$yhat <- as.numeric(yhat.best) - 1

#  Measurement statistics
true_pos_1 <- sum(ifelse(test_1$y == 1 & test_1$yhat == 1 , 1,0), na.rm = TRUE)
fals_pos_1 <- sum(ifelse(test_1$y == 0 & test_1$yhat == 1 , 1,0), na.rm = TRUE)
true_neg_1 <- sum(ifelse(test_1$y == 0 & test_1$yhat == 0 , 1,0), na.rm = TRUE)
false_neg_1<- sum(ifelse(test_1$y == 1 & test_1$yhat == 0 , 1,0),na.rm = TRUE)

# Precision
precision_1 <- true_pos_1 / (true_pos_1 + fals_pos_1)
#Recall
recall_1 <- true_pos_1 / (true_pos_1 + false_neg_1)
#F1 - Most important Criteria
F1_1 <- 2/ ((precision_1^-1) + (recall_1^-1)) 

#Fit and Predict Model
svm_tuning_2  <- svm(y~ ., data=train_2, kernel="radial", gamma=.001, cost=.1, na.action = na.omit)
yhat.best <- predict(svm_tuning_2, newdata=test_2, na.action = na.exclude)

#Format factors into time series numeric
test_2$y <- as.numeric(levels(test_2$y))[test_2$y]
test_2$yhat <- as.numeric(yhat.best) - 1

#  Measurement statistics
true_pos_2 <- sum(ifelse(test_2$y == 1 & test_2$yhat == 1 , 1,0), na.rm = TRUE)
fals_pos_2 <- sum(ifelse(test_2$y == 0 & test_2$yhat == 1 , 1,0), na.rm = TRUE)
true_neg_2 <- sum(ifelse(test_2$y == 0 & test_2$yhat == 0 , 1,0), na.rm = TRUE)
false_neg_2<- sum(ifelse(test_2$y == 1 & test_2$yhat == 0 , 1,0),na.rm = TRUE)

# Precision
precision_2 <- true_pos_2 / (true_pos_2 + fals_pos_2)
#Recall
recall_2 <- true_pos_2 / (true_pos_2 + false_neg_2)
#F1 - Most important Criteria
F1_2 <- 2/ ((precision_2^-1) + (recall_2^-1))

#Fit and Predict Model
svm_tuning_3  <- svm(y~ ., data=train_3, kernel="radial", gamma=.001, cost=.1, na.action = na.omit)
yhat.best <- predict(svm_tuning_3, newdata=test_3, na.action = na.exclude)

#Format factors into time series numeric
test_3$y <- as.numeric(levels(test_3$y))[test_3$y]
test_3$yhat <- as.numeric(yhat.best) - 1

#  Measurement statistics
true_pos_3 <- sum(ifelse(test_3$y == 1 & test_3$yhat == 1 , 1,0), na.rm = TRUE)
fals_pos_3 <- sum(ifelse(test_3$y == 0 & test_3$yhat == 1 , 1,0), na.rm = TRUE)
true_neg_3 <- sum(ifelse(test_3$y == 0 & test_3$yhat == 0 , 1,0), na.rm = TRUE)
false_neg_3<- sum(ifelse(test_3$y == 1 & test_3$yhat == 0 , 1,0),na.rm = TRUE)

# Precision
precision_3 <- true_pos_3 / (true_pos_3 + fals_pos_3)
#Recall
recall_3 <- true_pos_3 / (true_pos_3 + false_neg_3)
#F1 - Most important Criteria
F1_3 <- 2/ ((precision_3^-1) + (recall_3^-1))

SVM_F1 <- mean(c(F1_1,F1_2,F1_3))
SVM_P  <- mean(c(precision_1,precision_2,precision_3))
SVM_R  <- mean(c(recall_1,recall_2,recall_3))
