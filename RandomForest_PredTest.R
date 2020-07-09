library('tidyverse')
library('dplyr')
library('sas7bdat')
library('hms')
library('zoo')
library('highfrequency')
library('randomForest')
library('tree')
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
#Technical and Liquidity Measures
###################################################################################################

##Liquidity Measures 

#Bid/Ask Spread
data$ba_spread <- data$ASK - data$BID 

#Cumulative Return: Current return compared to yesterday's closing price
data <- data %>% 
  group_by(SYM_ROOT, DATE) %>% 
  mutate(cum_ret = cumsum(ln_ret)) %>% 
  ungroup()

#Fill missing start of day values with ln_ret
data$cum_ret <- ifelse(is.na(data$cum_ret), data$ln_ret, data$cum_ret)

#Depth Imbalance    
data <- data %>% 
  group_by(SYM_ROOT, DATE) %>% 
  mutate(depth_imb = 2*(cumsum(ASKSIZ) - cumsum(BIDSIZ))/(cumsum(ASK) + cumsum(BID))) %>% 
  ungroup()

#Quoted Spreads
data$qs <- (data$ASK - data$BID)/data$midpoint


##Technical Measures

#Price rate of Change
data$PROC <- (data$midpoint - lag(data$midpoint))/lag(data$midpoint) 

# Moving Average - 15 minute
data <- data %>% 
  group_by(SYM_ROOT) %>% 
  mutate(MA = rollapply(midpoint, FUN = sum, width = 15, fill = NA, align = 'right')) %>% 
  ungroup()

# Bias
data$BIAS <- (data$midpoint - data$MA)/data$MA

# Osciallator 
data$OS <- (data$MA - lag(data$MA)) /lag(data$MA)


# Volume Rate of Change 
data$mid_vol <- (data$BIDSIZ + data$ASKSIZ)/2
data$VROC <- (data$mid_vol - lag(data$mid_vol))/lag(data$mid_vol)
###VROC has infinite values
data$VROC <- NULL


###################################################################################################
#Model Preparation
###################################################################################################

#Create y- variable
data$y <- data$JumpTest
data$y <- lead(data$y)


#Create training dataset
train <- sample(1:nrow(data), nrow(data)/2)
data$y <- as.factor(data$y)
data_test <- (data[-train ,])
data_train <- data[train,]


###################################################################################################
#Random Forest: Best model mtry=21 trees=500??
###################################################################################################
#Fit model to training data
rf.y21 <- randomForest(y~. , data=data_train, mtry = 21, importance = TRUE, metric= 'Accuracy', na.action = na.omit )

#Out of sample prediction
yhat21 <- predict(rf.y21, newdata=data_test)


###################################################################################################
#Measures of Model Performance
###################################################################################################

#Format factors into time series numeric
data_test$yhat <- as.ts(yhat21) - 1
data_test$y <- as.ts(data_test$y) - 1
#  Measurement statistics - YHAT FOR OPTIMAL MODEL
data_test$true_pos <- ifelse(data_test$y == 1 & data_test$yhat == 1 , 1,0)
data_test$false_pos <- ifelse(data_test$y == 0 & data_test$yhat == 1 , 1,0)
data_test$true_neg  <- ifelse(data_test$y == 0 & data_test$yhat == 0 , 1,0)
data_test$false_neg <- ifelse(data_test$y == 1 & data_test$yhat == 0 , 1,0)


# Precision
precision <- sum(data_test$true_pos,na.rm = TRUE) / ((sum(data_test$true_pos,na.rm = TRUE) + sum(data_test$false_pos, na.rm = TRUE)))
precision

#Recall
recall <- sum(data_test$true_pos,na.rm = TRUE) / ((sum(data_test$true_pos,na.rm = TRUE) + sum(data_test$false_neg, na.rm = TRUE)))
recall 

#Accuracy
accuracy <- (sum(data_test$true_neg,na.rm = TRUE) + sum(data_test$true_pos,na.rm = TRUE)) / (sum(data_test$true_pos,na.rm = TRUE) + sum(data_test$false_pos, na.rm = TRUE) + sum(data_test$true_neg,na.rm = TRUE) + sum(data_test$false_neg, na.rm = TRUE))
accuracy

#Specificity 
specificity <- sum(data_test$true_neg,na.rm = TRUE) / ((sum(data_test$true_neg,na.rm = TRUE) + sum(data_test$false_neg, na.rm = TRUE)))
specificity

#F1 - Most important Criteria
F1 <- 2/ ((precision^-1) + (recall^-1)) 
F1

#Random Classifier (Weighted Probability)
prob_jump <- sum(as.ts(data_train$y)-1, na.rm=TRUE)/nrow(data_train)
data_test$benchmark <- rbinom(nrow(data_test), size=1, prob=prob_jump) 
 
data_test$true_pos_bench <- ifelse(data_test$y == 1 & data_test$benchmark == 1 , 1,0)
data_test$false_pos_bench <- ifelse(data_test$y == 0 & data_test$benchmark == 1 , 1,0)
data_test$true_neg_bench  <- ifelse(data_test$y == 0 & data_test$benchmark == 0 , 1,0)
data_test$false_neg_bench <- ifelse(data_test$y == 1 & data_test$benchmark == 0 , 1,0)


# Precision
precision_b <- sum(data_test$true_pos_bench,na.rm = TRUE) / ((sum(data_test$true_pos_bench,na.rm = TRUE) + sum(data_test$false_pos_bench, na.rm = TRUE)))
precision_b

#Recall
recall_b <- sum(data_test$true_pos_bench,na.rm = TRUE) / ((sum(data_test$true_pos_bench,na.rm = TRUE) + sum(data_test$false_neg_bench, na.rm = TRUE)))
recall_b 

#Accuracy
accuracy_b <- (sum(data_test$true_neg_bench,na.rm = TRUE) + sum(data_test$true_pos_bench,na.rm = TRUE)) / (sum(data_test$true_pos_bench,na.rm = TRUE) + sum(data_test$false_pos_bench, na.rm = TRUE) + sum(data_test$true_neg_bench,na.rm = TRUE) + sum(data_test$false_neg_bench, na.rm = TRUE))
accuracy_b

#Specificity 
specificity_b <- sum(data_test$true_neg_bench,na.rm = TRUE) / ((sum(data_test$true_neg_bench,na.rm = TRUE) + sum(data_test$false_neg_bench, na.rm = TRUE)))
specificity_b

#F1 - Most important Criteria
F1_b <- 2/ ((precision_b^-1) + (recall_b^-1)) 
F1_b



