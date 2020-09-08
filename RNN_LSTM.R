#LSTM Test
library('tensorflow')
library('tfdatasets')
library('reticulate')
library('keras')
library('dplyr')
library('ggplot2')
#Specify where python and modules are for tensorflow & keras
use_condaenv("C:/Users/Patrick/anaconda3")
use_python("C:/Users/Patrick/anaconda3")
use_virtualenv("C:/Users/Patrick/anaconda3/test")


library('tidyverse')
library('dplyr')
library('hms')
library('zoo')
library('highfrequency')
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
data$timeround <- as_hms(data$timeround)
data$date_time <- as.POSIXct(paste(data$DATE, data$timeround), format="%Y-%m-%d %H:%M:%S")


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

rm(testing,training,validation)

#Use data$y as list of values to be predicted 
y_train_1 <- as.numeric(train_1$y) -1
train_1$y <- NULL
y_val_1 <- as.numeric(val_1$y) -1
val_1$y <- NULL
y_test_1 <- as.numeric(test_1$y) - 1
test_1$y <- NULL

train_1$timeround <- NULL
val_1$timeround <- NULL
test_1$timeround <- NULL

train_1$`data$y` <- NULL
val_1$`data$y` <- NULL
test_1$`data$y` <- NULL

train_1$DATE <- NULL
val_1$DATE <- NULL
test_1$DATE <- NULL

train_1$APPL <- as.numeric(train_1$APPL) - 1
train_1$GOOG <- as.numeric(train_1$GOOG) - 1
train_1$MSFT <- as.numeric(train_1$MSFT) - 1
train_1$FB <- as.numeric(train_1$FB) - 1

val_1$APPL <- as.numeric(val_1$APPL) - 1
val_1$GOOG <- as.numeric(val_1$GOOG) - 1
val_1$MSFT <- as.numeric(val_1$MSFT) - 1
val_1$FB <- as.numeric(val_1$FB) - 1

test_1$APPL <- as.numeric(test_1$APPL) - 1
test_1$GOOG <- as.numeric(test_1$GOOG) - 1
test_1$MSFT <- as.numeric(test_1$MSFT) - 1
test_1$FB <- as.numeric(test_1$FB) - 1

train_1$date_time <- NULL
train_1 <- as.matrix(train_1)
val_1$date_time <- NULL
val_1 <- as.matrix(val_1)
test_1$date_time <- NULL
test_1 <- as.matrix(test_1)

#Use data$y as list of values to be predicted 
y_train_2 <- as.numeric(train_2$y) -1
train_2$y <- NULL
y_val_2 <- as.numeric(val_2$y) -1
val_2$y <- NULL
y_test_2 <- as.numeric(test_2$y) - 1
test_2$y <- NULL

train_2$timeround <- NULL
val_2$timeround <- NULL
test_2$timeround <- NULL

train_2$`data$y` <- NULL
val_2$`data$y` <- NULL
test_2$`data$y` <- NULL

train_2$DATE <- NULL
val_2$DATE <- NULL
test_2$DATE <- NULL


train_2$APPL <- as.numeric(train_2$APPL) - 1
train_2$GOOG <- as.numeric(train_2$GOOG) - 1
train_2$MSFT <- as.numeric(train_2$MSFT) - 1
train_2$FB <- as.numeric(train_2$FB) - 1

val_2$APPL <- as.numeric(val_2$APPL) - 1
val_2$GOOG <- as.numeric(val_2$GOOG) - 1
val_2$MSFT <- as.numeric(val_2$MSFT) - 1
val_2$FB <- as.numeric(val_2$FB) - 1


test_2$APPL <- as.numeric(test_2$APPL) - 1
test_2$GOOG <- as.numeric(test_2$GOOG) - 1
test_2$MSFT <- as.numeric(test_2$MSFT) - 1
test_2$FB <- as.numeric(test_2$FB) - 1

train_2$date_time <- NULL
train_2 <- as.matrix(train_2)
val_2$date_time <- NULL
val_2 <- as.matrix(val_2)
test_2$date_time <- NULL
test_2 <- as.matrix(test_2)


#Use data$y as list of values to be predicted & remove from training matrix
y_train_3 <- as.numeric(train_3$y) -1
train_3$y <- NULL
y_val_3 <- as.numeric(val_3$y) -1
val_3$y <- NULL
y_test_3 <- as.numeric(test_3$y) - 1
test_3$y <- NULL

train_3$timeround <- NULL
val_3$timeround <- NULL
test_3$timeround <- NULL

train_3$`data$y` <- NULL
val_3$`data$y` <- NULL
test_3$`data$y` <- NULL

train_3$DATE <- NULL
val_3$DATE <- NULL
test_3$DATE <- NULL

train_3$APPL <- as.numeric(train_3$APPL) - 1
train_3$GOOG <- as.numeric(train_3$GOOG) - 1
train_3$MSFT <- as.numeric(train_3$MSFT) - 1
train_3$FB <- as.numeric(train_3$FB) - 1

val_3$APPL <- as.numeric(val_3$APPL) - 1
val_3$GOOG <- as.numeric(val_3$GOOG) - 1
val_3$MSFT <- as.numeric(val_3$MSFT) - 1
val_3$FB <- as.numeric(val_3$FB) - 1

test_3$APPL <- as.numeric(test_3$APPL) - 1
test_3$GOOG <- as.numeric(test_3$GOOG) - 1
test_3$MSFT <- as.numeric(test_3$MSFT) - 1
test_3$FB <- as.numeric(test_3$FB) - 1

train_3$date_time <- NULL
train_3 <- as.matrix(train_3)
val_3$date_time <- NULL
val_3 <- as.matrix(val_3)
test_3$date_time <- NULL
test_3 <- as.matrix(test_3)


###################################################################################################
#Generate 3-Dimensional Input Data
###################################################################################################

#Lookback: 5-minutes behind
#Step: Sampled every minute
#Delay: 1 minute forward
lookback <- 5
step <- 1
delay <- 1
batch_size <- 128


generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 1) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      #targets[[j]] <- data[rows[[j]] + delay,2]
    }           
    list(samples)
  }
}

train_gen <- generator(
  data = train_1,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size =nrow(train_1)
)
val_gen = generator(
  data =val_1,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size = nrow(val_1)
)
test_gen <- generator(
  data = test_1,
  lookback = lookback,
  delay = delay,
  shuffle = FALSE,
  min_index = 1,
  max_index = NULL,
  step = step,
  batch_size = nrow(test_1)
)

train_matrix <- train_gen()
x_train_1 <- train_matrix[[1]]

val_matrix <- val_gen()
x_val_1 <- val_matrix[[1]]

test_matrix <- test_gen()
x_test_1 <- test_matrix[[1]]

y_train_1 <- y_train_1[1:nrow(x_train_1)]
y_val_1 <- x_val_1[1:nrow(x_val_1)]

train_gen <- generator(
  data = train_2,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size =nrow(train_2)
)
val_gen = generator(
  data =val_2,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size = nrow(val_2)
)
test_gen <- generator(
  data = test_2,
  lookback = lookback,
  delay = delay,
  shuffle = FALSE,
  min_index = 1,
  max_index = NULL,
  step = step,
  batch_size = nrow(test_2)
)

train_matrix <- train_gen()
x_train_2 <- train_matrix[[1]]

val_matrix <- val_gen()
x_val_2 <- val_matrix[[1]]

test_matrix <- test_gen()
x_test_2 <- test_matrix[[1]]

y_train_2 <- y_train_2[1:nrow(x_train_2)]
y_val_2 <- x_val_2[1:nrow(x_val_2)]


train_gen <- generator(
  data = train_3,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size =nrow(train_3)
)
val_gen = generator(
  data =val_3,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size = nrow(val_3)
)
test_gen <- generator(
  data = test_3,
  lookback = lookback,
  delay = delay,
  shuffle = FALSE,
  min_index = 1,
  max_index = NULL,
  step = step,
  batch_size = nrow(test_3)
)

train_matrix <- train_gen()
x_train_3 <- train_matrix[[1]]

val_matrix <- val_gen()
x_val_3 <- val_matrix[[1]]

test_matrix <- test_gen()
x_test_3 <- test_matrix[[1]]

y_train_3 <- y_train_3[1:nrow(x_train_3)]
y_val_3 <- x_val_3[1:nrow(x_val_3)]



###################################################################################################
#LSTM Model
###################################################################################################

model <- keras_model_sequential() %>% 
  layer_lstm(units = 40, recurrent_dropout = 0.5, return_sequences = FALSE, input_shape = list(NULL, 29) ) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_adam( lr =0.001),
  loss = "binary_crossentropy",
  metrics = c('accuracy')
)

history <- model %>% fit(
  x = x_train_1,
  y = y_train_1,
  batch_size = 256,
  epochs = 60,
  validation_data = list(x_val_1, y_val_1))

predict_1 <- model %>% 
  predict_classes(x_test_1)

history <- model %>% fit(
  x = x_train_2,
  y = y_train_2,
  batch_size = 256,
  epochs = 60,
  validation_data = list(x_val_2, y_val_2))

predict_2 <- model %>% 
  predict_classes(x_test_2)

history <- model %>% fit(
  x = x_train_3,
  y = y_train_3,
  batch_size = 256,
  epochs = 60,
  validation_data = list(x_val_3, y_val_3))

predict_3 <- model %>% 
  predict_classes(x_test_3)

##################################################################################################
# Measurement Statistics
###################################################################################################

#Combine X, Y and Predicted Y
test_1 <- as.data.frame(x_test_1)
test_1 <- cbind(y_test_1,predict_1)
test_1 <- as.data.frame(test_1)
names(test_1)[2] <- 'yhat'
names(test_1)[names(test_1) == 'y_test_1'] <- 'y'

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


#Combine X, Y and Predicted Y
test_2 <- as.data.frame(x_test_2)
test_2 <- cbind(y_test_2,predict_2)
test_2 <- as.data.frame(test_2)
names(test_2)[2] <- 'yhat'
names(test_2)[names(test_2) == 'y_test_2'] <- 'y'

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

#Combine X, Y and Predicted Y
test_3 <- as.data.frame(x_test_3)
test_3 <- cbind(y_test_3,predict_3)
test_3 <- as.data.frame(test_3)
names(test_3)[2] <- 'yhat'
names(test_3)[names(test_3) == 'y_test_3'] <- 'y'

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

mean(c(F1_1,F1_2,F1_3))
mean(c(precision_1,precision_2,precision_3))
mean(c(recall_1,recall_2,recall_3))

