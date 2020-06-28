##############################################################################
#Format SAS Data
##############################################################################
library('tidyverse')
library('dplyr')
library('sas7bdat')
library('hms')
library('highfrequency')
if(!require(tbl2xts)) install.packages("tbl2xts")
if(!require(rmsfuns)) install.packages("rmsfuns")
rm(list=ls())
#dev.off()


#Import data
data <- read.sas7bdat("C:/Users/Patrick/Documents/Current Semester/SAS/taq_fb.sas7bdat")

#formate dates/times
data$timeround <- as.hms(data$timeround)
data$TIME_M <- as.hms(data$TIME_M)
data$DATE <- as.Date(data$DATE,origin="1960-01-01")

#Create time format reference - Must work with original nonformatted values
time_ref <- as.data.frame(cbind('original' = data$timeround, 'formatted' = data$timeround))
time_ref$formatted <- as.hms(time_ref$formatted)
time_ref <- time_ref[1:392,1:2]


#No negative prices, but several 0 bids/asks
#Best to repeat previous value for missing numbers

#Recode 0 to NA
data <- data %>%
  mutate(BID=replace(BID, BID<10, NA))
data <- data %>% 
  mutate(ASK=replace(ASK, ASK==0, NA))

#Fill NA with previous values
data <- data %>% 
  group_by(SYM_ROOT) %>% 
  fill(BID, .direction = 'up') %>% 
  ungroup()

data <- data %>% 
  group_by(SYM_ROOT) %>% 
  fill(ASK, .direction = 'up') %>% 
  ungroup()

#Create midpoint price
midpoint <- (data$BID + data$ASK)/2
data <- cbind(data,midpoint)

#Create ln returns
data <- data %>% 
  group_by(SYM_ROOT) %>% 
  mutate(ln_ret = log((midpoint)/lag(midpoint))) %>% 
  ungroup()


#Save R.file
save(data, file = 'Final.RData')




#Check dates and values by stocks
AAPL <- data %>% 
  filter(SYM_ROOT == 'AAPL')
MSFT <- data %>% 
  filter(SYM_ROOT == 'MSFT')
FB <- data %>% 
  filter(SYM_ROOT == 'FB')
GOOG <- data %>% 
  filter(SYM_ROOT == 'GOOG')


summary(MSFT$BID)
summary(AAPL$BID)
summary(FB$BID)
summary(GOOG$BID)

summary(MSFT$ASK)
summary(AAPL$ASK)
summary(FB$ASK)
summary(GOOG$ASK)

AAPL %>% 
  filter(timeround == 34200)
FB %>% 
  filter(timeround == 34200)
MSFT%>% 
  filter(timeround == 34200)
GOOG%>% 
  filter(timeround == 34200)

