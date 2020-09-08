##############################################################################
#Format SAS Data
##############################################################################
library('tidyverse')
library('dplyr')
library('sas7bdat')
library('hms')
library('highfrequency')
library('zoo')
if(!require(tbl2xts)) install.packages("tbl2xts")
if(!require(rmsfuns)) install.packages("rmsfuns")
rm(list=ls())
#dev.off()

set.seed(123)
######DOUBLE CHECK SAS OUTPUT DATA FILES 
#Import data
data <- read.sas7bdat("C:/Users/Patrick/Documents/Current Semester/SAS/taq_data.sas7bdat")

#formate dates/times
data$timeround <- as.hms(data$timeround)
data$TIME_M <- as.hms(data$TIME_M)
data$DATE <- as.Date(data$DATE,origin="1960-01-01")


#Sort data by Tickr, Date then time
data <- data[order(data$SYM_ROOT, data$DATE, data$timeround), ]

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

#Drop counter & factor id
data$n <- NULL
data$X_FREQ_ <- NULL


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


#Save R.file
save(data, file = 'formatted.RData')







# #Check dates and values by stocks
# AAPL <- data %>% 
#   filter(SYM_ROOT == 'AAPL')
# MSFT <- data %>% 
#   filter(SYM_ROOT == 'MSFT')
# FB <- data %>% 
#   filter(SYM_ROOT == 'FB')
# GOOG <- data %>% 
#   filter(SYM_ROOT == 'GOOG')
# 
# 
# summary(MSFT$BID)
# summary(AAPL$BID)
# summary(FB$BID)
# summary(GOOG$BID)
# 
# summary(MSFT$ASK)
# summary(AAPL$ASK)
# summary(FB$ASK)
# summary(GOOG$ASK)



