library('tidyverse')
library('dplyr')
library('sas7bdat')
library('hms')
library('zoo')
library('highfrequency')
library('tidyquant')
library('ggplot2')
if(!require(tbl2xts)) install.packages("tbl2xts")
if(!require(rmsfuns)) install.packages("rmsfuns")
rm(list=ls())
dev.off()

###################################################################################################
#Lee & Mykland Jump test
#Using midpoint between bid/ask
###################################################################################################

#Import data
load('formatted.RData')
#Sort data by Tickr, Date then time
data <- data[order(data$SYM_ROOT, data$DATE, data$timeround), ]


# #Create midpoint price
# data <- data %>% 
#   mutate(midpoint = (BID + ASK)/2)

#Realized Bipower Variation 
#K rolling window : As suggested by Lee & Mykland   K = nobs (where nobs - # obs. per day) 
K= 315

data <- data %>%
  group_by(SYM_ROOT) %>%
  mutate(RV = abs(ln_ret)*abs(lag(ln_ret))) %>%
  ungroup()
# data <- data %>% 
#   group_by(SYM_ROOT) %>% 
#   mutate(RV = abs(log(midpoint/lag(midpoint)) * abs(log(lag(midpoint)/lag(midpoint,n = 2L))))) %>% 
#   ungroup()

#Realized Bipower Variation : Equation (8) of Lee & Mykland
data <- data %>% 
  group_by(SYM_ROOT) %>% 
  mutate(BV = (1/((K-2))*rollapply(RV, FUN = sum, width=K, fill = NA, align = 'right'))) %>% 
  ungroup()

#Measure of Instantaneous Volatility: Equation (7) of Lee & Mykland
data <- data %>% 
  group_by(SYM_ROOT) %>% 
  mutate(Lstat = ln_ret/sqrt(BV)) %>% 
  ungroup()

#Rejection Parameters : Equation (12) 
#Equation (13) : C & S 
#Should this adjust?
#n <- nrow(data)/4
data <- data %>%  
  group_by(SYM_ROOT) %>%  
  mutate(n = row_number()) %>% 
  ungroup()
c <- 0.7979
data$C_n <- (sqrt(2*log(data$n))/c)  - (log(pi)+log(log(data$n))) / (2*c*sqrt(2*log(data$n)))
data$S_n <- 1/(c*sqrt(2*log(data$n)))


#Rejection Parameters: If Equation (13) >  4.6001 then we reject the hypothesis of no jump at t_i at 1% level 
data <- data %>% 
  group_by(SYM_ROOT) %>% 
  mutate(Reject = ((abs(Lstat) - C_n)/S_n) - 4.6001) %>% 
  ungroup()

#Jump Test: 1 if Jump detected, 0 if else (techninaclly if we fail to reject the null hypothesis)
data$JumpTest <- ifelse(data$Reject > 0 , 1, 0)

#Total number of Jumps Detected & Total obs in data
sum(data$JumpTest, na.rm = TRUE)
nrow(data)

#Count missing values
sum(is.na(data$JumpTest))


#Drop unnecessary variables to speed up model training time
data$C_n <- NULL
data$S_n <- NULL
data$Lstat <- NULL



#Save R.file
save(data, file = 'Final.RData')

###################################################################################################
#Visualize one day of AAPL Returns/Jumps
###################################################################################################

#View a single day of Returns for AAPL
day <- data %>% 
  filter(DATE == as.Date('2020-01-09'))
day <- day %>% 
  filter(SYM_ROOT == 'AAPL')
day$Vis.Test <- ifelse(day$Reject > 0, 1, -1)

#Visualizing jumps in one day
# plot(day$timeround, day$ln_ret, type='l', col = 'blue')
# lines(day$timeround,day$Vis.Test, type = 'l',lwd = '2' )

ggplot(data = day, aes(x = timeround, y = ln_ret)) +
  geom_line() +
  ggtitle('One day of Returns', subtitle = "APPL 1-min log Returns") +
  labs(y = 'Log Returns', x = 'Time') +
  theme_tq()


