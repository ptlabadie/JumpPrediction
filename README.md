# jumptest
##Predicting Abnormal Returns in Financial Markets

The SAS macro pulls 1 minute returns for a specific stock from WRDS and appends these returns to a master file. To run the macro, input a beginning and end date in YYYYMMDD format. These dates must align with market open days, while any holidays/weekends between these days will be skipped. 
Use the .RData file 'format_sas' to format the SAS file for working with R.
Use the .RData file 'JumpTest' to perform Lee & Mykland's 2008 Jump Test. The only changes needed to be made are for the rolling window 'K' value. Currently uses the paper's suggestion of ~300 past observations for rolling volatility measure. If using 5 min returns, change K to 270 as per their suggestion. 
