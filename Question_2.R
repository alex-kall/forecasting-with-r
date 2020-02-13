library(lubridate)
library(DT)      
library(forecast)
library(plyr)       
library(ggplot2)
library(nnet)         
library(neuralnet)
library(readr)
library(RSNNS)
library(randomForest)
library(writexl)
library(data.table)
library(Metrics)
library(corrplot)
library(tsbox)
library(gbm)
library(tidyr)
library(data.table)
library(tibble)
library(tsintermittent)

calculate.smape = function(frc, outsample) {
  mean(200*abs(outsample-frc)/(abs(outsample)+abs(frc)))  
}

##### https://r.789695.n4.nabble.com/R-Reading-a-horizontally-arranged-csv-file-replacing-values-and-saving-again-td4752831.html
readYourFormat <- function(connection) {
  lines <- readLines(connection)
  lines <- lines[ nchar(lines)>0 ] # omit empty lines
  fields <- strsplit(lines, ",") # split lines by commas into fields
  headers <- vapply(fields, function(x)x[1], FUN.VALUE="")
  names(fields) <- headers
  contents <- lapply(fields, function(x)x[-1]) # remaining entries are
  contents
  contents[-1] <- lapply(contents[-1], as.numeric)
}

Insample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_insample_13.csv")

timeseries = as.numeric(Insample[[2]][-c(1:4, 30:44)])
Data = data.frame(
  matrix(
    NA, nrow=length(timeseries), ncol=3,
    dimnames=list(c(), c("Cheese_stock", "Month", "Year"))))
Data$"Cheese_stock" <- timeseries

# DATASET
start = 9
year = 2010
for (j in  c(1:length(timeseries))) {
  if (start %% 12 == 1) {
    year = year + 1
  }
  
  if (start%%12==0) {
    Data$Month[j] = 12 
  } else {
    Data$Month[j] = start%%12
  }
  
  Data$Year[j] = year
  start = start+1
}


## QUESTION 2.A
jpeg(filename='outputs/Q2A.jpg', width=1200, height=720)
plot(ts(Data$Cheese_stock, frequency=12, start=c(2010, 9)), type="l", ylab="Cheese_stock", xlab="Time", col=1)
dev.off()
## QUESTION 2.B

# ΔΙΑΚΟΠΤΏΜΕΝΗ ΖΗΤΗΣΗ! ΠΑΡΑΤΗΡΕΙΤΑΙ ΖΉΤΗΣΗ ΙΣΗ ΜΕ 0 ΣΕ ΤΟΥΛΑΧΙΣΤΟΝ 5 ΣΗΜΕΙΑ ΤΗΣ ΧΡΟΝΟΣΕΙΡΑΣ
# πχ μονάδες διαχείρισης αποθεμάτων (stockkeeping units) και ανταλλακτικών (spareparts)

## QUESTION 2.Γ

# Croston - θετικη προκαταληψη

TSData = ts(Data$Cheese_stock, frequency=12, start=c(2010, 9))
xc = croston(TSData, h=5, alpha=0.05)
frccrost = ts(xc$mean, frequency=12, start=c(2012, 10))
plot(xc)

# sba 
a = 0.05
Fdemand = xc$model$demand$mean
Fintervals = xc$model$period$mean
frcsba = ts((1 - a / 2) * (Fdemand / Fintervals), frequency=12, start=c(2012, 10))
jpeg(filename='outputs/Q2C.jpg', width=1200, height=720)
plot(TSData, xlim=c(2010.6, 2013.2), main="Forecasts from SBA's method",
     xlab="", ylab="")
lines(frcsba, col="blue")
dev.off()

# Q4.
#Naive
frc = ts(naive(TSData, h=5)$mean, frequency=12, start=c(2012, 10))

Outsample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_outsample_13.csv")
outsample_ts = ts(Outsample[[2]][1:5], frequency=12, start=c(2012, 10))

sMAPE_naive = calculate.smape(frc, outsample_ts)
sMAPE_sba = calculate.smape(frcsba, outsample_ts)
sMAPE_crost = calculate.smape(frccrost, outsample_ts)

relsMAPE_sba = sMAPE_sba / sMAPE_naive
relsMAPE_crost = sMAPE_crost / sMAPE_naive
