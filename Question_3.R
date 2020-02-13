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
library(growthrates)


# Συνάρτηση για το SMAPE
calculate.smape = function(frc, outsample) {
  mean(200*abs(outsample-frc)/(abs(outsample)+abs(frc)))  
} 

##### https://r.789695.n4.nabble.com/R-Reading-a-horizontally-arranged-csv-file-replacing-values-and-saving-again-td4752831.html
readYourFormat <- function(connection) {
  lines <- readLines(connection)
  lines <- lines[nchar(lines) > 0]
  fields <- strsplit(lines, ",")
  headers <- vapply(fields, function(x)x[1], FUN.VALUE="")
  names(fields) <- headers
  contents <- lapply(fields, function(x)x[-1])
  contents
  contents[-1] <- lapply(contents[-1], as.numeric)
}

Insample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_insample_13.csv")
timeseries = as.numeric(Insample[[3]][-c(1:4, 29:44)])
Data = data.frame(
  matrix(
    NA, nrow=length(timeseries), ncol=3,
    dimnames=list(c(), c("Wine_sales", "Quarter", "Year"))))
Data$"Wine_sales" <- timeseries

# DATASET
start = 1
year = 1963
for (j in  c(1:length(timeseries))) {
  if (start %% 4 == 1) {
    year = year + 1
  }
  
  if (start %% 4 == 0) {
    Data$Quarter[j] = 4 
  } else {
    Data$Quarter[j] = start %% 4
  }
  
  Data$Year[j] = year
  start = start + 1
}


# QUESTION 3.A
jpeg(filename='outputs/Q3A.jpg', width=1200, height=720)
plot(ts(Data$Wine_sales, frequency=4, start=c(1964, 1)), type="l", xlab="Time", ylab="Wine Sales")
dev.off()

# QUESTION 3.B - Υπολογίστηκε σε excel απο τον τύπο των διαφανειών Growth Rate
calculate.growth.rate = function(y, ppy) {
  n <- length(y)
  return ((sum(y[(n-ppy+1):n])/ppy-sum(y[1:(n-ppy)])/(n-ppy))*100/(sum(y[1:(n-ppy)])/(n-ppy)))
}

growth_rate = calculate.growth.rate(Data$Wine_sale, 4)
# QUESTION 3.Γ

## A. LRL - Kανω train (fit) το μοντελο με φορμουλα ..... ως προς αποεποχικοποιημένο insample

lrlfit = lm(Wine_sales ~ Quarter + Year, data=Data)
Newdata = data.frame(
  matrix(
    NA, nrow=24, ncol=2,
    dimnames=list(c(), c("Quarter", "Year"))))
Newdata$Quarter= c(1:24) %% 4
Newdata$Quarter[Newdata$Quarter==0] = 4
Newdata$Year = as.vector(t(laply(c(1970:1975), function(x) c(x, x, x, x))))
lrlpred= predict.lm(lrlfit, newdata=Newdata, h=24)

jpeg(filename='outputs/Q3C.jpg', width=1200, height=720)
plot(
  ts(Data$Wine_sales, frequency=4, start=c(1964, 1)),
  type="l", xlab="Time", ylab="Wine Sales",
  xlim=c(1964, 1976), ylim=c(4200, 6000),
  main="LRL Forecasts")
lines(ts(lrlpred, frequency=4, start=c(1970, 1)), type="l", col="red")
dev.off()

## B. Damped - αν βάλω γ= TRUE (δηλαδή του λέω οτι τα data μου εχουν εποχικοτητα) δεν χρειάζεται να κανω αποεποχικοποιηση
# Πίνακας sMAPE για διάφορες τιμές των a κ b
#EvaluationHolt =data.frame(matrix(NA, ncol= 3, nrow= 100000))
#colnames(EvaluationHolt) =c("a", "b","sMAPE")
#x = seq(0.1,1, 0.01)
#y = seq(0.01,1, 0.01)
#i=1
#for (valx in x) {
#  for (valy in y) {
#    if (valy < valx) {
#      EvaluationHolt$a[i]= valx
#      EvaluationHolt$b[i]= valy
#      hw = HoltWinters(train_no_seasonal, gamma=FALSE, alpha=valx, beta=valy) # holt winters αρχικοποιω μοντελο
#      frc4 = forecast(hw, h=frequency)$mean*Indexes_out
#      EvaluationHolt$sMAPE[i]= calculate.smape(frc4, validate)
#      i=i+1
#    }
#  }
#}

#hw = HoltWinters(train_no_seasonal, gamma=FALSE,alpha=0.5, beta=0.07) # holt winters αρχικοποιω μοντελο
#frc4 = forecast(hw, h=frequency)$mean*Indexes_out
#Evaluation$sMAPE[4] = calculate.smape(frc4, validate)
#Evaluation$sMAPE[4, ] = accuracy(frc4, validate)
#Damped
#Dm = HoltWinters(train_no_seasonal, gamma=TRUE,alpha=0.5, beta=0.07) # holt winters αρχικοποιω μοντελο
#frc5 = forecast(hw, h=frequency)$mean*Indexes_out
#Evaluation$sMAPE[5] = calculate.smape(frc5, validate)
#Evaluation$sMAPE[5, ] = accuracy(frc5, validate)

#3Δ - Δώστε αντίστοιχα διαστήματα εμπιστοσύνης σημαντικότητας 80%
x = predict.lm(lrlfit, newdata=Newdata, h=24, level=0.8, interval="confidence")
write_xlsx(as.data.frame(x), path="C:/Users/alexandra/Documents/My_project/outputs/Q3D.xlsx")
jpeg(filename='outputs/Q3D.jpg', width=1200, height=720)
plot(as.data.frame(x)$upr, type="l", xlab="", ylab="Wine Sales", main="Confidence Level", col="blue", ylim=c(5000, 6100))
lines(as.data.frame(x)$fit, type="l", col="black")
lines(as.data.frame(x)$lwr, type="l", col="red")
legend("topleft", legend=c("Upper", "Predict", "Lower"), col=c("blue", "black", "red"), lty=1, cex=0.8)
dev.off()

#3E - Προβλέψεις 10% μειωμένες
adjusted = lrlpred * 0.9
jpeg(filename='outputs/Q3Ε.jpg', width=1200, height=720)
plot(
  ts(Data$Wine_sales, frequency=4, start=c(1964, 1)),
  type="l", xlab="Time", ylab="Wine Sales",
  xlim=c(1964, 1976), ylim=c(4200, 6000),
  main="LRL Forecasts")
lines(ts(lrlpred, frequency=4, start=c(1970, 1)), type="l", col="blue")
lines(ts(adjusted, frequency=4, start=c(1970, 1)), type="l", col="red")
dev.off()

#4Γ για 3Γ - αξιολόγηση πρόβλεψης 
#Q41
Outsample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_outsample_13.csv")
outsample_ts = as.ts(Outsample[[3]])
smape = calculate.smape(lrlpred, outsample_ts)

#4Γ για 3Ε - αξιολόγηση πρόβλεψης
smape_adjusted = calculate.smape(adjusted, outsample_ts)
