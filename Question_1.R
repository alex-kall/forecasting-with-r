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
library(smooth)
library(fpp)

# Συνάρτηση για το SMAPE
calculate.smape = function(frc, outsample) {
  mean(200*abs(outsample-frc)/(abs(outsample)+abs(frc)))  
}

##### https://r.789695.n4.nabble.com/R-Reading-a-horizontally-arranged-csv-file-replacing-values-and-saving-again-td4752831.html
readYourFormat = function(connection) {
  # connection is a file name or connection object referring
  # to text of the form
  # Header A,Some text
  # Header B,<number>,<number>,<number>
  # Header C,<number>
  # Header D,<number>,<number>,<number>,<number>
  lines = readLines(connection)
  lines = lines[ nchar(lines)>0 ] # omit empty lines
  fields = strsplit(lines, ",") # split lines by commas into fields
  headers = vapply(fields, function(x)x[1], FUN.VALUE="")
  names(fields) = headers
  contents = lapply(fields, function(x)x[-1]) # remaining entries are
  contents
  contents[-1] = lapply(contents[-1], as.numeric)
}

Insample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_insample_13.csv")

Data = data.frame(
  matrix(
    NA, nrow = 40, ncol = 3,
    dimnames=list(c(),c("Shipments", "Month", "Year"))))
Data$"Shipments"= as.ts(Insample[[1]][-c(1:4)])
Data$"Shipments"[40]=5950


#DATASET
start=10
year= 1984
for (j in  c(1:40)) {
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


# QUESTION 1.A
jpeg(filename='outputs/Q1A.jpg', width=1200, height=720)
plot(Data$Shipments,type="l",ylab="Shipments", xlab="Time", col=1)
dev.off()

# QUESTION 1.B
freq = 12
TSData = ts(Data$Shipments, frequency=freq, start=c(1984, 10))
DecData = decompose(TSData, type="multiplicative")
jpeg(filename='outputs/Q1B.jpg', width=1200, height=720)
plot(DecData$seasonal, ylab="seasonal", xlab="Time", col=1)
dev.off()

# QUESTION 1.Γ - Test various Forecasting Methods
Evaluation = data.frame(matrix(NA, ncol=1, nrow=7))
row.names(Evaluation) = c("Naive", "sNaive", "SES_Mul", "Holt", "Damped", "MLR", "Comb")
colnames(Evaluation) = c("sMAPE")
train = head(TSData, length(TSData) - freq)
validate = tail(TSData, freq)

#Naive
frc1 = naive(train, h=freq)$mean
Evaluation$sMAPE[1] = calculate.smape(frc1, validate)

#Seasonal Naive
frc2 = as.numeric(tail(train, freq)) + validate - validate
Evaluation$sMAPE[2] = calculate.smape(frc2, validate)

#SES - with decomposition (Multiplicative)
Indexes_in = decompose(train, type = "multiplicative")$seasonal
Indexes_out = as.numeric(tail(Indexes_in, freq))
frc3 = ses(train/Indexes_in,h = freq)$mean*Indexes_out
Evaluation$sMAPE[3] = calculate.smape(frc3, validate)

#Holt
train_no_seasonal = train / Indexes_in  # αποεποχικοποιημένο insample

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
#      frc4 = forecast(hw, h=freq)$mean*Indexes_out
#      EvaluationHolt$sMAPE[i]= calculate.smape(frc4, validate)
#      i=i+1
#    }
#  }
#}

hw = HoltWinters(train_no_seasonal, gamma=FALSE, alpha=0.5, beta=0.07)  # holt winters αρχικοποιω μοντελο
frc4 = forecast(hw, h=freq)$mean*Indexes_out
Evaluation$sMAPE[4] = calculate.smape(frc4, validate)

#Damped
#EvaluationDamped =data.frame(matrix(NA, ncol= 3, nrow= 100000))
#colnames(EvaluationHolt) =c("a", "b","sMAPE")
#x = seq(0.1,1, 0.01)
#y = seq(0.01,1, 0.01)
#i=1
#for (valx in x) {
#  for (valy in y) {
#    if (valy < valx) {
#      EvaluationDamped$a[i]= valx
#      EvaluationDamped$b[i]= valy
#      hw = HoltWinters(train_no_seasonal, gamma=TRUE, alpha=valx, beta=valy) # holt winters αρχικοποιω μοντελο
#      frc5 = forecast(hw, h=freq)$mean*Indexes_out
#      EvaluationDamped$sMAPE[i]= calculate.smape(frc5, validate)
#      i=i+1
#    }
#  }
#}

Dm = HoltWinters(train_no_seasonal, gamma=TRUE, alpha=0.29, beta=0.28)  # holt winters αρχικοποιω μοντελο
frc5 = forecast(Dm, h=freq)$mean*Indexes_out
Evaluation$sMAPE[5] = calculate.smape(frc5, validate)

#MLR
Data_ml = Data
train_ml = head(Data_ml, nrow(Data_ml) - freq)
validate_ml = tail(Data_ml,freq)
ml_model = lm(Shipments~Year + Month, data=train_ml)  #Only Year, Month
frc6 = predict(ml_model, validate_ml)
Evaluation$sMAPE[6] = calculate.smape(frc6, validate_ml$Shipments)

#comb holt + mlr
frc7= 0.3 * frc4 + 0.7 * frc6
Evaluation$sMAPE[7]= calculate.smape(frc7, validate)

## Fit in whole insample
In_in = decompose(TSData, type = "multiplicative")$seasonal
In_out = as.numeric(tail(In_in, freq))
hw_1 = HoltWinters(TSData / In_in, gamma=FALSE, alpha=0.5, beta=0.07)  # holt winters αρχικοποιω μοντελο
frc4_1 = forecast(hw_1, h=freq)$mean*In_out

test_ml = data.frame(
  matrix(
    NA, nrow=12, ncol=2,
    dimnames=list(c(),c("Month", "Year"))))
test_ml$Year[1:11] = 1988
test_ml$Year[12] = 1989
test_ml$Month[1:11] = c(2:12)
test_ml$Month[12] = 1
ml_model1 = lm(Shipments~Year + Month, data=Data_ml)  # Only Year, Month
frc6_1 = predict(ml_model1,test_ml)

# Final Combination Forecasts
frc7_1 = 0.3 * frc4_1 + 0.7 * frc6_1

# QUESTION 1.Δ
## 3η Μέθοδος - ΑΝΙΧΝΕΥΣΗ ΕΙΔΙΚΩΝ ΓΕΓΟΝΟΤΩΝ ΜΕ ΜΕΘΟΔΟ ΑΠΟΕΠΟΧΙΚΟΠΟΙΗΣΗΣ (ΚΜΟ Κ ΠΕΡΙΟΔΩΝ ΤΗΣ ΑΠΟΕΠΟΧΙΚΟΠΟΙΗΜΕΝΗΣ ΧΡΟΝΟΣΕΙΡΑΣ)
#Data$KMO5 = 0
#Data$KMO7 = 0
Indexes_in_sea = decompose(TSData, type = "multiplicative")$seasonal
desesdata = TSData / Indexes_in_sea
Data$Desesdata = desesdata
#autoplot(desesdata)
#Data$KMO5 = ma(desesdata, 5)
#Data$KMO7 = ma(desesdata, 7)
#Data$Ratio5_7= Data$KMO7/Data$KMO5
#Data$Is_sea5_7 = FALSE
#Data$Is_sea5_7[Data$Ratio5_7 >= 1.05 | Data$Ratio5_7 <= 0.95 ] = TRUE


## 4η Μέθοδος
Data$KKMO12 = ma(desesdata, order=12, centre=TRUE)
Data$Ratio12 = Data$Desesdata / Data$KKMO12
Data$Is_sea12[!is.na(Data$Ratio12)] = FALSE
Data$Is_sea12[Data$Ratio12 >= 1.1 | Data$Ratio12 <= 0.9 ] = TRUE
write_xlsx(Data, path="C:/Users/alexandra/Documents/My_project/outputs/Q1D.xlsx")

# QUESTION 4.1
Outsample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_outsample_13.csv")
Data_outsample = data.frame(
  matrix(
    NA, nrow=12, ncol=3,
    dimnames=list(c(), c("Shipments", "Month", "Year"))))
Data_outsample$"Shipments" = as.ts(Outsample[[1]][-c(13:24)])
Data_outsample$Year[c(1:11)] = 1988
Data_outsample$Year[c(12)] = 1989
Data_outsample$Month[c(1:11)] = c(2:12)
Data_outsample$Month[c(12)] = 1

TSData_final = ts(Data$Shipments, frequency = freq, start=c(1984, 10))
Indexes_in_final = decompose(TSData_final, type = "multiplicative")$seasonal
Indexes_out_final = as.numeric(tail(Indexes_in_final, freq))
insample_no_seasonal_final = TSData_final/Indexes_in_final


hw_final = HoltWinters(insample_no_seasonal_final, gamma=FALSE, alpha=0.5, beta=0.07)  # holt winters αρχικοποιω μοντελο
frc4_1 = forecast(hw_final, h=freq)$mean*Indexes_out_final

# Predict Test MLR
Data_ml_final = Data
ml_model_final = lm(Shipments ~ Year + Month, data=Data_ml_final)  # Only Year, Month
frc6_1 = predict(ml_model_final, Data_outsample)

# comb final
TSData_outsample_final = ts(Data_outsample$Shipments, frequency=freq, start=c(1988, 2))
frc7_1 = 0.1 * frc4_1 + 0.9 * frc6_1
smape_final = calculate.smape(frc7_1, TSData_outsample_final)

# BIAS
ME_final <- TSData_outsample_final - frc7_1
bias = mean(ME_final)

#ME1 <- TSData_outsample_final - ts(frc1, frequency=freq, start=c(1988, 2))
#ME2 <- TSData_outsample_final - ts(frc2, frequency=freq, start=c(1988, 2))
#ME3 <- TSData_outsample_final - ts(frc3, frequency=freq, start=c(1988, 2))
#ME4 <- TSData_outsample_final - ts(frc4, frequency=freq, start=c(1988, 2))
#ME5 <- TSData_outsample_final - ts(frc5, frequency=freq, start=c(1988, 2))
#ME6 <- TSData_outsample_final - ts(frc6, frequency=freq, start=c(1988, 2))
#bias <- c(mean(ME1), mean(ME2), mean(ME3), mean(ME4), mean(ME5), mean(ME6))
#acc <- c(mean(abs(ME1)), mean(abs(ME2)), mean(abs(ME3)), mean(abs(ME4)), mean(abs(ME5)), mean(abs(ME6)))
#jpeg(filename='outputs/Q4A.jpg', width=1200, height=720)
#plot(bias, acc, ylim = c(1300, +2000), xlim=c(-3000, +2500))
#text(bias, acc, c("Naive", "sNaive", "SES mul", "HOLT", "Damped", "MLR"), col="red", pos=2)
#dev.off()

#Explain the effect of comb
#ME4_1 <- TSData_outsample_final - frc4_1
#ME6_1 <- TSData_outsample_final - frc6_1
#x <- bias / frc7_1
#bias <- c(mean(ME4_1), mean(ME6_1))
#acc <- c(mean(abs(ME4_1)), mean(abs(ME6_1)))
#plot(bias, acc, xlim=c(-5500, 0), ylim=c(1000, 6000))
#text(bias, acc, c("Damped", "MLR"), col="red", pos=4)
