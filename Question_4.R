
#Predict Test Holt
Outsample = readYourFormat("C:/Users/alexandra/Documents/My_project/ft_project_outsample_13.csv")
Data_outsample = data.frame(
  matrix(
    NA, nrow = 12, ncol = 3,
    dimnames=list(c(),c("Shipments", "Month", "Year"))))
Data_outsample$"Shipments"= as.ts(Outsample[[1]][-c(13:24)])
Data_outsample$Year[c(1:11)]= 1988
Data_outsample$Year[c(12)]= 1989
Data_outsample$Month[c(1:11)]= c(2:12)
Data_outsample$Month[c(12)]= 1

TSData_final= ts(Data$Shipments, frequency = 12, start=c(1984, 10))
Indexes_in_final=decompose(TSData_final, type = "multiplicative")$seasonal
Indexes_out_final= as.numeric(tail(Indexes_in_final, frequency))
insample_no_seasonal_final= TSData_final/Indexes_in_final


hw_final = HoltWinters(insample_no_seasonal_final, gamma=FALSE,alpha=0.5, beta=0.07) # holt winters αρχικοποιω μοντελο
frc4_1 = forecast(hw_final, h=frequency)$mean*Indexes_out_final

# Predict Test MLR
Data_ml_final=Data
ml_model_final=lm(Shipments~Year+Month,data=Data_ml_final) #Only Year, Month
frc6_1=predict(ml_model_final,Data_outsample)

# comb final
TSData_outsample_final= ts(Data_outsample$Shipments, frequency = 12, start=c(1988, 2))


frc7_1= 0.1*frc4_1+0.9*frc6_1

smape_final= calculate.smape(frc7_1, TSData_outsample_final)




# bias/forcast
#bias <-c(mean(ME1),mean(ME2),mean(ME3),mean(ME4),mean(ME5),mean(ME6))
#acc<-c(mean(abs(ME1)),mean(abs(ME2)),mean(abs(ME3)),mean(abs(ME4)),mean(abs(ME5)),mean(abs(ME6)))
#plot(bias, acc, ylim=c(14,18), xlim=c(0,15))
#text(bias, acc, c("MLR","NN","HOLT","Random Forest","GBM","Comb"), col="red",pos=1)
#Evaluation

#bias <-c(mean(ME3),mean(ME4))
#acc<-c(mean(abs(ME3)),mean(abs(ME4)))
#plot(bias, acc, ylim=c(14,18), xlim=c(0,15))
#text(bias, acc, c("HOLT","Random Forest"), col="red",pos=1)
#Evaluation
