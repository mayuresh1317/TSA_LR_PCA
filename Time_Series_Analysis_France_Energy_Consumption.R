rm(list=ls())
setwd("F://OneDrive - National College of Ireland/NCI/SEMESTER 1/SUBJECTS/Statistics for Data analysis/Assignments/TABA/Time Series Analysis/Datasets")
France_Con <- read.csv("France_Energy_Consumption.csv", header=T, na.strings=c(""), stringsAsFactors = T)
TFrance_Con <- t(France_Con)
NTFinland_EXP<- na.omit(TFinland_exp)
str(Finland_exp)
write.csv(NTFinland_EXP,"F://OneDrive - National College of Ireland/NCI/SEMESTER 1/SUBJECTS/Statistics for Data analysis/Assignments/TABA/Time Series Analysis/File Name_a.csv", row.names = FALSE)

install.packages("fpp2")
install.packages("carTool")


tsFrance <- ts(France_Con,start = c(2016,1),end=c(2020,10) ,frequency = 12)
is.ts(tsFrance_Con)
plot.ts(tsFrance, main= "Energy Consumption for France")

plot(tsFrance)
autoplot(tsFrance)
monthplot(tsFrance)
seasonplot(tsFrance)


#Decomposition

dec.france<- decompose(tsFrance, type="additive")
dec.france
plot(dec.france)
summary(dec.france)



#Model Snaive 

snaive.france<- snaive(tsFrance,h=4)
summary(snaive.france)
plot(snaive.france, PI="False")



#Model Holts winter

HWA<-hw(tsFrance,seasonal = "additive")
HWM<-hw(tsFrance,seasonal = "multiplicative")

HWA
summary(HWA)
summary(HWM)

forecast(HWA,2)
forecast(HWM,2)
round(accuracy(HWA),2)
round(accuracy(HWM),2)

autoplot(tsFrance)+autolayer(HWA,series = "additive hw",PI= FALSE)+
  autolayer(HWM,series = "multiplicative hw",PI= FALSE)+
  xlab("Months")+
  ylab("Consumption")+
  guides(colour=guide_legend(title="forecast"))
  

#Arima

#Test
adf.test(tsFrance)
ndiffs(tsFrance)
nsdiffs(tsFrance)
acf(tsFrance)
pacf(tsFrance)


rm(armia.france)
#Arima Model

autofit<-auto.arima(tsFrance)

qqnorm(autofit$residuals)
qqline(autofit$residuals)
Box.test(autofit$residuals, type = "Ljung-Box")
checkresiduals(autofit)
forecast(autofit,4)




fit <-arima(tsFrance,order = c(2,0,1))
fit
summary(fit)
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
checkresiduals(fit)
forecast(fit,4)

plot(forecast(fit,4),Xlab="Month",ylab="Consumption")




#not included ##########################################
ft.mdefrance<- decompose(tsFrance_Con, type="multiplicative")
ft.mdefrance
plot(ft.mdefrance)
summary(ft.mdefrance)

sesFrance_Con<- ses(tsFrance_Con, h=4)
summary(sesFrance_Con)

round(accuracy(sesFrance_Con),1)

autoplot(sesFrance_Con)+autolayer(fitted(sesFrance_Con),series="Fitted")

MATS5<-(ma(tsFrance_Con,5))
summary(MATS5)

ggtsdisplay(tsFrance_Con)


ft.mean <- meanf((tsFrance_Con),h=4)
summary(ft.mean)




hwets<- ets(tsFrance_Con,model="AAA")
hwets
forecast(hwets,1)
round(accuracy(hwets),2)
##################################################

split <- sample.split(tsFrance,SplitRatio =0.75)
training_set <- subset(tsFrance,split == TRUE)
test_set <- subset(tsFrance,split == FALSE)



#snaive
snaive.france_testset<- snaive(test_set,h=4)
summary(snaive.france_testset)
plot(snaive.france_testset, PI="False")

snaive.france_training_set<- snaive(training_set,h=4)
summary(snaive.france_training_set)
plot(snaive.france_training_set, PI="False")









