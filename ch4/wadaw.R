
attach(euretail)
euretailts<-ts(y, start=c(1996,1), frequency=4)
euretailts

library(ggfortify)
library(ggplot2)
autoplot(euretailts)


euretail.train<-window(euretailts, end=c(2010,3), frequency=4)
euretail.train
euretail.test<-window(euretailts, start=c(2010,4), frequency=4)
euretail.test

library(trend)
mk.test(euretail.train)

library(seastests)
welch(euretail.train)

library(forecast)
fc1 <-ses(euretail.train,h= 5)
summary(fc1)

library(forecast)
euretail.zzz<-ets(euretail.train, model = "ZZZ")
summary(euretail.zzz)

FZZZ<-forecast(euretail.zzz,h=5)




library(fpp2)
autoplot(window(euretailts, series="Data")) +
  
  autolayer(fc1, series="SES with alpha=0.999", PI= FALSE) +
  autolayer(FZZZ, series="AAA", PI=FALSE) +
  autolayer(euretail.holt, series="Holt's Method", PI=FALSE) +
  autolayer(euretail.Dmholt, series="Damped Holt's Method", PI=FALSE) +
  
  xlab("Year") + ylab("unit") +
  
  ggtitle("Total use of electricity in the university") +
  
  geom_vline(xintercept = 2010+3/4, color="blue",linetype="dashed") +
  guides(colour=guide_legend(title="Forecast"))




Seuretail.Dmholt<-holt(euretail.train, damped=TRUE, h=5)
summary(euretail.Dmholt)

library(forecast)
euretail.holt<-holt(euretail.train, h=5)
summary(euretail.holt)



fitted(euretail.holt)
predict(euretail.holt)
accuracy(fc1, euretail.test)
accuracy(FZZZ, euretail.test)
accuracy(fitD1, euretail.test)
accuracy(fc1, euretail.test)

checkresiduals(euretail.zzz)

res<-residuals(euretail.zzz)
t.test(res ,alternative="two.sided")

library(FinTS)
ArchTest(res)

shapiro.test(res)

library(tseries)
adf.test(euretailts)

kpss.test(euretailts)


fitD <- auto.arima(euretail.train, trace=TRUE)
summary(fitD)


fitD1 <-forecast(fitD,h=5)


fitA <- auto.arima(euretail.train, seasonal = TRUE, approximation = FALSE)
summary(fitA)

fitB <- auto.arima(euretail.train, seasonal = TRUE , stepwise=TRUE)
summary(fitB)

fitC <- auto.arima(euretail.train, seasonal = TRUE , stepwise=FALSE)
summary(fitC)


library(fpp2)
autoplot(window(euretailts, series="Data")) +
  
  autolayer(fitD1, series="ARIMA(0,1,3)(0,1,1)", PI= FALSE) +
  
  xlab("Year") + ylab("unit") +
  
  ggtitle("Total use of electricity in the university") +
  
  geom_vline(xintercept = 2010+3/4, color="blue",linetype="dashed") +
  guides(colour=guide_legend(title="Forecast"))



checkresiduals(fitD)

res1<-residuals(fitD)
t.test(res1 ,alternative="two.sided")

library(FinTS)
ArchTest(res1)

shapiro.test(res1)

fets <- function (x, h) {
+forecast(euretail.zzz, h=5)
}
    
library(fpp2)
e.ets <- tsCV(euretail.train, fets, h=1)

mean(e.ets^2, na.rm=TRUE)


