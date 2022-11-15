
attach(power)
powerts<-ts(unit, start=c(2012,1), frequency=12)
powerts

library(ggfortify)
library(ggplot2)
autoplot(powerts)
plot(powerts)

power.train<-window(powerts, start=c(2012,1), end=c(2020,12), frequency=12)
power.train
power.test<-window(powerts, start=c(2021,1), frequency=12)
power.test

attach(power2)
power2ts<-ts(power_2, start=c(2012,1), frequency=12)
power2ts

power2.train<-window(power2ts, start=c(2012,1), frequency=12)
power2.train



library(trend)
mk.test(power.train)

library(seastests)
welch(power.train)

library(forecast)
fc1 <-ses(power.train,h= 18)
summary(fc1)

library(forecast)
power.zzz<-ets(power.train, model = "ZZZ")
summary(power.zzz)

FZZZ<-forecast(power.zzz,h=18)
FZZZ

library(fpp2)
autoplot(window(powerts, series="Data")) +
  
  xlab("Year") + ylab("unit") +
  
  ggtitle("Total use of electricity in the university") +
  
  guides(colour=guide_legend(title="Forecast"))


library(fpp2)
autoplot(window(powerts, series="Data")) +
  
  autolayer(FZZZ, series="ANA", PI=FALSE) +
  
  xlab("Year") + ylab("unit") +
  
  ggtitle("Total use of electricity in the university") +
  
  geom_vline(xintercept = 2021+7/12, color="blue",linetype="dashed") +
  guides(colour=guide_legend(title="Forecast"))




power.Dmholt<-holt(power.train, damped=TRUE, h=18)
summary(power.Dmholt)

library(forecast)
power.holt<-holt(power.train, h=18)
summary(power.holt)



fitted(power.holt)
predict(power.holt)
accuracy(fitB1, power.test)
accuracy(FZZZ, power.test)
accuracy(fc1, power.test)

checkresiduals(power.zzz)

res<-residuals(power.zzz)
t.test(res ,alternative="two.sided")

library(FinTS)
ArchTest(res)

shapiro.test(res)

library(tseries)
adf.test(power.train)

kpss.test(power.train)

ggAcf(power.train)
ggPacf(power.train)



fitD <- auto.arima(power.train, trace=TRUE)
summary(fitD)

fit1 <-Arima(power.train, order=c(1,0,1), seasonal=c(1,0,1))
summary(fit1)

fit11 <-forecast(fit1,h=18)
summary(fit11)
fitB1 <-forecast(fitB,h=18)
summary(fitB1)
fitC1 <-forecast(fitC,h=18)

fitA <- auto.arima(power.train, seasonal = TRUE, approximation = FALSE)
summary(fitA)

fitB <- auto.arima(power.train, seasonal = TRUE , stepwise=TRUE)
summary(fitB)

fitC <- auto.arima(power.train, seasonal = TRUE , stepwise=FALSE)
summary(fitC)


library(fpp2)
autoplot(window(powerts, series="Data")) +
  
  autolayer(fit11, series="ARIMA(1,0,1)(1,0,1)", PI= FALSE) +
  autolayer(fitB1, series="ARIMA(1,0,2)(2,0,0)", PI= FALSE) +
  autolayer(fitC1, series="ARIMA(0,0,4)(1,0,0)", PI= FALSE) +
  
  
  xlab("Year") + ylab("unit") +
  
  ggtitle("Total use of electricity in the university") +
  
  geom_vline(xintercept = 2021, color="blue",linetype="dashed") +
  guides(colour=guide_legend(title="Forecast"))



checkresiduals(fitB,lag=24)

res1<-residuals(fitB)
t.test(res1 ,alternative="two.sided")

library(FinTS)
ArchTest(res1)

shapiro.test(res1)

fets <- function (x, h) {
  +forecast(power.zzz, h=12)
}

library(fpp2)
e.ets <- tsCV(power.train, fets, h=1)

mean(e.ets^2, na.rm=TRUE)

fit.01 <- tslm(x ~ FZZZ + B1,data=power2.train)
summary(fit.01)

fcast.01 <- forecast(fit.01,h=18)
fcast.01


for1 <- fitted(FZZZ)
for1
autoplot(FZZZ)

#arima
for2 <- (fitB1)
for2

library(forecast)
#train <- window(co2, end=c(1990,12))
#test <- window(co2, start=c(1991,1))
ETS <- forecast(ets(train), h=18)
ARIMA <- forecast(auto.arima(train, lambda=0), h=18)
X <- cbind(ARIMA=fitB1$mean, STL=FZZZ$mean)

library(rio)
export(FZZZ,"gg1.xlsx")
export(fitB1,"gg2.xlsx")
df_new <- cbind(df, new)



###X11###
library(seasonal)
fitx11<-seas(power.train,x11="")
fitx11

library(forecast)
fc.rain<-forecast(fitx11,method = "naive",h=length(power.test))
fc.rain

library(Ryacas)
cat(yac_str("PrettyForm(Simplify( (1-(f1*B))*(1-(F1*B^(12)))*yt ))"))

cat(yac_str("PrettyForm(Simplify( (1+(h1*B)+(h2*B^(2)))*et ))"))




