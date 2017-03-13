#more time series analysis with R link: http://www.rdatamining.com/docs/time-series-analysis-and-mining-with-r

#plot the air passengers data
plot(AirPassengers)

apts<-ts(AirPassengers,frequency=12)
plot(apts)

#decompose the air passenger data
f<-decompose(apts)
summary(f)

plot(f$figure)
plot(f$figure,type="b")

plot(f)


##forecasting with ARIMA

#build ARIMA model
?arima
fit<-arima(AirPassengers,order=c(1,0,0),list(order=c(2,1,0),period=12))
fore<-predict(fit,n.ahead=24)

#error bounds at 95% confidence level
U<-fore$pred + 2* fore$se
L<-fore$pred - 2* fore$se

ts.plot(AirPassengers, fore$pred, U, L,
        col=c(1,2,4,4), lty=c(1,1,2,2))
legend("topleft", col=c(1,2,4), lty=c(1,1,2),
       c("Actual","Forecast", "Error Bounds (95% CI)"))


