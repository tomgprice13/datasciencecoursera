##Exponential Smoothing
#if you have a time series which can be described by an additive model with no trend 
#and no seasonality then simple exponential smoothing can be made for short term forecasts
#the smoothing is controlled by parameter alpha which lies between 0 and 1
#the closer to zero it is means little weight is given to the most recent observations
#so using the dataset of annual rainfall in London from 1813 to 1912
rain<-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries<-ts(rain,start = c(1813))
plot.ts(rainseries)
#as can be seen fluctuations remain fairly constant over time and there is no seasonality or trend

#the holt winters smoothing method can be used to model this, but beta and gamma will need
#to be set to zero as these are for the exponential smoothing method

rainseriesforecasts<-HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
#the output shows alpha as 0.024 which means little weight is given to recent observations
#right now the orginal data covered 1813 to 1912 so this is when the forecasts are for at the moment
#the frecats made by holt winters during this period will be under the "fitted" variable
rainseriesforecasts$fitted

#we can now plot both of these by using the following
plot(rainseriesforecasts)

#the forecats line (red) is a lot smoother than the original data (black)
#to measure the accuracy we can calculate the sum of squared errors for the in sample forecast errors
#this is stored as a variable "SSE"
rainseriesforecasts$SSE

#we can also make the holt winters start from a certain point, the first point is 23.56 so we'll use this
HoltWinters(rainseries, beta=F, gamma=F, start=23.56)

#to forecast points in the future we'll need to install the R "forecast" package

install.packages("forecast")
library("forecast")

#now we can make forecast on so many observations in the future

rainseriesforecasts2<-forecast.HoltWinters(rainseriesforecasts,h=8)
rainseriesforecasts2
#this gives us forecasts for the next 8 years, and also a 80% and 95% confidence level interval
#to plot the original data and the forecasted data use the following

plot.forecast(rainseriesforecasts2)
#the blue line gives us the forecasted figures as well as the shaded areas represeneting the CI levels

#as mentioned before the SSE are a measure of accuracy, however, there should be no 
#correlation between successive predictions, if there is any correlation between them then
#the forecasts could probably be imporved by another technique i.e. exponential or something else

#to find this out we can plot a correlogram of the forecast errors for the last so many lags 
#to do this we can use the acf() function and specify how many lags
acf(rainseriesforecasts2$residuals, lag.max = 20)
#this data contains missing variables, so need to ignore the NA values
acf(rainseriesforecasts2$residuals, lag.max = 20, na.action = na.pass)

#as can be seen lag 3 is touching the significance bounderies
#to test whether there is significant evidencde from non zero correlations at
#lags 1-20 then we can carry out Ljung-Box test, essentially testing for autocorrelation
#for the in sample forecast errors for london rainfall
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

#with the o-value of 0.6 this shows that there is little evidence of non zero autocorrelations 
#in the forecast errors

#to be sure there is no autocorrelation in the data and the model can't be imporved upon
#it is worth checking to see if the forecast errors are normally distributed with mean zero 
#and constant variance

#to check the constant variance, we can make a time plot of the in sample forecast errors
plot.ts(rainseriesforecasts2$residuals)
#this plot shows the in sample forecasts errors have roughly constant variance
#although the fluctuations do change in size the vairance remains fairly constant

#to check whether the forecats errors are normally distributed with mean zero 
#we can plot a histogram  of the forecast errors with an overlaid normal curve 
#with mean zero and the same standard deviation as the distribution of forecast errors
#to do this we must define a plotforecasterrors() function
plotforecasterrors<-function(forecasterrors, na.rm=TRUE){
  #make a histogram of the forecast errors
  mybinsize<-IQR(forecasterrors,na.rm = na.rm)/4
  mysd<-sd(forecasterrors,na.rm = na.rm)
  mymin<-min(forecasterrors,na.rm = na.rm)-mysd*5
  mymax<-max(forecasterrors,na.rm = na.rm)+mysd*3
  #generate normally distributed data with mean zero and stan dev mysd
  mynorm<-rnorm(1000,mean=0, sd=mysd)
  mymin2<-min(mynorm)
  mymax2<-max(mynorm)
  if(mymin2 < mymin) {mymin<-mymin2}
  if(mymax2>mymax) {mymax<-mymax2}
  #make a red histogram of the forecast errors, with the normally distributed data overlaid
  mybins<-seq(mymin,mymax,mybinsize)
  hist(forecasterrors, col="red",freq=F,breaks=mybins,na.rm = na.rm)
  #freq=FALSE ensures the area under the histogram =1
  #generate normally distributed data with mean 0 and standard deviation mysd
  myhist<-hist(mynorm, plot=F, breaks=mybins,na.rm=na.rm)
  #plot the  normal curve as a blue line on top of histogram of errors
  points(myhist$mids, myhist$density, type="1", col="blue", lwd=2,na.rm=na.rm)
}
plotforecasterrors(rainseriesforecasts2$residuals,na.rm = T)

##DIDN'T QUITE WORK

