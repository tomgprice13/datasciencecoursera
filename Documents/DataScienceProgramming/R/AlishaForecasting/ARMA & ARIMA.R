#information has been taken from the website http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

#ARIMA Modelling ## formal tests for stationarity can be found in funitroots package but isn't discussed below

#ARIMA models for non zero autocorrelations in the irregular component
#ARIMA models are defined for stationary time series, so if you have a non stationary time series
#you will need to difference it to obtain stationarity, can be done by using diff()
#use the data of the annual diameter of women's skirts from 1866 to 1911
skirts<- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries<-ts(skirts,start=c(1866))
plot.ts(skirtsseries)

#as can be seen this data does not look stationary, there are clear trends
#this will need to be differenced
skirtsseriesdiff1<-diff(skirtsseries,differences=1)
plot.ts(skirtsseriesdiff1)

#this graph still doesn't appear to stationary, so we can difference the data again
skirtsseriesdiff2<-diff(skirtsseries,differences=2)
plot.ts(skirtsseriesdiff2)

#in the ARIMA model we used a difference (d) of 2, which means we will now need
#to find the values of p and q in ARIMA (p,q,d)

#take the age of kings death for example
plot.ts(kingstimeseries)
#this data is not stationary either

#differencing it would yield the below
kingtimeseriesdiff1<-diff(kingstimeseries,differences=1)
plot.ts(kingtimeseriesdiff1)
#now the data is stationary in mean and variance and so an ARIMA(p,1,q) model
#would be appropriate
#the trend component has been removed from the time series of age of kings death
#and we are left with the irregular component, we can now examine whether there
#are correlations between the successive terms of the irregular component


#next to find the value of p and q we'll need to examine the correlogram and 
#partial correlogram functions
#to get the actual values of the autocorrelations and partial autocorrelations
#then use the "plot=FALSE" command int the function
acf(kingtimeseriesdiff1, lag.max=20)
acf(kingtimeseriesdiff1,lag.max=20, plot=F)
#as can be seen lag 1 breaches the confidence levels with -0.360 but all others are within them

#now plot the partial autocorrelogram
pacf(kingtimeseriesdiff1,lag.max=20)
pacf(kingtimeseriesdiff1,lag.max = 20,plot=F)

#the partial correlogram shows that the partial autocorrelation at lags 1,2 and 3
#exceed the significance bounds, are negative, and are slowly decreasing in magnitude
#with increasing lag (lag 1:-0.1360, lag 2:-0.335, lag 3:-0.321). the partial autocorrelations
#tail off to zero after 3

#there are three models available:

#1) an ARMA(3,0) autoregressive model of order p=3, as partial autocorrelogram
#is zero after lag 3, and correlogram tails off to zero after (possibly to abruptly to be appropriate)

#2) an ARMA(0,1) moving average model of order q=1, as autocorrelogram is zero after lag 1 and the
#correlogram tails off to zero

#3) an ARMA(p,q) a mixed model with p and q greater than 0 since the autocorrelogram 
#and partial autocorrelogram tail off to zero (though probably too abruptly)

#the first model has 2 parameters, the second has 1 parameter and the third has at
#least 2 parameters

#we use the principle of parsimony to decide which model is best, so we assume the model with fewest 
#parameters is best which is model 2

#a moving average model is usually used for short term dependencies between
#successive observations, as we might expect the age of death of a king may have
#influence over the next one or kings but nothing more than that

#since an ARMA(0,1) model is taken to be the best candidate to model the time series 
#of the differences of the ages at death of English kings then the original data can 
#be modelled with ARIMA(0,1,1)

#to check this there is a shortcut function which tells us what the best coefficients
#to use are for the ARIMA data

auto.arima(kings)

#which confirms ARIMA(0,1,1)


#to fit the ARIMA model do the following
kingstimeseriesarima<-arima(kingstimeseries,order=c(0,1,1))
kingstimeseriesarima

#if we are fitting an ARIMA(0,1,1) to our data it means that we are fitting an 
#ARMA(0,1) to the first differences of our data. In the ARAM model theta is the
#parameter being estimated which is given as mal from the above commands (-0.7218)

#now we can forecast future values of the model
kingstimeseriesforecast<-forecast.Arima(kingstimeseriesarima,h=5)
kingstimeseriesforecast

#the forecast gives the age of death of the next 5 kigns as 67.7 years, as well
#as the 80% and 95% confidence interval levels, this can be plotted by the below
plot.forecast(kingstimeseriesforecast)


#as with the case of the exponential smoothing, it is a good idea to check whether
#the forecast errors of the ARIMA model are normally distributed with mean 0 and 
#constant variance, and whether there are correlations between sucessive forecast errors

#we can make a correlogram of the forecast errors for the ARIMA(0,1,1) model
#for the ages at death of kings , and perform the Ljung-Box test for lags 1-20

acf(kingstimeseriesforecast$residuals,lag.max=20)
Box.test(kingstimeseriesforecast$residuals, lag=20, type="Ljung-Box")

#since the correlogram shows that none of the sample autocorrelations for lags 1-20
#exceed the significance bounds, and the p-value for the Ljung-Box test is 0.9,
#we can conclude there is very little evidence for non zero autocorrelations 
#in the forecast

#now we need to test whether the forecast errors are normally distributed with
#mean zero and constant variance by making a time plot

plot.ts(kingstimeseriesforecast$residuals)
plotforecasterrors(kingstimeseriesforecast$residuals)

###DIDNT WORK

#if the variance of forecast errors is roughly constant over time, and they are
#normally distributed with mean zero then the model would be a good predictor of 
#the data