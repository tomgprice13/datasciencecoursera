#loading time series data of the age of kings
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
?ts
kingstimeseries<-ts(kings)
kingstimeseries

#you can set the frequency of time series for monthly frequency=12, quarterly frequency=4
#and we can also set the start date, and if it start in either the second month or quarter of 1985
#then we'd use c(1985,2)
#the following data shows the births in new york between Jan 1946 and Dec 1959
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

#similarly the next data set conatains monthly sales for a souvenir shop in Queensland Australia
#from Jan 1987 to Dec 1993
souvenir<-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries<-ts(souvenir,frequency=12,start=c(1987,1))
souvenirtimeseries

#to plot the time series data use the following
plot.ts(kingstimeseries)
#the above plot looks like an additive tims seires model can be used as the fluctuations 
#are fairly constant over time

plot.ts(birthstimeseries)
#the above plot shows seasonality as there dips every winter and peaks every summer, 
#it can also use an additive time series model

plot.ts(souvenirtimeseries)
#for this data an additive model would not be applicable as the seasonal variations increase
#over time, so it will need to be transformed and then described by an additive model
logsouvenirtimesseries<-log(souvenirtimeseries)
plot.ts(logsouvenirtimesseries)

#the seasonal and random fluctuations now appear to be constant over time and so an additive model can be used


#the data needs to be seperated into it's constiuent components trend, irregular and seasonal components

####first start off with the non seasonal data
#the time series needs to be broken up into the irregular and trend components using an additive model
#a smoothing method can be used to calculate the simple moving average of the time series

install.packages("TTR")
library("TTR")

#with the above package I'll be able to use the SMA function

#from the above data the age of kings deaths was non seasonal and can probably be described
#using an additive modelsince the fluctuations are constant over time

#we can try and estimate the trend component of the age of kings data by using SMA
#we'll use an arbitrary order of 3 to start with
kingstimeseriesSMA3<-SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

#there still appears to be a fair amount of rnadom fluctuations, therefore we should use 
#a higher order of the SMA to try and smooth it out, this is just a trial and
#error process
kingstimeseriesSMA8<-SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

#that's a better model and this shows us that after the first 20 kings their age of death
#fell from about 55 years old to about 38 years old, and then by the end of the reign
#at the 40th king it had increased to around 73 years old


#####now moving on to decomposing seasonal data
#now we need to estimate all three components, irregular, seasonal and trend
#for an additive model the function decompose() can be used to achieve this
#as discussed above the births in new york city would be good data to model using this

birthstimeseriescomponents<-decompose(birthstimeseries)

#this now stores each of the compnents in it's own variable birthstimeseriescomponents$seasonal,
#birthstimeseriescomponents$trend and birthstimeseriescomponents$random

birthstimeseriescomponents$seasonal

#this shows that the seasonal factors are the same for each year, the largest factor is
#July and the lowest in February indicating there are peak births in July and
#peak troughs in February
#we can plot the individual components on a graph
plot(birthstimeseriescomponents)
#this shows that the birth rate is generally trending upwards

#it is possible to seasonally adjust the data by removing the seasonal component from 
#the original data
birthstimeseriesseasonallyadjusted<-birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
#you can see the seasonality has been taken out just leaving the trend and irregular components

