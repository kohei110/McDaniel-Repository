
#
#Script for ANA 535 Laboratory #4 Step 1 
#
#Written by Marvine Hamner April 2025
#

library(xlsx)
library(fpp3)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(forecast)
library(lubridate)
library(zoo)
library(tseries)
# install.packages("pak")                  # if you don’t have it yet
# pak::pak("KevinKotze/tsm")
library(tsm)
library(ggpubr)

#
#******************************* Step 1a ************************
#

#
#First generate the data for white noise.  Use the rnorm() function
#to do that.  Use mean = 0, variance = 4.0, standard deviation = 2.0
#Generate 1,000 data points to start with. 

n = 1000
e = rnorm(n, mean = 0.0, sd = 4.0)

par(mfrow = c(1,1))
plot(e, type = "l")

#
#How do we handle time in this case?  Well, we can assume this is
#a signal we are sampling and just establish a sampling frequency
#to get time.  But, we can do this an easier way when we setup the
#time series object in R. We just need to get the sampling frequency 
#right. If we say we are sampling at 10 Hertz then we would have 100
#seconds of data.  Let's try that.  
#

e.ts <- ts(e, frequency = 10)
str(e.ts)
plot(e.ts, xlab = "Time (seconds)", ylab = "Magnitude", bty = "l")
autoplot(e.ts) +
  labs(title = "White-noise series e",
       x = "Time (seconds)",
       y = "Magnitude")
ggsave("Fig01_white_noise_ts.png", dpi = 300, width = 4, height = 3)

#
#Now, let's consider the ACF.  If the ACF has no significant spikes
#that would indicate that we should be able to fit an AR(0) model
#to these data.
#

ggAcf(e.ts) +
  ggtitle("Sample ACF for e sim data")
ggsave("Fig02_white_noise_acf.png", dpi = 300, width = 4, height = 3)

#
#This looks really good for white noise.  There are no significant
#spikes in the ACF. This looks good for an AR(0) model. What other
#diagnostic tests should we run?  Well... we should run a unit root
#test.  Section 9.7 in out textbook shows graphically the steps in
#time series analysis and forecasting. 
#

e.ts.adf <- adf.test(e.ts)
e.ts.adf

#
#Here we have used the Augmented Dickey-Fuller (unit root) test.  
#Since the p-value is less than 0.05 we can reject the null 
#hypothesis and conclude that the data are stationary.  Let's 
#try the KPSS test and see if, in this case, it is consistent
#with the ADF test.
#

e.ts.kpss <- kpss.test(e.ts)
e.ts.kpss

#
#The p-value for the kpss test is greater than 0.05 so we reject
#the null hypothesis and, yes, the result is consistent between
#the ADF test and the KPSS test.  When you are using these unit
#root tests remember that the null hypotheses are the opposite.
#For the ADF test the null hypothesis is that the data are non-
#stationary.  For the KPSS test the null hypothesis is that the
#data are stationary.  For the ADF test the p-value is 0.01 so we 
#reject the null hypothesis that the data are non-stationary, i.e
#we accept that there is evidence that the data are stationary.
#For the KPSS test the p-value is 0.10 or greater than 0.05 and
#we cannot reject the null hypothesis that the data are stationary.
#

#
#********************************** Step 1b **********************
#

#
#Let's look at another (artificial) dataset using the rnorm() 
#function to generate data. We want to generate a data that we
#can fit an AR(1) model to. 
#

y <- numeric(1000)
e2 <- rnorm(1000)
for(i in 2:1000)
  y[i] <- 0.6*y[i-1] + e[i]
e2.tsb <- tsibble(idx = seq_len(1000), y = y, index = idx)
e2.tsb
e2.ts <- ts(e2.tsb[,2], frequency = 10)

autoplot(e2.ts)
str(e2.ts)

#
#Now let's look at the ACF, and if necessary the PACF
#

e2Acf <- ggAcf(e2.ts) +
  ggtitle("Sample ACF for e2 sim data")
plot(e2Acf)

e2Pacf<- ggPacf(e2.ts) +
  ggtitle("Sample PACF for e2 sim data")
plot(e2Pacf)

#
#The ACF does quickly taper off to insignificant spikes.  This is
#the behavior we want to see in the ACF.  This tells us that an AR(1)
#model may be a good fit for these data.  Double-checking this result
#with the PACF we do see that only the first spike is significant.
#Therefore, I would try to fit an AR(1) model to these data first.
#

#
#What about stationarity?  Let's run the ADF and KPSS tests.
#

e2.ts.adf <- adf.test(e2.ts)
e2.ts.adf

e2.ts.kpss <- kpss.test(e2.ts)
e2.ts.kpss

#
#Once again our data appears to be stationary. The difference now
#is that we have an AR(1) or a lag=1 simulation rather than
#white noise.
#

#
#************************************ Step 1d ******************
#**********************************Note that this step and step 1c
#********************************are reversed in order in this script
#

#
#Moving along to MA(q) models.  What do you think we need to do to
#generate data that an MA(q) model would fit well? 
#
#We can write a function for this that makes it easier to change
#theta to see how the plot changes.  To consider how you want to 
#change theta look at the bottom of the page, Section 9.4. 
#First set theta equal to 0.6. 
#

e3.ts <- arima.sim(model = list(ma = 0.6), n = 1000)
plot.ts(e3.ts)
str(e3.ts)

#
#Compare to the AR(1) model with phi equal to 0.6.
#

e3Acf <- ggAcf(e3.ts) +
  ggtitle("Sample ACF for e3 sim data")
plot(e3Acf)

e3Pacf <- ggPacf(e3.ts) +
  ggtitle("Sample PACF for e3 sim data")
plot(e3Pacf)

#
#Don't forget to run the unit root tests!
#

e3.ts.adf <- adf.test(e3.ts)
e3.ts.adf

e3.ts.kpss <- kpss.test(e3.ts)
e3.ts.kpss

#
#Now let's compare the ACF and PACF for the simulated data e2 and 
#e3. To compare the plots its easier if they are all part of one
#figure. Because the plots were all generated with ggplot we cannot
#just use par(mfrow). To do this plotting I've installed the 
#ggpubr package and am using the ggarrange() command. But...
#just to make sure that the simulation generating the data is doing
#what we think it is let's use the tsm package to generate data
#for the AR(1) model too. 
#
install.packages("ggpubr")   # only the first time
library(ggpubr)              # every new session

e2b.ts <- arima.sim(model = list(ar = 0.6), n = 1000)
plot.ts(e2b.ts)
str(e2b.ts)

e2bAcf <- ggAcf(e2b.ts) +
  ggtitle("Sample ACF for e2 sim data")
plot(e2bAcf)

e2bPacf<- ggPacf(e2b.ts) +
  ggtitle("Sample PACF for e2 sim data")
plot(e2bPacf)

#
#Now put the plots together for a comparison
#

fig3 <- ggarrange(e2bAcf, e2bPacf, e3Acf, e3Pacf, ncol = 2, nrow = 2)
fig3
# ggsave("Fig03_ar_vs_ma.png", plot = fig3, dpi = 500, width = 4, height = 3)

#
#What I see is what I expected to see. The ACF falls off quickly
#for the simulated data for AR(1) while the ACF for the simulated
#data for MA(1) only has 1 large spike at lag = 1.  So, it seems
#that Kevin Kotzel has made a really nice package. What I wanted
#was the ability to easily generated simulated data for a variety
#of situations; AR(0), AR(1), and MA(1).  This we have now.
#

#
#***************************** Step 1c ***********************
#

#
#Now let's go back and pick up the AR(1) with drift model. Remember
#that the drift is added as a constant to the equation.  We'll add
#c = 10.0 for the drift. I played around with several different values
#for c but couldn't really see a lot of effect in the data or trend.
#It seems to me that adding a positive constant should just raise 
#the trend line overall. To really understand the effect of drift will
#take some more specialized study...
#

c = 10.0
y <- numeric(1000)
e2wd <- rnorm(1000)
for(i in 2:1000)
  y[i] <- c + 0.6*y[i-1] + e[i]
e2wd.tsb <- tsibble(idx = seq_len(1000), y = y, index = idx)
e2wd.tsb
e2wd.ts <- ts(e2wd.tsb[,2], frequency = 10)

autoplot(e2wd.ts)
str(e2wd.ts)

e2wd.comp <- decompose(e2wd.ts)
autoplot(e2wd.comp)

#
#Now let's look at the ACF, and if necessary the PACF
#

e2wdAcf <- ggAcf(e2.ts) +
  ggtitle("Sample ACF for e2 sim data")
plot(e2Acf)

e2wdPacf<- ggPacf(e2.ts) +
  ggtitle("Sample PACF for e2 sim data")
plot(e2Pacf)

#
#The ACF does quickly taper off to insignificant spikes.  This is
#the behavior we want to see in the ACF.  This tells us that an AR(1)
#model may be a good fit for these data.  Double-checking this result
#with the PACF we do see that only the first spike is significant.
#Therefore, I would try to fit an AR(1) model to these data first.
#

#
#Now put the AR(1) and AR(1) with drift plots together for a 
#comparison.
#

ggarrange(e2Acf, e2Pacf, e2wdAcf, e2wdPacf, ncol = 2, nrow = 2)

#
#What about stationarity?  Let's run the ADF and KPSS tests.
#

e2wd.ts.adf <- adf.test(e2wd.ts)
e2.ts.adf

e2wd.ts.kpss <- kpss.test(e2wd.ts)
e2wd.ts.kpss

#
#The thing I really see changing is the results of the unit root
#tests.  These are very different than the results before without
#drift.  Now, here is the case where the ADF test result is 
#inconsistent with the KPSS test result.  The ADF test says that we
#reject the null hypothesis (p-value = 0.01) and say there is 
#sufficient evidence to accept the alternate hypothesis.  That is, 
#that the data are stationary. But, the KPSS test result also says
#reject the null hypothesis.  Since these null hypotheses are 
#opposite we would need to get opposite test results to be 
#consistent.  In the KPSS test result we would accept the alternate
#hypothesis and say the data are non-stationary. This means we
#need more testing! 
#

#
#Here is a bit more practice.  Let's look at the results of some
#real-world data; earthquakes and Google stock closing prices.
#

#
#************************* Step 1e quakes data ******************
#

#
#Let's work a bit with the Earthquake data.  I've made the Excel
#spreadsheet I downloaded for earthquakes from 1933 to to 1994. 
#There are almost 16,000 observations in this dataset. But many of
#them are related to the same earthquake.  I started by taking the
#first observation of every earthquake that was obviously different
#than the one before it.  I got 297 observations from 1933 to 1976
#and stopped due to the time available. It is an interesting
#dataset.  Let's see what we can tell from it.
#

quakes.v2.txt <- read.csv("/Users/nkohei/Workspace/McDaniel-Repository/535/lab4/quakes.csv")
quakes <- quakes.v2.txt
View(quakes)
colnames(quakes) <- c("Year", "Mag", "Name", "State", "Country")

plot(quakes[,2], type = "l")
quakes.ts <- ts(quakes[,2])
str(quakes.ts)

#
#There isn't any data in the first record so we'll eliminate it.
#

quakes.ts <- ts(quakes.ts[2:297])
str(quakes.ts)

plot(quakes.ts, bty = "l")

par(mfrow=c(1,2))
quakes_tslm <- tslm(quakes.ts ~ trend)
quakes_tslm
plot(quakes.ts, ylab = "Magnitude", bty = "l")
lines(quakes_tslm$fitted, lwd = 2, col = "red")

quakes_tslm2 <- tslm(quakes.ts ~ trend + I(trend^2))
plot(quakes.ts, ylab = "Magnitude", bty = "l")
lines(quakes_tslm2$fitted, lwd = 2, col = "blue")


df_quakes <- tibble(idx = seq_along(quakes.ts),
                    mag = as.numeric(quakes.ts),
                    lin_fit  = fitted(quakes_tslm),
                    quad_fit = fitted(quakes_tslm2))

plot_lin <- ggplot(df_quakes, aes(idx, mag)) +
  geom_line() +
  geom_line(aes(y = lin_fit), colour = "red", linewidth = 1) +
  labs(title = "Linear trend fit",  y = "Magnitude") +
  theme_bw()

plot_quad <- ggplot(df_quakes, aes(idx, mag)) +
  geom_line() +
  geom_line(aes(y = quad_fit), colour = "blue", linewidth = 1) +
  labs(title = "Quadratic trend fit", y = "Magnitude") +
  theme_bw()

plot_acf  <- ggAcf(quakes.ts)  + ggtitle("Sample ACF for Quake Magnitudes")
plot_pacf <- ggPacf(quakes.ts) + ggtitle("Sample PACF for Quake Magnitudes")

fig5 <- ggarrange(plot_lin, plot_quad, plot_acf, plot_pacf,
                  ncol = 2, nrow = 2)

fig5

#A really big problem with these data are that the time interval
#is not the same between each earthquake, nor should it be. But
#our codes are made to do time series analyses on times that are
#equal between data points.  So, we'll see what we can get 
#but I wouldn't put too much into this.
#

ggAcf(quakes.ts) +
  ggtitle("Sample ACF for Quake Magnitudes")

ggPacf(quakes.ts) +
  ggtitle("Sample PACF for Quake Magnitudes")

#
#This looks an awful lot like the e2 simulated data, i.e. the
#data we can fit an AR(1) model with and without drift to.  Let's 
#see if we can continue to blissfully ignore this big time problem 
#and decompose these data. And, no! We run into trouble because of
#the time problem here. We cannot specify a correct frequency.
#Without that there is no way for the code to compute a trend.
#

quakes.ts.comp <- decompose(quakes.ts) #intentionally failed?
autoplot(quakes.ts.comp) 

#
#Penn State's online course STAT 510 does go ahead and make some
#observations from just looking at the time plot of the data. I
#don't know if I would do that or not.  I cannot really see if
#there is any periodicity in these data.  And, as Penn State says
#there do tend to be swarms of earthquakes so that sometimes there
#is some autocorrelation in the data. 
#

#
#*********************** Step 1f Google data***********************
#

#
#Let's start looking at another dataset, i.e. the data for Google's
#closing stock prices for the last 5 years.  I've also uploaded that
#data file for you on Blackboard. Unfortunately, we are going to have
#some of the same trouble with the Google data that we had with the
#earthquake data.  That is, the time intervals are not equal. Stock
#prices are recorded daily for every day the stock market is open.
#But, the stock market is not open every day of the year.  Let's see
#how far we can get.  
#

GoogleStockData.5yr.v2 <- read.csv("/Users/nkohei/Workspace/McDaniel-Repository/535/lab4/GoogleStockData-5yr.csv")
google <- GoogleStockData.5yr.v2
View(google)
colnames(google) <- c("Date", "Close", "Volume", "Open", "High", "Low")
str(google)

#
#Change the Date variable from a char format to a Date format.  This
#is going to be a bit difficult because the time intervals in the 
#data are not equal.  The stock market doesn't operate 7 days per
#week. So, we'll have to see how to best handle that.  
#

google$Date <- mdy(google$Date)
str(google)
head(google)

google.ts <- ts(google$Close)
str(google.ts)
par(mfrow = c(1,1))
plot(google.ts, ylab = "Google Closing Price", xlab = "Time", bty = "l")

#
#Let's see what the Augmented Dickey-Fuller (unit root) test tells us.
#

google.adf <- adf.test(google.ts)
google.adf

#
#The p-value is 0.6336 which is greater than 0.05 we cannot reject the
#null hypothesis which in the ADF test is that the time series is non-
#stationary.  So, we will need to do something to make the time series
#stationary before we can proceed with our time series analysis. What
#about the ACF?
#

ggAcf(google.ts) +
  ggtitle("Sample ACF for Google Stock Closing Prices")

ggPacf(google.ts) +
  ggtitle("Sample PACF for Google Stock Closing Prices")


#
#This is somewhat inconsistent.  The ACF plot indicates that the data
#are non-stationary which agrees with the ADF test results.  But, the
#PACF test indicates that an AR(1) model may fit the data well.  
#But, we know that to use an AR(p) model the data MUST be stationary!
#



#
#*************************************Step 1g **********************
#

#
#Let's see the the auto ARIMA() function returns the same model
#parameters we have decided to use in the first parts of Step 1. We
#know the simulated data for "e" is white noise so we won't work
#on that particular case.  Let's start with e2. 
#

#
#First we'll plot the data again.
#

e2.tsb |> 
  autoplot() +
  labs(title = "Simulated data 'e2'", y = "Magnitude of sim")

#
#The data goes up and down.  If we add a trendline we can see that more
#easily.  At any rate, it does seem to be stationary. Let's go to the
#next step and look at the difference and the ACF and PACF plots.
#

e2.tsb |>
  gg_tsdisplay(difference(y), plot_type = 'partial')

e2_fit <- e2.tsb |>
  model(stepwise = ARIMA(y),
        search = ARIMA(y, stepwise = FALSE))

e2_fit

e2.tsb |>
  gg_tsdisplay(difference(y), plot_type = "partial")

# auto-ARIMA, stepwise and exhaustive
e2_fit <- e2.tsb |>
  model(stepwise = ARIMA(y),
        search   = ARIMA(y, stepwise = FALSE))

report(e2_fit)          # confirms ARIMA(1,0,0)

#
#When you print out the ARIMA automatic models the parameters p, d, and
#q are the same as those we determined when we first examined the
#ACF and PACF for e2.  Let's try the MA(1) model data now, the e3 data.
#
e3.tsb <- tsibble(
  idx = seq_len(length(e3.ts)),        
  y   = as.numeric(e3.ts),             
  index = idx                          
)

e3.tsb |> 
  autoplot() +
  labs(title = "Simulated data 'e3'", y = "Magnitude of sim")

e3.tsb |>
  gg_tsdisplay(difference(y), plot_type = 'partial')

e3_fit <- e3.tsb |>
  model(arima001 = ARIMA(y ~ pdq(0,0,1)),
        stepwise = ARIMA(y),
        search = ARIMA(y, stepwise=FALSE))

report(e3_fit)

#
#This is interesting.  The ACF plot shows the same large (but negative)
#spike at lag = 1.  However, the automatic ARIMA code doesn't pick this up
#as an MA(1) model. It looks like all the models built are very close in
#terms of accuracy.  ARIMA(0,0,0) is again AR(0) or white noise.  So,
#caveat emptor!  When using automatic codes it is always best to double-
#check their output against something you know is true.
#

#
#Let's look at the quakes data using the automatic ARMIA code. Remember
#that we had determined the quakes data were best fit with an AR(1)
#model.  
#

quakes.tsb <- as_tsibble(quakes.ts)

quakes.tsb |> 
  autoplot() +
  labs(title = "Differences for Earthquake Data", y = "")

#
#The data goes up and down.  If we add a trendline we can see that more
#easily.  At any rate, it does seem to be stationary. Let's go to the
#next step and look at the difference and the ACF and PACF plots. Note
#that the variables in the tsibble are indx and value. So use value in
#the commands in the code this time.  Also, for some reason the first
#observation value is "NA" so I just started from the 2nd observation.
#

quakes.tsb |>
  gg_tsdisplay(difference(value[2:297]), plot_type = 'partial')

quakes_fit <- quakes.tsb |>
  model(arima100 = ARIMA(value[2:297] ~ pdq(1,1,0)),
        stepwise = ARIMA(value[2:297]),
        search = ARIMA(value[2:297], stepwise = FALSE, approximation = FALSE))

report(quakes_fit)

#
#We get the same answer whether we run a stepwise solution or just do
#a search.  Both the stepwise and the search return ARIMA models that are 
#better than the AR(1) model or the AR(1) with 1 diference, i.e. setting
#d = 1 in the command.  It looks like we've got an ARIMA(0,0,3) model as
#best. Try playing around a bit with this and see what you can find! Now




#let's try the Google data. Remember that we found the Google data were
#best fit with an AR(1) model.  However, because it was non-stationary
#we will likely have to take at least 1 difference.
#

google.tsb <- as_tsibble(google.ts)

google.tsb |> 
  autoplot() +
  labs(title = "Differences for Google Closing Data", y = "")

#
#The data goes up and down.  If we add a trendline we can see that more
#easily.  At any rate, it does seem to be stationary. Let's go to the
#next step and look at the difference and the ACF and PACF plots.
#

google.tsb |>
  gg_tsdisplay(difference(value), plot_type = 'partial')

google_fit <- google.tsb |>
  model(arima110 = ARIMA(value ~ pdq(1,1,0)),
        arima111 = ARIMA(value ~ pdq(1,1,1)),
        arima310 = ARIMA(value ~ pdq(3,1,0)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise = FALSE, approximation = FALSE))

report(google_fit)


#
#I tried this taking 1 difference and then 2 differences and both gave
#the same result ARIMA(1,1 or 2, 0).  Since both the ACF and PACF show a
#significant lag at 3, I tried ARIMA(3,1,0) which was worse and actually
#both the automatic answers were worse too. I don't know why they would
#have changed.  But it looks like the ARIMA(1,1,1) has the best AICc.
#


