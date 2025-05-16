#
#Lab2Complete.R
#Written by Marvine Hamner
#February 25, 2025
#

install.packages(c("xlsx", "fpp3", "dplyr", "tidyverse", "ggplot2", "tsibble", 
                   "tsibbledata", "fable", "feasts", "lubridate", "zoo", 
                   "forecast", "seasonal"))

library(xlsx)
library(fpp3)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(lubridate)
library(zoo)
library(forecast)
library(seasonal)

#
#********************** Step 1 *************************
#

# First complete the analysis of a simple sine wave at a specified freq.
# Setup the parameters.  You are given that the data are collected at
# a sampling frequency (Fs) of 1,000 Hertz.  Hertz are cycles per second.
# That is am important distinction.  Later we will compute frequencies and
# need to put time in the denominator, i.e. 1/time.  The period, T, is 
# 1/Fs.There are 1,500 data points available. The independent variable is 
# time (t).  
#

Fs <- 1000
T = 1/Fs
L = 1500
t = (0:L-1)*T

#
#Generate the data and plot it
#
par(mfrow = c(2,1))

Sig1 <- sin(2*pi*120*t)
plot(1000*t(1:100), Sig1[1:100], type = "l", main = "120 Hz plot")

# test
# plot(1000*t, Sig1, type = "l")

#
#Conduct the FFT and plot the resulting power spectrum
#

Y <- fft(Sig1)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)
plot(f, P1, type = "l", main = "Fast Fourier Transform")

#
#There are several ways you can look at the energy in the frequency
# spectrum.  One way is to generate a perriodogram.  Another way
# is to generate the frequency spectrum.  First, we'll look at
#the periodogram.
#


library(knitr)
library(TSA)

p <- periodogram(Sig1)
dd <- data.frame(freq = p$freq, spec = p$spec)
ordered_dd <- dd[order(-dd$spec), ]
top10 <- head(ordered_dd, 10)

kable(top10, digits = 4, caption = "Top 3 Frequencies from Periodogram")
# plot(periodogram(Sig1))

#
# If you multiply the frequency output by the TSA periodogram by
# the sampling frequency you'll get 119.7917 or about 120 Hz
# which is the original signal frequency.  The periodogram is great 
# for a univariate signal.  It does not work well for complex signals 
# composed of many frequencies.  
#

#
#********************** Step 2 ************************
#

#Now work on the combined, complex wave problem
#wave1 has a freq of 100 and amplitude of 5
#wave2 has a freq of 200 and amplitude of 10
#wave3 has a freq of 400 and amplitude of 15
#

#
#Setup the parameters
#

Fs <- 1000
T = 1/Fs
L = 1500
t = (0:L-1)*T

#
#Generate the data and plot it
#

wave1 <- 5*sin(2*pi*100*t)
wave2 <- 10*sin(2*pi*200*t)
wave3 <- 15*sin(2*pi*400*t)

par(mfrow = c(2,2))

plot(1000*t(1:100), wave1[1:100], type = "l", main = '100Hz and amplitude of 5')
plot(1000*t(1:100), wave2[1:100], type = "l" ,main = '200Hz and amplitude of 10')
plot(1000*t(1:100), wave3[1:100], type = "l",main = '400Hz and amplitude of 15')

#
#Combine the three waves into one complex wave and plot
#

wave4 <- wave1 + wave2 + wave3
plot(1000*t(1:100), wave4[1:100], type = "l", 
     main = "Complex, Combined Wave Time Plot")

#
#******************** Step 3 *************************
#

#
#Now let's look at what a phase shift does.  A phase shift by pi will setup
#destructive interference.  First, setup a sequence of points over 180 degrees.
#

xs <- seq(-100*pi, 100*pi, pi/100)
min(xs)
#
#Then, set up the sine waves. And, just to make things interesting
#let's combine them into a complex wave. 
#
par(mfrow = c(3,1))
wavea <- sin(0.1*xs)
plot(xs,wavea)
waveb <- sin(0.333*xs)
plot(xs,waveb)
wavec <- wavea + waveb
plot(xs,wavec)

par(mfrow = c(3,1))
plot(xs,wavea,type="l", ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,waveb,type="l", ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wavec,type="l", ylim=c(-2,2)); abline(h=0,lty=3)

#
#Next setup a phase shift by 180 degrees or pi.
#

wavea2 <- sin(0.1*xs + pi)
waveb2 <- sin(0.333*xs + pi)
wavec2 <- wavea2 + waveb2


# test 
par(mfrow = c(3,1))
plot(xs, wavea, type = "l", 
     ylim = c(-1, 1), 
     xlab = "xs", 
     ylab = "Amplitude",
     main = "WaveA: Original vs Phase-Shifted (π) on One Chart",
     col = "red")

# Add the second wave using lines()
lines(xs, wavea2, col = "blue")

# Optional horizontal line at 0
abline(h = 0, lty = 3)


plot(xs, waveb, type = "l", 
     ylim = c(-1, 1), 
     xlab = "xs", 
     ylab = "Amplitude",
     main = "WaveB: Original vs Phase-Shifted (π) on One Chart",
     col = "red")

# Add the second wave using lines()
lines(xs, waveb2, col = "blue")
abline(h = 0, lty = 3)

plot(xs, wavec, type = "l", 
     ylim = c(-2, 2), 
     xlab = "xs", 
     ylab = "Amplitude",
     main = "WaveC: Original vs Phase-Shifted (π) on One Chart",
     col = "red")
# Add the second wave using lines()
lines(xs, wavec2, col = "blue")
abline(h = 0, lty = 3)
# test



#
#Now let's plot the original complex wave, the wave shifted by pi
#or 180 degrees, and the combination of these two waves. As you 
#can see, the combination of the original wave and the phase
#shifted wave is zero, total destructive interference.  
#

par(mfrow = c(3,1))

plot(xs,wavec,type="l", ylim=c(-2,2), main = "WaveC: Original"); abline(h=0,lty=3)
plot(xs,wavec2,type="l", ylim=c(-2,2), main = "WaveC: Phase-Shifted (π)"); abline(h=0,lty=3)
waved <- wavec + wavec2 # flattened because of phase shift
plot(xs,waved,type="l", ylim=c(-1,1), main = "WaveC: Cancelled"); abline(h=0,lty=3)

#
#Conduct the fft analysis.  But now, since we have several waves
#combined, we'll  plot the resulting power spectrum rather than a periodogram.  
#

Y <- fft(wave4)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)

par(mfrow = c(1,1))
plot(f, P1, type = "l", main = "Combined Wave Power Spectrum",
     xlab = "Frequency", ylab = "Strength")

#
#******************** Additional example from real-world
#******************** signal acquisition for air pollution
#******************** due to street traffic. 
#

#
#Second complete the analysis of a CO signal from an air monitoring
#station by a roadway in Italy. 
#

#
#Setup the parameters.  Note that the data were acquired hourly so the sampling
#frequency is (60 sec * 60 min = 3600 or 0.000278 Hertz). 
#

Fs <- 0.000278
T = 1/Fs
L = 8760
t = (0:L-1)*T

#
#Conduct the fft analysis and plot the resulting power spectrum
#
#Read in the data and add variable names
#
setwd("/Users/nkohei/Workspace/McDaniel-Repository/535/lab2")
COandNOx2 <- read.csv("COandNOx2.txt", header=TRUE)
# colnames(COandNOx2) <- c("CO.GT", "NOx.GT", "T", "RH", "AH")

Y <- fft(COandNOx2$CO.GT)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)
par(mfrow = c(1,1))
plot(f, P1, type = "l", ylim = c(0, 1),
     main = "CO Power Spectrum", xlab = "Frequency", ylab = "Strength")

#
#Zoom into power spectrum
#

plot(f, P1, type = "l", xlim = c(0, 0.00002), ylim = c(0, 1),
     main = "CO Power Spectrum", xlab = "Frequency", ylab = "Strength")


#
#If there is a daily (24-hour) periodicity we would expect a spike at
#24*60*60 or 1.157 x e-005 which we see when we zoom into the power
#spectrum.  
#
plot(f, P1, type = "l",
     main = "Power Spectrum Zoomed Around Daily Frequency",
     xlab = "Frequency (Hz)",
     ylab = "Amplitude (or Strength)",
     xlim = c(0, 2e-5)   # Zoom the x-axis to 0..2e-5
)


library(TSA)

# Suppose 'AmtrakMiles' is your time series of monthly passenger miles
# or 'COdata' is your CO values. 
# Then you do the same approach:

p <- periodogram(COandNOx2$CO.GT,main= "CO Periodogram" )   # or periodogram(COdata)

# Combine freq & spec
dd <- data.frame(freq = p$freq, spec = p$spec)

# Sort by descending spec
dd_sorted <- dd[order(-dd$spec), ]
top1 <- head(dd_sorted, 3)

kable(top1, digits = 4, caption = "Top 3 Frequencies from Periodogram")




# 1/0.0416666667
# 1/0.0833333333
# 1/0.0001111111


#
#*********************** Step 4 ***********************
#

#
#Now work on the Amtrak data.  If you do not still have it in
#R/RStudio you can use the following lines to read it in again. You'll
#need to rewrite the path in this command to the directory/folder where
#you have stored the Amtrak data.  For example, the commands and the 
#path to my directory is
#setwd("I:/My Passport Documents/McDaniel/DataAnalytics/ANA535/NewANA535/Laboratories")
#Amtrak <- read.csv("Amtrak1991-2024.csv")
#colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
#
#If you have set up different working directories for each of the
#laboratories you might need to change from where you stored the
#dataset in the "base" directory to the individual laboratory as
#follows.
#
#setwd("I:/My Passport Documents/McDaniel/DataAnalytics/ANA535/NewANA535/Laboratories")
#
#and then have sub-directories or sub-folders under this, e.g. where 
#Laboratory2 is this particular laboratory.
#


Amtrak <- read.csv("Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)

Amtrak$Month <- mdy(Amtrak$Month)
Amtrak
str(Amtrak)

#Amtrak$Month <- zoo::as.yearmon(Amtrak$Month)
#Amtrak 

#Amtrak |>
#  mutate(Month = yearmonth(Month)) |>
#  as_tsibble(index = Month)


#
#Setup the parameters
#

Fs <- 1/(30.4*24*60*60)
T = 1/Fs
L = 402
t = (0:L-1)*T

#
#Conduct the fft analysis and plot the resulting power spectrum
#

Y <- fft(Amtrak$PassengerMiles)
P2 <- abs(Y/L)
m <- (length(Y)/2)+1
P1 <- P2[1:m]
P1 <- 2*P1
f = Fs*(0:(L/2)/L)

#options(scipen = 999)  # Larger number = less scientific notation
options(scipen = 0)


par(mfrow = c(1,1))
plot(f, P1, type = "l", xlim = c(0,0.0000002), ylim = c(0,90000000),
     main = "Amtrak time series power spectrum", xlab = "freq in Hz")

#
# To look for annual periodicity we need to look for a spike at 
# 1/(365 * 24 * 60 * 60 = 3.1536 x e07) or 3.1710 x e-08 and there is a nice
# large spike at about 3 x e-08.  
#

# Because months have different numbers of days and so on, looking at plots on
# this scale is a really crude approximation.  The main point is that there are 
# multiple periods in the Amtrak data as we will see in the time series
# decomposition.  
#

#
# Code to analyze Amtrak example/case from Practical Time Series Forecasting
#  with R: A Hands-on Guide (G. Shmueli and K.C. Lichtendahl Jr.) and fpp3
#

#
#Plot Amtrak data in time plot
#

ggplot(data = Amtrak) +
  geom_line(mapping = aes(x = Month, y = PassengerMiles)) +
              labs(title = "Amtrak Passenger Miles by Month")

#
# The data is rough.  First, the drop due to the Covid lockdowns is very obvious. 
# There is also a small dip from the 2008 financial meltdown. It looks like there 
# may be a seasonal component...  First, split out the desired variable (Ridership
# or PassengerMiles) numbers as a time series. 
#

PassengerMiles.ts <- ts(Amtrak$PassengerMiles, start = c(1991,1), end = c(2024, 6), frequency = 12)
Amtrak.comp <- decompose(PassengerMiles.ts)
par(mfrow = c(4,1))
autoplot(Amtrak.comp) +
  labs(title = 'Amtrak Passenger Miles Decomposition')
  # + rect(0, 1.1, 1, 1.7, xpd=TRUE, col="white", border="white")
  # + title("Amtrak Passenger Miles Decomposition")

#
#Let's look at just the seasonal component of the Amtrak data by 
#zooming into two years from that single plot
#

par(mfrow = c(1,1))
plot(Amtrak.comp$seasonal, xlim = c(1994, 1997))

#
#As we saw when we looked at the power spectrum of the Amtrak data,
#there are a number of periods in the data, e.g. annual or every year periodicity
#is pretty obvious but the data also repeat or have periodicity every 6-months,  
#every roughly 3 months, etc.  
#

#
#Let's look at what a seasonal adjustment would be for the Amtrak data. 
#Keep in mind that there are many ways to do this. I'll show you three ways
#here. Let's start with a tried and true method by using lags.  I know we 
#haven't covered lags yet but we will.  Just keep this in mind for later 
#when you'll use it more extensively.  
#


PassengerMiles.ts <- ts(Amtrak$PassengerMiles, start = c(1991,1), end = c(2024, 6), frequency = 12)

par(mfrow = c(1,1))

#
#The first way is to use the diff() command from the forecast package in R
#You can use either the syntax to find values of components of a time series 
#with a lag = 12 first, or just put all the commands into the plot() command 
#directly.
#
#The best webpage I've found with examples of the diff() command is at
#https://atsa-es.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html
#

par(mfrow = c(1,2))
d12 <- diff(PassengerMiles.ts, lag = 12)

plot(d12, ylab = "Lag-12, 
     then Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2024.25), 
     main = "Differenced (Lag-12)")

plot(diff(PassengerMiles.ts, lag = 12), ylab = "Lag-12, 
     then Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2024.25), 
     main = "Differenced (Lag-12)")

#
#You can see from the side-by-side plots that either syntax returns the same
#result.
#

par(mfrow=c(1,2))
plot(diff(diff(PassengerMiles.ts, lag = 12), lag = 6), ylab = "Lag-12, 
     then Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2024.25), 
     main = "Twice-Differenced (Lag-12, Lag-1)")

plot(diff(diff(diff(PassengerMiles.ts, lag = 12), lag = 6), lag = 3), ylab = "Lag-12, 
     then Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2024.25), 
     main = "Three times-Differenced (Lag-12, Lag-1, Lag-6)")

#
#Here is the 2nd way to make a "seasonal adjustment" to time series data. It uses
#data from the decompose() command. 
#

PassengerMiles.SeasAdj <- PassengerMiles.ts - Amtrak.comp$seasonal 

par(mfrow = c(2,2))
plot(PassengerMiles.ts, xlab = "Time", ylab = "Passenger Miles", main = "Original",type = "l")
plot(PassengerMiles.SeasAdj, xlab = "Time", , main = "Seasonally adjusted",ylab = "Passenger Miles", type = "l")
PassengerMiles.seas.zoom <- window(PassengerMiles.SeasAdj, start = c(1997, 1), end = c(2000, 12))
plot(PassengerMiles.seas.zoom, xlab = "Time", ylab = "Seasonally adjusted passenger miles",main = "Zoomed-in", type = "l")
par(mfrow = c(1,1))

#
#Again, there doesn't appear to be a obvious periodicity in this zoomed in plot.
#


# p_adj <- periodogram(PassengerMiles.SeasAdj)
# plot(p_adj)

#
#Here is the 3rd way to make a seasonal adjustment.  It is from your textbook,
#Section 3.5. To use this you will need to install the "seasonal" package in R/
#RStudio. Also, to use the "seasonal" package we need to transform out time
#series data from .ts into a tsibble. 
#

PassengerMiles.tsb <- as_tsibble(PassengerMiles.ts)
PassengerMiles.tsb

x11_dcmp <- PassengerMiles.tsb |>
  model(x11 = X_13ARIMA_SEATS(value ~ x11())) |>
  components()

autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total Amtrak Passenger Miles using X-11.")

#
#This decomposition looks a little different than the decomposition using the
#decompose() command.  Notice the spike in the trend data just before the year 
#2015?  It isn't nearly as sharp of a spike in the trend using the decompose()
#command from the "forecast" package.  
#
#Let's finish this off with a nice way to illustrate the decomposition of a
#time series.
#

x11_dcmp |>
  ggplot(aes(x = index)) +
  geom_line(aes(y = value, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Passenger Miles",
       title = "Total Amtrak Passenger Miles") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )


#
#The zoomed in figure isn't a very pretty figure but it is hard to see any 
#repeating pattern in it that would suggest any residual periodicity. You might
#have to make more than one adjustment to the data.  
#What you are doing is  removing the trend and seasonality to make the data stationary.  

# The textbook
# does a good job of covering what this means but you may have to read and reread
# a couple sections to get it clearly set in your mind.This is an important
# part of time series analysis so you want to make sure you really do 
# understand what it means for your data to be stationary and what you have to
# do to achieve that! 

PassengerMiles.lm <- tslm(PassengerMiles.ts ~ trend + I(trend^2))
summary(PassengerMiles.lm)

plot(PassengerMiles.ts, xlab = "Time", ylab = "Passenger Miles", type = "l")
lines(PassengerMiles.lm$fitted, lwd = 2)

#
#From this plot it doesn't look like a quadratic model fits the data fairly well.

#Remember the last plot from the first laboratory?
#It was pretty obvious that the quadratic was not really working. So, let's add
#a cubic term and see if we get the same result as the last lab.
#

PassengerMiles.lm <- tslm(PassengerMiles.ts ~ trend + I(trend^2) + I(trend^3))

kable(summary(PassengerMiles.lm), digits = 4, caption = "Summary: Cubic fit")

plot(PassengerMiles.ts, xlab = "Time", ylab = "Passenger Miles", type = "l")
lines(PassengerMiles.lm$fitted, lwd = 2)

#
#It doesn't capture everything but it does look better than the quadratic fit.
#
