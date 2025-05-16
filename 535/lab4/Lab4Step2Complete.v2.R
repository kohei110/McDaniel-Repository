
#
#Laboratory 4 ARIMA Models
#
#Written by Marvine Hamner April 2024 updated April/May 2025
#


#
#Load all the necessary libraries
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

#
#Set the working directory and read in the data - if you are not continuing with
#the data already loaded!
#

Amtrak <- read.csv("/Users/nkohei/Workspace/McDaniel-Repository/535/lab4/Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)
str(Amtrak)
Amtrak

#
#Take care of the date format and data type
#

Amtrak$Month <- mdy(Amtrak$Month)
Amtrak

#
#To make things a little easier in the lab let's divide the passenger miles by
#1000000. so that we are working with a number of Passenger Miles in the 1M's
#

Amtrak$PassengerMiles <- Amtrak$PassengerMiles/1000000.
Amtrak

Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

#
#Generate a 3-month centered moving average model for the Amtrak data (all years).
#Do we need to deseasonalize the data?  Consider the ACF
#

Amtrak.tsb.91.16 |>
  ACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles")

#
#But first we'll remove the seasonality in the data.  But we only want to use the
#years 1991 through and including 2016.  
#

Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.comp.91.16 <- decompose(Amtrak.ts.91.16)
autoplot(Amtrak.comp.91.16)
str(Amtrak.comp.91.16)

#
#Now detrend and/or deseason the data as required
#
#Subtract the seasonal component
Amtrak.ts.91.16_desea <- Amtrak.comp.91.16$x - Amtrak.comp.91.16$seasonal

#Subtract the trend component
Amtrak.ts.91.16_detren <- Amtrak.ts.91.16_desea - Amtrak.comp.91.16$trend

#Decompose and plot the remaining data
Amtrak.ts.91.16_de <- decompose(Amtrak.ts.91.16_detren)
autoplot(Amtrak.ts.91.16_de)

#
#Convert to a tsibble and get the ACF and PACF.
#

Amtrak.tsb.91.16_de <- as_tsibble(Amtrak.ts.91.16_detren)

Amtrak.tsb.91.16_de |>
  ACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles")

Amtrak.tsb.91.16_de |>
  PACF() |> autoplot() +
  labs(subtitle = "Amtrak Passenger Miles")

#
#Consider the components of the Amtrak data after we've taken two
#differences to detrend and deseasonalize the data. If you just
#quickly look at the seasonal component it looks like it is all still
#there.  However, the magnitude of the values is 25x less than 
#before.  So, if you look at the ACF you can see a bit of seasonality
#but it quickly becomes insignificant.  The same thing with the
#trend.  Originally it went from 450 to 600 but no only ranges in the
#-5 to 10 magnitude.  From the ACF and PACF plots what do you think would be
#a good model to try to fit to these data now? Or, do you think we
#should still take another difference? 
#

#
#Unfortunately, no, it still looks like there are multiple spikes in the 
#ACF and PACF.  We'll actually take two differences a little further
#along.
#

#
#Let's consider moving averages for a little bit.  We've looked at them
#before.  We know that to use a moving average the data have to be
#stationary.  That is, remember that to use a moving average you can't 
#have any trend or seasonality.  For the moving averages use the 
#deseasonalized data.  But don't forget to go back to the regular 
#data after we've looked at the moving averages!!! 
#
#
#We went from the original data and took a difference to "desea" i.e.
#deseasonalize it.  Then, we went to "detren" to detrend the data.
#In the end we just used ".de" to make a little shorter name for the
#data object converted to a tsibble. So, we'll start from there. 
#

Amtrak_3MA <- Amtrak.tsb.91.16_de

#
#Now work on the moving average smoothing beginning with the centered moving
#average.  For some reason if you run the entire script all at once it 
#doesn't complete a plot for the centered moving average.  But you can
#run the script, highlighting from the beginning until the section for the
#trailing moving average, and the plot of the centered moving average does
#plot correctly.  
#

Amtrak_3MA.19.16 <- Amtrak_3MA |>
  mutate(
    '3-MA' = slider::slide_dbl(value, mean,
                               .before = 1, .after = 1, .complete = TRUE)
  )

Amtrak_3MA.19.16 |>
  autoplot(Amtrak_3MA$value) +
  geom_line(aes(y = `3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles",
       title = "Amtrak Passenger Miles by Month and Centered Moving Average")

#
#Let's zoom into a few years to see exactly how the 3-month moving average
#is handling the fit.  To zoom in we'll need to go back to the time
#series data object, i.e. the ".detren" data object. 
#

str(Amtrak.ts.91.16_detren)

Amtrak_MAZoom_2001_03 <- window(Amtrak.ts.91.16_detren, start = c(2001,1), 
                                end = c(2004,1), frequency = 12)
str(Amtrak_MAZoom_2001_03)
Amtrak.tsb_2001_03 <- as_tsibble(Amtrak_MAZoom_2001_03)

Amtrak_3MA_2001_03 <- Amtrak.tsb_2001_03 |>
  mutate(
    '3-MA' = slider::slide_dbl(value, mean,
                               .before = 1, .after = 1, .complete = TRUE)
  )

Amtrak_3MA_2001_03 |>
  autoplot(Amtrak.tsb_2001_03$value) +
  geom_line(aes(y = Amtrak_3MA_2001_03$`3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles",
       title = "Amtrak Passenger Miles by Month and Centered Moving Average")

#
#Generate a 3-month trailing moving average model for the Amtrak data (all years).
#

Amtrak_3MATR <- Amtrak_3MA_2001_03 |>
  mutate(
    '3-MA' = slider::slide_dbl(value, mean,
                               .before = 2, .after = 0, .complete = TRUE)
  )

Amtrak_3MATR |>
  autoplot(Amtrak.tsb_2001_03$value) +
  geom_line(aes(y = Amtrak_3MATR$`3-MA`), colour = "#D55E00") +
  labs(y = "Passenger Miles",
       title = "Amtrak Passenger Miles by Month and Trailing Moving Average")

#
#Going back to the original data, let's try the Augmented 
#Dickey-Fuller test next.  
#

library(tseries)
Amtrak_adf <- adf.test(Amtrak.ts.91.16)
Amtrak_adf

#
#The ADF test tells us that the data are not stationary. Let's 
#work on this from another perspective, this is an example from 
#Shmueli and Lichtendahl's textbook, Practical Time Series Forecasting
#with R.  This reproduces Figure 5.2 on page 82.  
#

#This example uses "Ridership" rather than "Passenger Miles" so first we'll
#read in the data.


#
#Take care of the date format and data type
#

Amtrak$Month <- mdy(Amtrak$Month)
Amtrak

#
#Now get the same months as we have been using, 1991 - 2016 as a time
#series and a tsibble.  
#

AmtrakRiders.ts.91.16 <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2017, 1), 
                            frequency = 12)
AmtrakRiders.tsb.91.16 <- as_tsibble(AmtrakRiders.ts.91.16)

library(zoo)
ma.trailing <- rollmean(AmtrakRiders.ts.91.16, k=12, align = "right")
ma.centered <- ma(AmtrakRiders.ts.91.16, order = 12)
plot(AmtrakRiders.ts.91.16, ylab = "Ridership", xlab = "Time", bty = "l", xant = "n")
#axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma.centered, col = "steelblue",  lwd = 2)      # centred MA  → blue
lines(ma.trailing, col = "firebrick",  lwd = 2, lty = 2)  # trailing MA → red, dashed

legend("topleft",
       legend = c("Ridership", "Centered MA", "Trailing MA"),
       col    = c("black", "steelblue", "firebrick"),
       lty    = c(1,      1,          2),
       lwd    = c(1,      2,          2),
       bty    = "n")

#
#Now let's zoom into the same years we zoomed into before.
#

AmtrakRiders.ts.01.03 <- ts(Amtrak$Ridership, start = c(2001, 1), end = c(2004, 1), 
                            frequency = 12)

ma.trailing <- rollmean(AmtrakRiders.ts.01.03, k=12, align = "right")
ma.centered <- ma(AmtrakRiders.ts.01.03, order = 12)
plot(AmtrakRiders.ts.01.03, ylab = "Ridership", xlab = "Time", bty = "l", xant = "n")
#axis(1, at = seq(2001.5, 2004.25, 1), labels = format(seq(2001.5, 2004.25, 1)))
lines(ma.centered, col = "steelblue",  lwd = 2)      # centred MA  → blue
lines(ma.trailing, col = "firebrick",  lwd = 2, lty = 2)  # trailing MA → red, dashed

legend("topleft",
       legend = c("Ridership", "Centered MA", "Trailing MA"),
       col    = c("black", "steelblue", "firebrick"),
       lty    = c(1,      1,          2),
       lwd    = c(1,      2,          2),
       bty    = "n")


ma.trailing <- rollmean(AmtrakRiders.ts.01.03, k=3, align = "right")
ma.centered <- ma(AmtrakRiders.ts.01.03, order = 3)
plot(AmtrakRiders.ts.01.03, ylab = "Ridership", xlab = "Time", bty = "l", xant = "n")
#axis(1, at = seq(2001.5, 2004.25, 1), labels = format(seq(2001.5, 2004.25, 1)))
lines(ma.centered, col = "steelblue",  lwd = 2)      # centred MA  → blue
lines(ma.trailing, col = "firebrick",  lwd = 2, lty = 2)  # trailing MA → red, dashed

legend("topleft",
       legend = c("Ridership", "Centered MA", "Trailing MA"),
       col    = c("black", "steelblue", "firebrick"),
       lty    = c(1,      1,          2),
       lwd    = c(1,      2,          2),
       bty    = "n")


#
#We'll go on from moving averages now to AR(p), MA(q), and ARIMA models.
#If you are like me you have a whole bunch of Amtrak data subsets.  Let's clean 
#up things a little so we do not make any mistakes by using the wrong data. 
#First we'll remove the Amtrak data and then read it back in again.
#

# rm(list = ls(pattern = "Amtrak*"))
# 
# setwd("I:/My Passport Documents/McDaniel/DataAnalytics/ANA535/NewANA535/Laboratories")

Amtrak <- read.csv("/Users/nkohei/Workspace/McDaniel-Repository/535/lab4/Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)
str(Amtrak)
#Amtrak

#
#Take care of the date format and data type
#

Amtrak$Month <- mdy(Amtrak$Month)
#Amtrak

#
#To make things a little easier in the lab let's divide the passenger miles by
#1000000. so that we are working with a number of Passenger Miles in the 1M's
#

Amtrak$PassengerMiles <- Amtrak$PassengerMiles/1000000.
#Amtrak

Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), 
                      frequency = 12)
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

#
#Now we have our Amtrak data read in, our subset for 1991-2016 setup and 
#a corresponding tsibble generated.  
#

#
#We will develop models using a couple different packages in R. One from
#the textbook and another from other online sources and Schmueli's book.
#We didn't load the package tseries at the beginning so we need to load that
#now.  If you haven't installed the urca package in R you will need to
#do that too.  We'll also load that with the library() command now.
#

library(tseries)
library(urca)

Amtrak_kpss <- kpss.test(Amtrak.ts.91.16, null = "Trend")
Amtrak_kpss

Amtrak_adf <- adf.test(Amtrak.ts.91.16)
Amtrak_adf

#
#This is an interesting result because it gives us conflicting results.  This
#is actually not that uncommon. Now let's consider the unit root test to see 
#what it tells us about the number of differences we should take.  
#

Amtrak_unitroot <- unitroot_ndiffs(Amtrak.ts.91.16)
Amtrak_unitroot

Amtrak_seasdiff <- Amtrak.tsb.91.16 |>
  mutate(Amtrak.ts.91.16 = difference(Amtrak.ts.91.16), 12) |>
  features(Amtrak.tsb.91.16, unitroot_ndiffs)
Amtrak_seasdiff

#
#Since we have monthly data we are going to use lag = 12 to get a seasonal
#difference.
#

Amtrak.comp.91.16b <- decompose(Amtrak.ts.91.16)
autoplot(Amtrak.comp.91.16b)

Amtrak_seasdiff_1 <- diff(Amtrak.ts.91.16, lag = 12)
Amtrak.comp.seasdiff.1 <- decompose(Amtrak_seasdiff_1)
autoplot(Amtrak.comp.seasdiff.1)

Amtrak_seasdiff_ndiffs_1 <- unitroot_ndiffs(Amtrak_seasdiff_1)
Amtrak_seasdiff_ndiffs_1 

#
#We've taken one difference as the unitroot_ndiffs() command
#tells us.  But are the data stationary now?  Let's run the
#unit root tests again and see.
#

Amtrak_kpss <- kpss.test(Amtrak_seasdiff_1, null = "Trend")
Amtrak_kpss

Amtrak_adf <- adf.test(Amtrak_seasdiff_1)
Amtrak_adf

#
#We've taken 1 difference to remove seasonality.  Out of curiousity
#let's take a 2nd difference to remove seasonality to see what 
#difference it makes in the number of differences it tells us to
#take.
#

Amtrak_seasdiff_2 <- diff(Amtrak_seasdiff_1, lag = 1)
Amtrak.comp.seasdiff.2 <- decompose(Amtrak_seasdiff_2)
autoplot(Amtrak.comp.seasdiff.2)

Amtrak_seasdiff_ndiffs_2 <- unitroot_ndiffs(Amtrak_seasdiff_2)
Amtrak_seasdiff_ndiffs_2

Amtrak_kpss <- kpss.test(Amtrak_seasdiff_2, null = "Trend")
Amtrak_kpss

Amtrak_adf <- adf.test(Amtrak_seasdiff_2)
Amtrak_adf



#
#So, it did continue to reduce the magnitude of the values somewhat 
#and now it tells us we don't need to take any more differences.
#And, the unit root tests tell us the data are now stationary. 
#

#
#The last step of all this is to get the ARIMA model for the Amtrak
#data.  We can do this without differencing but, again for curiosity,
#let's do it for both the twice-differenced data and for the original
#data and see what we get.
#

Amtrak_2.ts <- ts(Amtrak_seasdiff_2)
Amtrak_2.tsb <- as_tsibble(Amtrak_seasdiff_2)

Amtrak_2.tsb |> 
  autoplot() +
  labs(title = "Amtrak Data", y = "")

#
#First let's look at a time plot of the Amtrak twice-differenced data.
#That looks interesting.  There is something there I didn't see in the
#data around the end of 2014/beginning of 2015 that is causing some
#wild swings in the data.  I'm not sure what that is but it looks like
#we've got stationary data (no discernable trend or periodicity) now.
#

#
#There are lots of ways to get an ARIMA model.  I'll use 2 of them here.
#One is from the forecast package.  The other is from the fable package.
#I ran the forecast package first and got an answer.  Then, I used that
#answer in the fable package.  It produced some errors and wouldn't
#work.  The answers the fable package did come up with are slightly
#more accurate than the one from the forecast package.  Here are the
#commands.
#

library(forecast)
model1 <- auto.arima(Amtrak_2.ts)
summary(model1)

Amtrak_2.tsb |>
  gg_tsdisplay(difference(value), plot_type = 'partial')

#
#I'll use the model built by the auto.arima() command, a ARIMA(5,0,4)
#model as one of the models tested by the fable package. I'll also add
#another commone model, the ARIMA(1,0,1) model to see what that does.
#And, I'll let fable automatically build 2 models with different 
#parameters set.  

Amtrak_2_fit <- Amtrak_2.tsb |>
  model(arima504 = ARIMA(value ~ pdq(5,0,4)),
        arima101 = ARIMA(value ~pdq(1,0,1)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise = FALSE, approximation = FALSE))

report(Amtrak_2_fit)

#
#You can see the summary for the models built using this command.
#It looks like the code won't run the ARIMA(5,0,4) model. So we cannot
#directly compare the results for that model as built by 2 different
#packages/codes.  But, the value of AICc for all the models built
#by both packages is pretty close. From the forecast package AICc is
#2880 and from the fable package the AICc's are in the 2760-2770 range.
#

#
#Now let's produce a forecast using these results.
#
f
#
#First the forecast results. I'll just do a forecast for 5 years. 
#

f <- forecast(model1, level = c(95), h=5*12)
plot(f)

#
#This actually looks pretty good compared with the time plot of the 
#time plot of the data.  But, can you tell what's wrong with
#it?  Remember that I said whatever we did during processing the data
#and analysis we would have to undo to get real-world values???  This
#is where that shows up.
#

#
#Let's go back to the original time series from 1991 to 2016 and do all 
#this again using the forecast package again. Remember that with an 
#ARIMA model we do not have to independently take differences, i.e. 
#the data do not need to be stationary for this code to work.  
#

Amtrak$PassengerMiles <- Amtrak$PassengerMiles/1000000.
#Amtrak

Amtrak.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
AmtrakR.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2017, 1), frequency = 12)


model2 <- auto.arima(Amtrak.ts)
summary(model2)

model3 <- auto.arima(AmtrakR.ts)
summary(model3)

#
#Here I've built a model for Ridership as well as for Passenger Miles.
#This actually results in a simpler model but is much less accurate.
#The ARIMA model built using the original Amtrak data is 
#ARIMA(0,1,3)(2,1,1)[12].  Let's look at the 5-year forecast now.
#

f2 <- forecast(model2, level = c(95), h=5*12)
plot(f2)

#
#That looks like a forecast.  It is pretty flat (because the prior few
#years are pretty flat). It probably isn't that different than a naive
#forecast.  Let's see what we get from the fable and feasts packages.
#

#
#Just to make sure I've got the right data objects I'll go back to the
#tsibble of the original data.  
#

Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

Amtrak.tsb.91.16 |>
  gg_tsdisplay(difference(value), plot_type = 'partial')

Amtrak_fit <- Amtrak.tsb.91.16 |>
  model(arima013 = ARIMA(value ~ pdq(0,1,3)),
        arima101 = ARIMA(value ~pdq(1,0,1)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise = FALSE, approximation = FALSE))

report(Amtrak_fit)

forecast(Amtrak_fit, h=60) |>
  filter(.model == 'arima013') |>
  autoplot(Amtrak.tsb.91.16) + 
  labs(title = "Amtrak Data 1991-2016 plus 5-year Forecast")

#
#Another interesting point is that the ARIMA(0,1,3) model is actually
#slightly more accurate using the AICc than the automatically found
#ARIMA model from the fable package.  The plots from the forecast 
#package and the feasts package appear different but the scales are the
#same.  

#
#Another point I need to make is that the further out in the future you
#go the larger the potential error gets. This is straightforward but
#might not be something you always think about but should. You can see
#the effect of this in the plots of forecasts. 
#

#
#One of the things I like to do to check my work is to use 2 different
#programs that do the same thing. If you use both with the same data
#and get the same answer I think that's a pretty good conclusion!




#

###***********************************************************###
###***********************************************************###

#
#The last part of Laboratory #4 are skills-building exercises again.
#These are intended to help you get more comfortable with the coding
#that the textbook authors use.  It will help you with the final
#exam.
#

#
#Exercise 3.  Part A: Turkey GDP
#

turkey <- global_economy |> filter(Country == "Turkey")
turkey |> autoplot(GDP)
turkey |> autoplot(log(GDP))

#
#No noticeable seasonal anything.
#

turkey |> autoplot(log(GDP) |> difference())
turkey |> features(GDP, guerrero)

turkey_gdp <- global_economy %>%
  filter(Country == "Turkey")
gg_tsdisplay(turkey_gdp, GDP)

#strip df down
turkey_gdp_prepped <- dplyr::select(turkey_gdp, GDP)

#determine lambda
BoxCox.lambda(turkey_gdp_prepped$GDP)


#
#Exercise 3. Part b: Accomodation takings in Tasmania
#

# create data object
tasmania_takings <- aus_accommodation %>%
  filter(State == 'Tasmania')

#display data
gg_tsdisplay(tasmania_takings, Takings)

#strip data object down
tasmania_takings_prepped <- dplyr::select(tasmania_takings, Takings)

#determine lambda
BoxCox.lambda(tasmania_takings_prepped$Takings)

#perform boxcox and apply to df
tasmania_takings_prepped$Takings <- box_cox(tasmania_takings_prepped$Takings, BoxCox.lambda(tasmania_takings_prepped$Takings))

#plot transformed data
tasmania_takings_prepped %>% gg_tsdisplay(Takings, plot_type = 'partial')

ndiffs(tasmania_takings_prepped$Takings)

ggtsdisplay(diff(tasmania_takings_prepped$Takings))

#
#Here is a second way to do all this
#

tas <- aus_accommodation |> filter(State == "Tasmania")
tas |> 
  autoplot(Takings, ) +
  geom_point()

tas |> autoplot(log(Takings))
tas |> autoplot(log(Takings) |> difference(lag = 4))
tas |> autoplot(log(Takings) |> difference(lag = 4) |> difference())
tas |> features(Takings, guerrero)


#
#Exercise 3. Part c: Souvenir sales
#

#display data
gg_tsdisplay(souvenirs, Sales)

souvenirs_prepped <- souvenirs

#determine lambda
BoxCox.lambda(souvenirs_prepped$Sales)
#perform boxcox and apply to df
souvenirs_prepped$Sales <- box_cox(souvenirs_prepped$Sales, BoxCox.lambda(souvenirs_prepped$Sales))
#plot transformed data
souvenirs_prepped %>% gg_tsdisplay(Sales, plot_type = 'partial')

ndiffs(souvenirs_prepped$Sales)
ggtsdisplay(diff(souvenirs_prepped$Sales))


#
#Here is the second way to do all this.
#

souvenirs |> autoplot(Sales)
souvenirs |> autoplot(log(Sales))
souvenirs |> autoplot(log(Sales) |> difference(lag=12))
souvenirs |> autoplot(log(Sales) |> difference(lag=12) |> difference())
souvenirs |> features(Sales, guerrero)




###***********************************************************###
###***********************************************************###

#
#Exercise 8.  Part a: United States GDP
#

usa_gdp <- global_economy %>%
  filter(Country == "United States") %>%
  dplyr::select(GDP)

usa_gdp %>%
  autoplot(GDP)

lambda <- usa_gdp |>
  features(GDP, features = guerrero) |>
  pull(lambda_guerrero)
lambda

#perform boxcox and apply to data object
usa_gdp$GDP <- box_cox(usa_gdp$GDP, BoxCox.lambda(usa_gdp$GDP))

#plot transformed data
usa_gdp %>% 
  gg_tsdisplay(GDP, plot_type = 'partial')

#
#Part b
#

fit <- usa_gdp |>
  model(ARIMA(GDP))
report(fit)

fit <- usa_gdp |>
  model(
    arima010 = ARIMA((GDP) ~ 1 + pdq(0, 1, 0)),
    arima011 = ARIMA((GDP) ~ 1 + pdq(0, 1, 1)),
    arima012 = ARIMA((GDP) ~ 1 + pdq(0, 1, 2)),
    arima013 = ARIMA((GDP) ~ 1 + pdq(0, 1, 3)),
    arima110 = ARIMA((GDP) ~ 1 + pdq(1, 1, 0)),
    arima111 = ARIMA((GDP) ~ 1 + pdq(1, 1, 1)),
    arima112 = ARIMA((GDP) ~ 1 + pdq(1, 1, 2)),
    arima113 = ARIMA((GDP) ~ 1 + pdq(1, 1, 3)),
    arima210 = ARIMA((GDP) ~ 1 + pdq(2, 1, 0)),
    arima211 = ARIMA((GDP) ~ 1 + pdq(2, 1, 1)),
    arima212 = ARIMA((GDP) ~ 1 + pdq(2, 1, 2)),
    arima213 = ARIMA((GDP) ~ 1 + pdq(2, 1, 3)),
    arima310 = ARIMA((GDP) ~ 1 + pdq(3, 1, 0)),
    arima311 = ARIMA((GDP) ~ 1 + pdq(3, 1, 1)),
    arima312 = ARIMA((GDP) ~ 1 + pdq(3, 1, 2)),
    arima313 = ARIMA((GDP) ~ 1 + pdq(3, 1, 3))
  )

fit |>
  glance() |>
  arrange(AICc) |>
  select(.model, AICc)

#
#Part d
#

best_fit <- usa_gdp |>
  model(ARIMA(box_cox(GDP, lambda) ~ 1 + pdq(1, 1, 0)))
best_fit |> report()

best_fit |> gg_tsresiduals()

#
#Part e
#

fit %>%
  forecast(h = 10) %>%
  autoplot(usa_gdp)

best_fit %>%
  forecast(h = 10) %>%
  autoplot(usa_gdp)

#
#Part f
#

ets_check <- usa_gdp$GDP %>%
  ets()

ets_check

usa_gdp |>
  model(ETS(GDP)) |>
  forecast(h = 10) |>
  autoplot(usa_gdp)



###***********************************************************###
###***********************************************************###

#
#Additional Information for ARIMA models and the Amtrak data
#

library(forecast)

Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)

Amtrak.ts.91.16 %>% 
  ggtsdisplay(Amtrak.ts.91.16, plot.type="histogram", points=FALSE,
              smooth = FALSE)

fit_Amt <- auto.arima(Amtrak.ts.91.16)
summary(fit_Amt)

fit <- Amtrak.tsb.91.16 |>
  model(arima = ARIMA())
report(fit)

fit |> gg_tsresiduals()


#
#Plot the Amtrak data again
#

Amtrak.tsb.91.16 %>% 
  gg_tsdisplay(value, plot_type = 'partial')

AmtrakARIMA <- Amtrak.tsb.91.16 |> 
  gg_tsdisplay(difference(difference(value, 12)),
               plot_type = 'partial', lag = 12) +
  labs(title = "Seasonally differenced", y="")

AmtrakARIMA

fit <- Amtrak.tsb.91.16 |>
  model(
    AmtrakAMRIMA = ARIMA(value ~ pdq(0,1,0) + PDQ(1,0,0)),
    Auto = ARIMA()
  )
report(fit)

glance(fit)

fit |> select(Auto) |> gg_tsresiduals(lag = 12)
fit |> select(AmtrakAMRIMA) |> gg_tsresiduals(lag = 12)


#
#Step 6: Cross-validation
#

#
#First let's setup a training and test set and look at the accuracy
#we get using those.
#

AmtrakTraining <- Amtrak.tsb.91.16 |>
  filter(year(index) < 2009)
autoplot(AmtrakTraining)

autoplot(Amtrak.tsb.91.16, value) +
  autolayer(AmtrakTraining, value, colour = "red")

fit <- AmtrakTraining |>
  model(SNAIVE(value))

fit |> gg_tsresiduals()

fcast <- fit |>
  forecast(new_data = anti_join(Amtrak.tsb.91.16, AmtrakTraining))

fcast |> autoplot(Amtrak.tsb.91.16)

bind_rows(
  accuracy(fit),
  accuracy(fcast, Amtrak.tsb.91.16)
) |>
  select(-.model)



#
#Here is cross-validation following https://robjhyndman.com/hyndsight/tscvexample/
#

plot(Amtrak.ts.91.16, ylab = "Passenger Miles", xlab = "Time")
k <- 60 #minimum data length for fitting a model
n <- length(Amtrak.ts.91.16)
mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12)
st <- tsp(Amtrak.ts.91.16)[1]+(k-2)/12

for (i in 1:(n-k)) 
  {
  xshort <- window(Amtrak.ts.91.16, end=st + i/12)
  xnext <- window(Amtrak.ts.91.16, start = st + (i+1)/12, end = st + (i+12)/12)
  fit1 <- tslm(xshort ~ trend + season, lambda = 0)
  fcast1 <- forecast(fit1, h=12)
  fit2 <- Arima(xshort, order = c(3, 0, 1), seasonal = list(order=c(0,1,1), period=12), 
                include.drift = TRUE, lambda = 0, method = "ML")
  fcast2 <- forecast(fit2, h=12)
  fit3 <- ets(xshort, model = "MMM", damped = TRUE)
  fcast3 <- forecast(fit3, h=12)
  mae1[i, 1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
  mae2[i, 1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i, 1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}

mae1Cols <- mae1 %>%
  colMeans(na.rm = TRUE)
mae1Cols
mae2Cols <- mae2 %>%
  colMeans(na.rm = TRUE)
mae2Cols
mae3Cols <- mae3 %>%
  colMeans(na.rm = TRUE)
mae3Cols

mae1Cols <- as.data.frame(mae1Cols)
mae1Cols
mae2Cols <- as.data.frame(mae2Cols)
mae3Cols <- as.data.frame(mae3Cols)
mae3Cols
idn <- seq(1,12,by=1)
maes <- data.frame(idn, mae1Cols, mae2Cols, mae3Cols)
maes

maes_reshaped <- data.frame(x = idn, y = c(maes$mae1Cols, maes$mae2Cols, maes$mae3Cols),
                          group = c(rep("mae1", nrow(maes)),
                                    rep("mae2", nrow(maes)),
                                    rep("mae3", nrow(maes))))

library(data.table)
maes_dt <- as.data.table(maes)
data_melted <- melt(maes_dt, id.vars="idn")

ggplot(data_melted, aes(x = idn, y = value, fill = variable)) +
  geom_line(aes(group = variable, colour = variable)) +
  xlab("Months 1-12") +
  ylab("MAE") +
  scale_color_manual(labels = c("LM", "ARIMA", "ETS"),
                     values = c("red", "blue", "green"))
