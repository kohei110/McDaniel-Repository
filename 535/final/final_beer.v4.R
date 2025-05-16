#
#ANA 535 Final Examination
#
#Written by Marvine Hamner May 2024, updated May 2025
#

library(forecast)
library(zoo)
library(tseries)
library(pastecs)
library(lubridate)
library(ggplot2)
library(ggplottimeseries)
library(reshape2)
library(fable)
library(tsibble)
library(tsibbledata)
library(xts)
library(dplyr)
library(TSstudio)
library(tidyr)
library(tidyverse)
library(astsa)
library(fpp)
library(fpp2)
library(fpp3)
library(xlsx)


#str(aus_production)
#Write out the data.  To use the xlsx package the data must be a dataframe.
#
#df <- as.data.frame(aus_production)
#str(beerProd)
#write.xlsx(df, file = "beerProduction.xlsx")

setwd("~/Workspace/McDaniel-Repository/535/final")
beerProduction <- read.csv("beerProd.csv")
#I've had a lot of trouble with this. You may have a better way.  Note that some
#of the commands in the EDA may be a bit out of order with the online questions.

#Check for any missing values

#attach(beer)
sum(is.na(beer))

#There are no missing value so plot the data.  

beer <- beerProduction
colnames(beer) <- c("Date", "beer")
View(beer)
str(beer)

beer.ts <- ts(beer$beer, start = c(1956, 1), frequency = 4)
plot(beer.ts, xlab = "Time", ylab = "Beer Production", type = "l")

beer.tsb <- as_tsibble(beer.ts)
beer.tsb

beer.tsb |> 
  autoplot() +
  labs(title = "Australian Beer Production", y = "")

zoomBeer.tsb <- beer.tsb %>%
  filter_index("1981 Q1" ~ "1985 Q4")
zoomBeer.tsb
autoplot(zoomBeer.tsb)

#Obviously beer production has leveled off since about 1974 or so
#
#What is the current (since 2000) mean production of Austrailian 
#beer in liters?  ****Use this to answer Q! on the exam***** The beer production
#prior to 1974 was increasing every year so that a "mean" isn't really very
#meaningful.  
#

beerShort <- window(beer.ts, start = c(2000, 1))
str(beerShort)
mean(beerShort)


#There appear to be two parts to beer production (and consumption)
#in Australia.  The first part is before about 1974.  The second
#part is after 1974.  Let's plot that and see if it looks correct.

firstBeer.tsb <- beer.tsb |>
  filter(year(index) <= 1974)
firstBeer.tsb
autoplot(firstBeer.tsb)



autoplot(beer.tsb, value) +
  autolayer(firstBeer.tsb, value, colour = "red")

#That's looks as good as we're going to get it without doing a real
#numerical analysis on the data.  We don't necessarily need that
#for a simple EDA. Now we can get summary information for each 
#part of the beer production data.

secondBeer.tsb <- beer.tsb |>
  filter(year(index) > 1974)
secondBeer.tsb

autoplot(firstBeer.tsb, value, colour = "blue") +
  autolayer(secondBeer.tsb, value, colour = "red")

#Now we've got the data split into two pieces that we can analyze.
#This is not quite as bad as the Amtrak data across the Covid 
#years but it is bad enough to warrant some special attention.

hist(beer$beer, breaks = 20, xlab = "Beer Production",
     main = "Histogram of Beer Production")

#The histogram doesn't appear to be too skewed for real world data.

boxplot(beer$beer)

#Even with the simplest boxplot it is obvious that many data points
#in the beer data appear to be outliers. But are they? But more than 
#boxplots we are interested in lag plots in time series data.  Let's 
#look at the data in roughly 10 year increments.  

recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "Lag(Beer, k)")

recent_production <- aus_production |>
  filter(year(Quarter) >= 1956)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "Lag(Beer, k)")


recent_production <- aus_production |>
  filter_index("2000 Q1" ~ "2010 Q4")
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "Lag(Beer, k)")




#This is interesting.  The textbook authors want us to see that there there is a
#strong, positive relationship reflecting a strong seasonality in the data.
#And we do see that at lags 4 and 8. For quarterly data that corresponds to the 
#last quarter of a year, i.e. the 4th qtr or last qtr of the first year and the
#8th qtr or last qtr of the second year. Lags 2 and 6 are opposite that in terms 
#of time. It is difficult to see what's going on there but the "spread" or variance
#is greatest for those lags... In an annual periodic pattern you'll have peaks 
#(lags strongly positive) but you'll also have troughs.  Can't have one without 
#the other! 

##### WIP

#Let's go ahead and look at the ACF and PACF. Let's go ahead and add a few more
#lags to these plots, i.e. k=24 by default.  

beerAcf <- ggAcf(beer.ts) +
  ggtitle("Sample ACF for Beer Production Data")
plot(beerAcf)

beerPacf <- ggPacf(beer.ts) +
  ggtitle("Sample PACF for Beer Production Data")
plot(beerPacf)

#Yes, we do see the strong seasonality in the data but not just at lags 4 and
#8. On the ACF every quarter reflects a different level of production and all qtrs
#are strongly correlated with the prior season's corresponding quarter.  
#Let's check on the unit root tests.

beer.ts.adf <- adf.test(beer.ts)
beer.ts.adf

beer.ts.kpss <- kpss.test(beer.ts)
beer.ts.kpss

#Interesting!  Both the ADF and KPSS tests show that the data are non-stationary
#and consistent between the tests.  It is also consistent with the high degree 
#of periodicity in the data.

#Should we take the log of the data? Let's see what we have.


### Q24
par(mfrow=c(1,2))

#Check the logs
lnbeer <- log(beer$beer)
lnbeer.ts <- ts(lnbeer, start = c(1956, 1), frequency = 4)
plot(lnbeer.ts, type = "l", col = "blue", ylab = "Log Beer Production")

beer.ts <- ts(beer$beer, start = c(1956, 1), frequency = 4)
plot(beer.ts, xlab = "Time", ylab = "Beer Production", type = "l")


#
#Because I think about this more from the perspective of an engineer than
#a business person or economist, I do not think it is worthwhile to take 
#the log of beer.  What I mean is that I do not see a lot of difference
#in global or local patterns (e.g. overall trend) by taking the log of the
#data. It isn't like there is an curve that we have linearized by taking
#the log of the data.  

#
#You will need to decide for yourself whether or not 
#you want to use logs more when you are doing your own analysis.  For this
#script I will not use logs going forward.
#

#Now let's look at the decomposed Beer data to see if there is a 
#trend and/or seasonality and of course there will be. 

# Q25
beer.comp <- decompose(beer.ts)
par(mfrow = c(4, 1))
plot(beer.comp)

beer.tsb <- as_tsibble(beer.ts)
str(beer.tsb)

par(mfrow = c(1,1))
beer.tsb |>
  ACF() |> autoplot() +
  labs(subtitle = "Quarterly Beer Production")

#
#What about the ACF associated with a linear model?  Well, first
#we have to construct a linear model of the time series data, i.e.
#we'll use the tslm() command to do that.  
#

library(forecast)
beer.tsb <- as_tsibble(beer.ts)
str(beer.tsb)

fit.beer.tslm <- beer.tsb |>
  model(fit_tslm2 = TSLM(value ~ trend() + I(trend()^2) + season()))
report(fit.beer.tslm)

fit.beer.tslm |> gg_tsresiduals() +
  labs(title = "Australian Beer Production")

fit.beer.tslm %>%
  accuracy() %>%
  arrange(MAPE)

#Well... that's interesting!  The plot of residuals does not have the residuals
#as "iid," that is independent and identically distributed about a mean of zero.
#The histogram of residuals doesn't look quite normal. It seems a bit right
#skewed.  There is also significant periodicity still showing in the ACF. 

#Let's start looking at this using the two time ranges.  First the initial
#period from inception to about 1974.  Second the time range from 1974 to
#2010.

fit.firstBeer.tslm <- firstBeer.tsb |>
  model(fit_tslm2 = TSLM(value ~ trend() + I(trend()^2) + season()))
report(fit.firstBeer.tslm)

fit.firstBeer.tslm |> gg_tsresiduals() +
  labs(title = "First Time Range")

fit.firstBeer.tslm %>%
  accuracy() %>%
  arrange(MAPE)

fit.secondBeer.tslm <- secondBeer.tsb |>
  model(fit_tslm2 = TSLM(value ~ trend() + I(trend()^2) + season()))
report(fit.secondBeer.tslm)

fit.secondBeer.tslm |> gg_tsresiduals() +
  labs(title = "Second Time Range")

fit.secondBeer.tslm %>%
  accuracy() %>%
  arrange(MAPE)

#Everything does look a bit better when we split up the data into two time
#ranges. For the first time range, inception to about 1974, the residuals
#appear to be iid, the histogram looks much more normal.  And, there are no
#significant spikes in the ACF.  For the second time range, 1974 to 2010, 
#there still is a bit of up and down in the plot of residuals.  But, the 
#histogram looks much more normal.  There is the significant spike at lag = 4
#which is consistent with a pattern in the plot of residuals.  Therefore,
#we may need to take a difference in the second time range to eliminate
#the periodicity.  

#
#We'll continue looking at the beer data as a whole and in the two different time
#ranges for now.  
#
#Considering all the information we've plotted it looks like we 
#certainly have seasonality but the scale is very small.  Although there doesn't 
#appear to be as much trend there has been some in the past and maybe a slightly
#decreasing trend now.  So let's adjust the data for seasonality and
#see if that takes care of the trend. 
#

#Because we decomposed the beer data earlier we can go ahead and use that now.

beer.adj <- beer.ts - beer.comp$trend - beer.comp$seasonal
beer.final <- decompose(beer.adj)
autoplot(beer.final)

firstBeer.comp <- decompose(firstBeer.ts)
firstBeer.ts <- as.ts(firstBeer.tsb)
firstBeer.adj <- firstBeer.ts - firstBeer.comp$trend - firstBeer.comp$seasonal
firstBeer.final <- decompose(firstBeer.adj)
autoplot(firstBeer.final, ylab = "firstBeer")

secondBeer.comp <- decompose(secondBeer.ts)
secondBeer.ts <- as.ts(secondBeer.tsb)
secondBeer.adj <- secondBeer.ts - secondBeer.comp$trend - secondBeer.comp$seasonal
secondBeer.final <- decompose(secondBeer.adj)
autoplot(secondBeer.final, ylab = "secondBeer")

#
#Plotting the decomposition of seasonally and trend adjusted data for each time
#range it appears that we've removed any trend but what about seasonality?
#We can use the gg_tsresiduals() function to look at the ACF for that
#

str(beer.adj)
beer.adj.tsb <- as_tsibble(beer.adj)

fit.beer.adj.tslm <- beer.adj.tsb |>
  model(fit.beer.adj.tslm = TSLM(value ~ trend() + season()))
report(fit.beer.adj.tslm)

fit.beer.adj.tslm |> gg_tsresiduals()

fit.beer.adj.tslm %>%
  accuracy() %>%
  arrange(MAPE)


str(firstBeer.adj)
firstBeer.adj.tsb <- as_tsibble(firstBeer.adj)

fit.firstBeer.adj.tslm <- firstBeer.adj.tsb |>
  model(fit.firstBeer.adj.tslm = TSLM(value ~ trend() + season()))
report(fit.firstBeer.adj.tslm)

fit.firstBeer.adj.tslm |> gg_tsresiduals()

fit.firstBeer.adj.tslm %>%
  accuracy() %>%
  arrange(MAPE)


str(secondBeer.adj)
secondBeer.adj.tsb <- as_tsibble(secondBeer.adj)

fit.secondBeer.adj.tslm <- secondBeer.adj.tsb |>
  model(fit.secondBeer.tslm = TSLM(value ~ trend() + season()))
report(fit.secondBeer.adj.tslm)

fit.secondBeer.adj.tslm |> gg_tsresiduals()

fit.secondBeer.adj.tslm %>%
  accuracy() %>%
  arrange(MAPE)

#
#Well it "looks" like there is still alot of seasonality left in the second time
#range.  But, look at the scale of the ACF. It is very small.  Seasonal variation 
#from the entire dataset to the first time range and the second time range is.  
#striking. In the individual time ranges it is really very small. Also, from 
#gg_tsresiduals() we can see that the residuals look much better and the ACF 
#spikes are really about 0.3 to 0.45 or less for the first spike.  Again, not 
#much left there.  

#The big question is are we doing any good by making things more complex by
#splitting the dataset into two time ranges.  From the evidence so far;
#residuals, accuracy and so on, I would say a qualified no.  Once we have adjusted
#for seasonality, i.e. taken one seasonal difference, things look pretty good. 
#So, let's just go back to treating the dataset as one whole data object for the
#rest of this exam.  

#
#Earlier we decomposed the beer data then took a seasonal difference to
#generate an adjusted beer data object, beer.adj. Now, we'll 
#replace NAs in data where the adjustment for seasonality or trend
#resulted in a zero value that is shown now as NA.  Just replace
#with zeros. Then, check to make sure all NA's have been replaced.
#

beer.adj <- replace(beer.adj, is.na(beer.adj), 0)

missing <- sum(which(is.na(beer.adj)))
missing
str(beer.adj)

#Set up the training and validation periods
nValid <- 12
nTrain <- length(beer.adj) - nValid
train.ts <- window(beer.adj, start = c(1956, 1), end = c(1956, nTrain), frequency = 4)
valid.ts <- window(beer.adj, start = c(1956, nTrain + 1), end = c(1956, nTrain + nValid), freqency = 4)

train.comp <- decompose(train.ts)
train.adj <- train.ts - train.comp$trend - train.comp$seasonal
plot(beer.ts, xlab = "Time", ylab = "Beer Production", type = "l")
plot(beer.adj, xlab = "Time", ylab = "Adjusted Beer Production", type = "l")

#Build a linear model with trend and season
train.lm.trend.season <- tslm(train.ts ~ trend +I(trend^2) +I(trend^3) +I(trend^4) + season)
summary(train.lm.trend.season)

#Note that I've added a couple more higher order terms to this model.  Play around
#with this and see if you agree that this adds a bit more accuracy to the model.

#Look at the residuals and ACF
par(mfrow = c(1,2))
plot(train.lm.trend.season$residuals, ylab = "Residuals", xlab = "Time", type = "l",
     xaxt = "n")
Acf(train.lm.trend.season$residuals)

#The residuals look ok, but it's difficult to tell about the magnitude on this plot.
#You might want to re-plot this with only 1 plot per figure.  And, there is still 
#some residual (no pun intended) problems with the trend and/or seasonal variation.  
#But, let's move forward and look at another way we generated a forecast in Lab4. 
#Create the forecast for the validation period, i.e. the portion of the dataset
#that we set aside to evaluate the forecast accuracy.  

train.lm.pred <- forecast(train.lm.trend.season, h=nValid, level = 0)
summary(train.lm.pred)

par(mfrow = c(1,1))
plot(train.lm.pred, xlab = "Time", ylab = "Beer Production", type = "l")

#Add a line for the actual values in the forecast period
#
lines(valid.ts)
axis(1, at = seq(1956, 2016, 10), labels = format(seq(1956, 2016, 10)))
lines(c(2007.75, 2007.75), c(-500, 3500), lwd = 2, col = "red")
lines(c(2010.25, 2010.25), c(-500, 3500), lwd = 2, col = "red")
text(1980.0, 32.5, "Training")
text(2008.5, 32.5, "Validation")
arrows(2008, 425, 1956.25, 425, code = 3, length = 0.1, angle = 30, 
       col = "red", lwd = 2)
arrows(2011, 425, 2008, 425, code = 3, length = 0.1, angle = 30, 
       col = "red", lwd = 2)

#
#Let's just look at the validation period
#

plot(train.lm.pred,xlim = c(2007.5, 2010.5), xlab = "Time", ylab = "Beer Production", type = "l")

#Add a line for the actual values in the forecast period
#
lines(valid.ts, xlim = c(2007.5, 2010.5))
axis(1, at = seq(1956, 2016, 10), labels = format(seq(1956, 2016, 10)))
lines(c(2007.5, 2007.5), c(-500, 3500), lwd = 2, col = "red")
lines(c(2010.25, 2010.25), c(-500, 3500), lwd = 2, col = "red")
text(1980.0, 32.5, "Training")
text(2008.5, 32.5, "Validation")
arrows(2008, 425, 1956.25, 425, code = 3, length = 0.1, angle = 30, 
       col = "red", lwd = 2)
arrows(2011, 425, 2008, 425, code = 3, length = 0.1, angle = 30, 
       col = "red", lwd = 2)

#From the zoomed in plot it is pretty obvious that we have smoothed out the yearly
#fluctuations in the data. That is something we did in differencing so that we
#could build a linear model.  It seems that it did work as we wanted.  So, let's
#look at the residuals.  

fit.beer.adj.tslm |> gg_tsresiduals()

#This gives us some conflicting information.  Let's look further.  Let's use the
#ADF test and the unitroot_ndiffs() command to see what else we can discover. 

adf.test(beer.ts)

beer_unitroot <- unitroot_ndiffs(beer.ts)
beer_unitroot

#Earlier we took 1 seasonal difference but this model still shows the same
#problems with seasonality that we have seen so far. Now let's take a second
#seasonal difference to deseasonalize the data and see what effect that has on 
#a linear model and its accuracy.

beer.comp <- decompose(beer.ts)
str(beer.comp)
beer.deseas <- beer.comp$x - beer.comp$seasonal
str(beer.deseas)

beer.deseas.tsb <- as_tsibble(beer.deseas)

fit.beer.deseas.tslm <- beer.deseas.tsb |>
  model(fit.beer.deseas.tslm = TSLM(value ~ trend() + season()))
report(fit.beer.deseas.tslm)

fit.beer.deseas.tslm |> gg_tsresiduals()

fit.beer.deseas.tslm %>%
  accuracy() %>%
  arrange(MAPE)

#The residuals don't look too good, actually worse than before.  Let's look at 
#results from an ADF and a KPSS test to see if the data are stationary.

beer.deseas.ts <- as.ts(beer.deseas.tsb)
str(beer.deseas.ts)

beer.deseas.ts.adf <- adf.test(beer.deseas.ts)
beer.ts.adf

beer.deseas.ts.kpss <- kpss.test(beer.deseas.ts)
beer.deseas.ts.kpss

#The results are consistent that the data are still non-stationary.  The ACF in
#the residuals plot also reflects this.  Let's just plot the PACF and see how it
#looks.  

par(mfrow = c(1,1))
beer.deseas.tsb |>
  PACF() |> autoplot() +
  labs(subtitle = "Quarterly Beer Production")

#Although the ACF looks like even though the periodicity has been removed from 
#the data, the data are non-stationary.  The PACF doesn't really look different 
#than it did before.  Let's try taking two more seasonal differences but this 
#time we'll use the diff() command from the R base package.  That is, we'll twice 
#difference for seasonality. 

diff.once.ts <- diff(beer.ts, lag = 4)
beer.diff.tsb <- as_tsibble(diff.once.ts)

diff.twice.ts <- diff(diff.once.ts, lag = 4)

beer.diff.twice.tsb <- as_tsibble(diff.twice.ts)

fit.beer.twice.tslm <- beer.diff.twice.tsb |>
  model(fit.beer.twice.tslm = TSLM(value ~ trend() + season()))
report(fit.beer.twice.tslm)

fit.beer.twice.tslm |> gg_tsresiduals()

fit.beer.twice.tslm %>%
  accuracy() %>%
  arrange(MAPE)

beer_unitroot2 <- unitroot_ndiffs(diff.twice.ts)
beer_unitroot2

beer.deseas2.tsb <- as_tsibble(diff.twice.ts)

beer.deseas2.tsb |>
  PACF() |> autoplot() +
  labs(subtitle = "Quarterly Beer Production")

diff.twice.ts.adf <- adf.test(diff.twice.ts)
diff.twice.ts.adf

diff.twice.ts.kpss <- kpss.test(diff.twice.ts)
diff.twice.ts.kpss

#I've done this using both the base package and the fable package and
#still get the same answer.  This is a very important result for you to
#consider.  I've discussed it in a bit more detail in the companion Word
#doc for this exam.  




#I have started from the beginning so that I don't get variables mixed up that
#shouldn't be mixed up in this.  

#####****************Part 2 Using the fable package in R****************#############
#
#
# In Part 2 we will look at ARIMA models
#
#

#Libraries
#
library(forecast)
library(zoo)
library(tseries)
library(pastecs)
library(lubridate)
library(ggplot2)
library(ggplottimeseries)
library(reshape2)
library(fable)
library(tsibble)
library(tsibbledata)
library(xts)
library(dplyr)
library(TSstudio)
library(tidyr)
library(tidyverse)
library(astsa)
library(fpp)
library(fpp2)
library(fpp3)
library(xlsx)

beerProd <- read.csv("I:/My Passport Documents/McDaniel/DataAnalytics/ANA535/NewANA535/Exams/Final Exam/beerProd.csv")
beer <- beerProd
colnames(beer) <- c("Date", "beer")
str(beer)

#Create the time series object and the tsibble and check to anchor this Part 2
#to the work before.  If you get a different plot than you did at the beginning
#then you probably have some extra variables running around that you need to
#remove.


beer.ts <- ts(beer$beer, start = c(1956, 1), frequency = 4)
plot(beer.ts, xlab = "Time", ylab = "Beer Production", type = "l")

beer.tsb <- as_tsibble(beer.ts)
beer.tsb

beer.tsb |> 
  autoplot() +
  labs(title = "Australian Beer Production", y = "")


#Once you've verified that you have reloaded the data and created the desired
#data objects move onto building ARIMA models.

auarBeer <- auto.arima(beer.ts)
summary(auarBeer)

diffs <- unitroot_nsdiffs(auarBeer)
diffs

#nvalid is 3 years with a frequency of 4 or quarterly data which equals 12.

nvalid = 12
auarBeer.pred <- forecast(auarBeer, h = nValid)
summary(auarBeer.pred)

plot(auarBeer.pred, xaxt = "n", type = "l")

recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

recent_production |> ACF(Beer, lag_max = 9)

recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title="Australian beer production")

#This is a very interesting ACF plot. The positively correlated lags are 
#positive spikes, the negatively correlated lags (troughs) are negative
#spikes.

#Consider a 4 month moving average for the beer production.  What does that look
#like? There are some years where both peaks and troughs are less than other years.

beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)
beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )

beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )
beer_ma |>
  autoplot(Beer, colour = "gray") +
  geom_line(aes(y = `2x4-MA`), colour = "#D55E00") +
  labs(y = "Beer",
       title = "Total Beer Production")

# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> forecast(h = 14)
# Plot forecasts against actual values
beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

augment(beer_fit)

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
beer_train <- recent_production |>
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit |>
  forecast(h = 10)

beer_fc |>
  autoplot(
    aus_production |> filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)

# Extract data of interest
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
# Define and estimate a model
fit <- recent_production |> model(SNAIVE(Beer))
# Look at the residuals
fit |> gg_tsresiduals()

#Check the unit root tests
fit_adf <- adf.test(beer_train$Beer)
fit_adf

fit_kpss <- kpss.test(beer_train$Beer)
fit_kpss


# Look at some forecasts
fit |> forecast() |> autoplot(recent_production)

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer |>
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )

fc <- aus_production |>
  filter(Quarter < max(Quarter - 11)) |>
  model(
    ETS(Beer),
    SNAIVE(Beer),
    stlm = decomposition_model(STL(log(Beer)), ETS(season_adjust))
  ) |>
  forecast(h = "3 years")
fc |>
  autoplot(filter_index(aus_production, "2000 Q1" ~ .), level = NULL)

fc |> accuracy(aus_production)


fc <- aus_production |>
  filter(Quarter < max(Quarter - 11)) |>
  model(
    ETS(Beer),
    SNAIVE(Beer),
    stlm = decomposition_model(STL(log(Beer)), ETS(season_adjust))
  ) |>
  forecast(h = "3 years")
fc |>
  autoplot(filter_index(aus_production, "2007 Q1" ~ .), level = NULL)
