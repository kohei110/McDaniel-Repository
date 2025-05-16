#
#Lab1Complete.R
#Written by Marvine Hamner
#February 25, 2025
#

# install.packages(c("xlsx", "forecast", "dplyr", "tidyverse", "ggplot2", "tsibble", "tsibbledata", "fable", "feasts", "lubridate", "zoo", "fpp3"))
# install.packages("rJava", type = "source")
# 
library(xlsx)
library(forecast)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(lubridate)
library(zoo)
library(fpp3)

#
#****************** Step 1 **************************
#
#Import the data.
#

setwd("/Users/nkohei/Workspace/McDaniel-Repository/535/lab1")

#
#Remember that there is something wrong or at least inconsistent between
#the number of RidersReported and the number of riders in Ridership.  I
#do not know what the source of the problem is.  Use the variable Ridership.
#Do NOT use the variable RidersReported.  Only the first few months are
#different but they do appear to be the wrong values!
#

Amtrak <- read.csv("Amtrak1991-2024.csv")
colnames(Amtrak) <- c("Date", "Ridership", "PassengerMiles", "RidersReported")
View(Amtrak)

mean(Amtrak$Ridership)
mean(Amtrak$PassengerMiles)
min(Amtrak$Date)
max(Amtrak$Date)

#
#One of the first things you need to do is get the dates in a format that
#can be used by the code.  The following commands will do that.  There
#are many ways to accomplish this.  If you want to do it differently that
#is ok.  But you will want to make sure that whatever you use does change
#the format to that produced by these commands.
#
#The str() command will tell you what the format is for each of the
#variables in the dataset.  
#

Amtrak$Date <- as.Date(Amtrak$Date, format = "%m/%d/%Y")
str(Amtrak)

#
#****************** Step 2 ***************************
#
#Determine if there are any missing values in the dataset.
#

missing <- sum(which(is.na(Amtrak)))
missing


summary(Amtrak)

#
#Convert the format of the data to a tibble. 
#

Amtrak <- as_tibble(Amtrak)

Amtrak

#
#*************************** Step 3 ********************
#
#Generate a time plot of the data.
#

#Now let's look at plotting the Amtrak data as a time plot and adding
#a trend line to the plot.  First we need to get the data setup as a
#time series.  We'll use the ts() command to do that.  z
#

Amtrak.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.ts)
plot(Amtrak.ts, xlab = "Time", ylab = "Ridership om 1,000's", bty = "l")

#
#Let's look at passenger miles and see if that makes a difference
#

Amtrak.ts <- ts(Amtrak[,"PassengerMiles"], start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.ts)
plot(Amtrak.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")

#
#This does make an interesting plot.  It is easier to see the effect of the 
#economic downturn in 2008.  Plus, what was really going on in the 1990's that
#made such a long downturn for passengers riding Amtrak?  Obviously there is
#enough going on that a simple quadratic will not model PassengerMiles.  
#
#
#Also, I don't want to call this the "old" way because that isn't really 
#the correct way to think about this.  But, this way does not require
#the use of tsibbles.
#

#
#************************ Step 4 **************************
#
#Filter the data to examine a one year period between January 1, 1999 and
#December 31, 2000. Note that we are not saving this one year period to
#another data object. 
#
#Note that using this syntax for the command can be interpreted as an
#action on the dataset named without saving that action.  That is, the
#original dataset Amtrak remains unchanged! 
#

Amtrack_filtered <- Amtrak %>% 
  filter(between(Amtrak$Date, as.Date('1999-01-01'), as.Date('2000-12-31')))

install.packages("palmerpenguins")
library(palmerpenguins)
data(package = "palmerpenguins")
penguins
dplyr::glimpse(penguins)

nrow(filter(penguins, 'sex' == male))
count(filter(penguins, sex == 'male'))
length(filter(penguins, sex == 'male'))


#
#Then rearrange the Amtrak data into descending order. Since we did
#not save the filtered data we will get the entire dataset in descending
#order. 
#

Amtrak %>% arrange(desc(Amtrak$Date))
Amtrak

Amtrak.ts1999 <- ts(Amtrak[,"PassengerMiles"], start = c(1991, 1), end = c(2000, 12), frequency = 12)
str(Amtrak.ts1999)

#
#Let's look at a different way to filter the data for a specified 
#period of time. Before we pull one year of data out let's go back 
#to our time series and consider the date format. When we used the ts()
#command to generate the time series we designated it as monthly
#by Amtrak$Date. So, we'll need to use that to enter the
#correct syntax to get one year's (or any one period) worth of data 
#from the time series.  
#

Amtrak.1999 <- Amtrak %>%
  filter(format(Amtrak$Date, "%Y") == "1999")
str(Amtrak.1999)
Amtrak.1999
plot(Amtrak.ts1999, xlab = "Time", ylab = "Ridership", bty = "l")

#
#Students seem to want to go straight to the ACF now.  That is not
#correct because we have not ensured that the data are stationary 
#yet. We really haven't even considered if the data are stationary.
#　We'll just consider PassengerMiles over the years from 1991 to 2016 
#　for now. And, we'll put the data in a tsibble first.  
#

Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.16 <- as_tsibble(Amtrak.ts.91.16)
Amtrak.tsb.91.16 |>
  ACF() |> autoplot() 
  labs(subtitle = "Amtrak Passenger Miles")


#
#This plot shows that there is both autocorrelation illustrating a
#trend in the data as well as periodicity in the data. That is, the
#data are not yet stationary!
#

#
#********************* Step 5 ***********************
#
#Now we want to add a trendline to the time plot of the data. We'll looke
#at adding a trendline to several different ways of plotting the time plot. 
#Let's start with what would have been used over the past years first.
#I started as we usually would and just added I(trend^2) for a "squared"
#term.  This really didn't fit the data very well so I added another
#term, I(trend^3) for a cubed term.  That looks a lot better.  It
#does not capture the downturn do to the lockdowns during Covid but
#it does capture the general trend before that.  
#

Amtrak.Miles.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.Miles.ts)
plot(Amtrak.Miles.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")

Amtrak.Miles.lm <- tslm(Amtrak.Miles.ts ~ trend + I(trend^2) + I(trend^3))

par(mfrow = c(1,1))
plot(Amtrak.Miles.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")
lines(Amtrak.Miles.lm$fitted, lwd = 2)
Amtrak.Miles.ts.zoom <- window(Amtrak.Miles.ts, start = c(1997, 1), end = c(2000, 12))
plot(Amtrak.Miles.ts.zoom, xlab = "Time", ylab = "Passenger Miles", bty = "l")

Amtrak.Miles.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)

length(Amtrak.Miles.ts)

# 1. Subtract the mean before FFT (a common practice to remove the "DC component")
data_centered <- Amtrak.Miles.ts - mean(Amtrak.Miles.ts, na.rm = TRUE)

# 2. Perform the FFT
amtrak_fft <- fft(data_centered)

# 3. Compute the magnitude (absolute value of the complex numbers)
amtrak_fft_mag <- Mod(amtrak_fft)

# 4. (Optional) For a simpler plot, plot half of it, as the second half often mirrors the first
n <- length(amtrak_fft_mag)
half <- seq_len(n %/% 2)

# 5. Plot the magnitude of the FFT
plot(
  half,
  amtrak_fft_mag[half],
  type = "h",
  main = "FFT Magnitude for Passenger Miles",
  xlab = "Frequency Index",
  ylab = "Magnitude"
)

par(mfrow = c(2,1))
plot(Amtrak.Miles.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")

lines(Amtrak.Miles.lm$fitted, lwd = 2)

str(Amtrak)
Amtrak.riderships.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.riderships.ts)
plot(Amtrak.riderships.ts, xlab = "Time", ylab = "Ridership", bty = "l")

Amtrak.riderships.lm <- tslm(Amtrak.riderships.ts ~ trend + I(trend^2))

par(mfrow = c(1,1))
plot(Amtrak.riderships.ts, xlab = "Time", ylab = "Ridership", bty = "l")
lines(Amtrak.riderships.lm$fitted, lwd = 2)

#
#Now let's do this using a newer method by the textbook and use a 
#tsibble. Again, we'll just look at Passenger Miles. We'll use 
#both autoplot() and commands from the ggplot2 package. Again, I've
#added a 3rd order or cubed term so the trendline better matches 
#the data.
#

Amtrak.Miles.tsb <- as_tsibble(Amtrak.Miles.ts)
Amtrak.Miles.tsb

autoplot(Amtrak.Miles.tsb) +
  labs(y = "Passenger Miles", x = "Date")

Amtrak.Miles.tsb |>
  ggplot(aes(x = Amtrak.Miles.tsb$index, y = Amtrak.Miles.tsb$value)) +
  labs(y = "Passenger Miles", x = "Date") +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3)", se = FALSE)

#
#Let's compare this to a plot of the data from earlier methods
#

str(Amtrak)
Amtrak.Miles <- Amtrak %>% select(c(Date, PassengerMiles))
str(Amtrak.Miles)
Amtrak.Miles.ts <- ts(Amtrak.Miles[, "PassengerMiles"], start = c(1991, 1), end = c(2024, 6), frequency = 12)
Amtrak.Miles.tsb <- as_tsibble(Amtrak.Miles.ts[,"PassengerMiles"])
Amtrak.Miles.tsb
str(Amtrak.Miles.tsb)

par(mfrow = c(1,1))
Amtrak.Miles.ts <- ts(Amtrak.Miles[,"PassengerMiles"], start = c(1991, 1), end = c(2024, 6), frequency = 12)
str(Amtrak.Miles.ts)
plot(Amtrak.Miles.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l")


nrow(filter(Amtrak.Miles, 'Date' == '1991')

str(Amtrak.Miles.ts[,"PassengerMiles"])

ggplot(Amtrak.Miles.tsb, mapping = aes(x = index, y=value)) +
  geom_point() +
  geom_smooth(aes(x = index, y=value), 
              method = "lm", formula = y ~ x + I(x^2) + I(x^3))

#
#Here you can see I left on the gray representing the confidence interval
#on the plot. I also went ahead and used the tsibble since we
#generated it.  
#
#It is really hard to tell what is going on with each data point in a scatter
#plot.  But it obviously paints a different picture than the previous plot.  
#Let's go back to a line plot and see if that helps. I'll plot
#Ridership first and then Passenger Miles. 
#

par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Date, y=Ridership)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = Ridership), method = "lm", formula = y ~ x + I(x^2))

#
#Let's also look at passenger miles.  The commands for the plot are given below.
#

par(mfrow = c(1,1))
ggplot(Amtrak, aes(x=Date, y=PassengerMiles)) +
  geom_line(color = "blue") +
  stat_smooth(aes(y = PassengerMiles), method = "lm", formula = y ~ x + I(x^2) +I(x^3))


#
#That's better.  We can see what the effect of shutdown(s) during Covid really
#was.  It is also obvious that our quadratic model is not really working.  
#There is also a bit of a problem in that when a passenger buys a ticket they may
#be traveling 100, 200, or even over 1,000 miles.  So, just counting the number
#of passengers probably won't produce the best picture of what is going on overall.
#I am going to suggest using "Passenger Miles" as the dependent variable.  I 
#think that makes a lot more sense.  
#

#
#******************** Step 7 ***********************
#
#Now let's take a look at how we can use other packages in R
#Let's use the additional hierarchical sales data file to do that.
#


library(fable)
library(tsibbledata)
library(tsibble)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(feasts)
library(janitor)
library(reshape)

sales <- read.csv("hierarchical_sales_data.csv")

str(sales)

# Parse the DATE column
sales$DATE <- as.Date(sales$DATE, format = "%m/%d/%Y")

# Convert wide -> long format
sales_long <- sales %>%
  pivot_longer(
    cols = -DATE,          # everything except the DATE column
    names_to = "product",  # name of the 'key' variable
    values_to = "quantity" # name of the value column
  )

# Optional: clean names if needed
# sales_long <- clean_names(sales_long)
View(sales)
glimpse(sales_long)

sales_tsb <- sales_long %>%
  as_tsibble(
    index = DATE,   # your time column
    key   = product # the grouping variable
  )

# Confirm structure
sales_tsb %>%
  filter(product %in% c("QTY_B1_1", "QTY_B1_2")) %>%
  autoplot(quantity) +
  labs(y = "Quantity Sold", x = "Date",
       title = "Hierarchical Sales: Time Series (Selected Products)")


#View(hierarchical_sales_data) 
# View(sales)
# 
# 
varlist <- names(sales[1:length(sales)])
str(varlist)

df10 <- sales[,1:11]
df10$DATE <- mdy(df10$DATE)
df10tb <- as_tibble(df10)
str(df10tb)

df10tb_long <- df10tb %>%
  pivot_longer(-DATE)
str(df10tb_long)
head(df10tb_long)

sales10.tsb <- as_tsibble(df10tb_long, index = DATE, key = name)
str(sales10.tsb)

sales10.tsb %>%
  autoplot(value)

sales$Date <- as.Date(sales$DATE, format = "%m/%d/%Y")


sales

sales <- ts(sales$QTY_B1_1, start = c(2014, 1), end = c(2018, 12), frequency = 12)
str(sales)
plot(sales, xlab = "Time", ylab = "Ridership om 1,000's", bty = "l")
