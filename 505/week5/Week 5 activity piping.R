#Lines 5 through 20 are examples of various file types 
#and the code to read and write them. 
#Your tasks begin at line 22.

#Getting and saving your dataset is typically a two step process
#Read and write a delimited text file.
#datasetname <- read.table(‘file.txt’)
#write.table(datasetname, ‘file.txt’)

#Read and write a comma separated value file. This is a special case of read.table/ write.table.	
#datasetname <- read.csv(‘file.csv’)
#write.csv(datasetname, ‘file.csv’)

#Read and write an R data file, a file type special for R.	
#load(‘file.RData’)
#save(datasetname, file = ‘file.Rdata’)

#Read and write an R data file from GitHub.
#You need to select 'raw data' on the GitHub page 
#and then copy the URL and put in your code, as below

#TASK: run the code below to get and save the dataset
download.file(url = "https://projects.fivethirtyeight.com/soccer-api/international/2022/wc_matches.csv", destfile = "WorldCup.csv")
#Then you need to name your dataset. Run this:
WorldCup<- read.csv("WorldCup.csv")

#TASK: take a look at the World Cup data. 
head(WorldCup)
summary(WorldCup)

#TASK: Install and call the dplyr package. 
install.packages("dplyr")
library("dplyr")
#Let's make a random sample of our data and save it
#Task: run the code below
mysample<-sample_n(WorldCup, size=15, replace = FALSE, weight = NULL, .env = NULL)

#TASK: Save the new sample as a csv file
setwd("/Users/nkohei/Workspace/McDaniel-Repository/505/week5")
current_wd <- getwd()
file_dir <- paste(current_wd,"/test.csv",sep="")
write.csv(mysample, file_dir)

#Now let's have some fun with *piping*

#we will use our mysample dataset
#The pipe, %>%, comes from the magrittr package. 
#Packages in the tidyverse (like dplyr) load %>% for you automatically, 
#so you don’t usually load magrittr explicitly.

#Example: Let's try some piping with our mysample data. Note how the dataset name is not repeated in each function
piping<-mysample %>% 
  rename(SoccerPowerIndex = spi1) %>%
  subset(SoccerPowerIndex >60) %>%
  dim()%>%
  print()

# TASK: revise this code chunk using piping
# Revised idea 1
lookup <- c(Index1="spi1", Index2="spi2")
mysample2 <- mysample %>% 
  arrange(mysample,date) %>%
  filter(spi1<80) %>%
  rename(all_of(lookup))%>%
  select(Index1, Index2, team1, team2)%>%
  summarise(
    Index1 = quantile(Index1, c(0, 0.25, 0.5, 0.75,1)),
    Index2 = quantile(Index2, c(0, 0.25, 0.5, 0.75,1)),
    team1 = n_distinct(team1),
    team2 = n_distinct(team2)
  )%>%
  print()

# Revised idea 2
lookup <- c(Index1="spi1", Index2="spi2")
mysample2 <- mysample %>% 
  arrange(mysample,date) %>%
  filter(spi1<80) %>%
  rename(all_of(lookup))%>%
  select(Index1, Index2, team1, team2)%>%
  summarise(
    Index1_min = min(Index1),
    Index1_1stQuartile = quantile(Index1, 0.25),
    Index1_median = median(Index1),
    Index1_mean = mean(Index1),
    Index1_3rdQuartile = quantile(Index1, 0.75),
    Index1_max = max(Index1),
    Index2_min = min(Index2),
    Index2_1stQuartile = quantile(Index2, 0.25),
    Index2_median = median(Index2),
    Index2_mean = mean(Index2),
    Index2_3rdQuartile = quantile(Index2, 0.75),
    Index2_max = max(Index2),
    team1 = n_distinct(team1),
    team2 = n_distinct(team2)
  )%>%
  print()
