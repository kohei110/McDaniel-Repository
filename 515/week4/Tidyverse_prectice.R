# install.packages("tidyverse")
library(tidyverse)


raw_data <- read.csv("https://github.com/rstudio/learning-r-survey/raw/master/2019/data/2019%20English%20R%20Community%20Survey%20Responses.csv", fileEncoding = "UTF-8")
str(raw_data[1:5])
str(raw_data$In.what.country.do.you.currently.reside.)

class(raw_data$What.year.did.you.first.start.learning.R.)

names(raw_data[1:5])

head(raw_data[1:2])


renamed <- raw_data %>%
rename("Qr_experience" = "How.would.you.rate.your.level.of.experience.using.R.")

names(renamed[1:2])

renamed <- raw_data %>%
  rename("Qr_experience" = "How.would.you.rate.your.level.of.experience.using.R.",
         "Qr_difficulty_experienced" = "Compared.with.other.technical.topics.you.ve.learned.in.school.and.on.the.job..on.a.scale.of.1.to.5..how.difficult.do.you.expect.learning.R.to.be.")

# new name
names(renamed[2:3])



names(renamed) <- c("Q1","Q2", "Q3")
names(renamed[1:2]) 

names(renamed)

qnames <- read_tsv("https://raw.githubusercontent.com/rstudio/learning-r-survey/master/2019/data/2019-english-question-names-only.tsv")

rsurvey <- raw_data

# rename the columns based on qnames
names(rsurvey) <- names(qnames)

# 2.5
# install.packages("janitor") #useful

rsurvey <- rsurvey %>%
  janitor::clean_names()

names(rsurvey)

#install.packages("psych")

library("psych")
rsurvey$age <- 2020-rsurvey$qyear_born
describeBy(rsurvey$age, group=rsurvey$qr_experience, mat=TRUE)


summary(rsurvey[2:3])
summary(as.factor(rsurvey$qr_experience))

#install.packages("skimr")

library("skimr") # formatted well
skim(rsurvey[2:3])

table(rsurvey$qcountry, rsurvey$qr_experience)[1:5,]

#2.6
keep <- rsurvey %>% select(qr_experience, qr_year)
dropped <- rsurvey %>% select(-qr_experience, -qr_year)

#apply multiple columns altogether
keep_multi <- rsurvey %>%
  select(qr_experience, -qr_year, qr_difficulty:qobstacles_to_starting)


#2.8

# install.packages("jmv")
library("jmv")
library("tidyverse")
rsurvey %>%
  select(qr_experience, qr_year) %>%
  descriptives(freq = TRUE)

#2.9
rsurvey %>% 
  select(qr_year) %>% 
  #arrange(qr_year) %>% #order?
  head(n = 10)

#2.10
rsurvey <- rsurvey %>% 
  mutate(qr_year2 = ifelse(qr_year < 1977,NA, qr_year))

rsurvey %>% 
  select(qr_year2, qr_year) %>% 
  arrange(qr_year) %>% #order?
  head(n = 10)


#run this to check
rsurvey %>%
  select(qr_year, qr_year2) %>%
  arrange(qr_year) %>%
  head(n=10)

#2.11

rsurvey %>% count(qr_experience)
recoded <- rsurvey %>% 
  select(qr_experience) %>%
  mutate(qr_experience2 = factor(qr_experience,
                                 levels=c("None","Beginner", "Intermediate", "Expert", NA ))
  )

recoded %>% count(qr_experience2)

#2.16
plot(rsurvey$age)

boxplot(rsurvey$age)


#install.packages("effectsize")

rsurvey$z_of_age <- effectsize::standardise(rsurvey$age)

summary(rsurvey$z_of_age)

rsurvey %>% 
  filter(age < 100) %>% 
  with(plot(age))


#2.17
hist(rsurvey$age)
qqnorm(rsurvey$age)
shapiro.test(rsurvey$age)