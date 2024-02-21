library(ggplot2)
library(tidyverse)
library(janitor) 
setwd("D:\\Workspace\\McDaniel-Repository\\515\\week6")



## Q1 Read dataset
data <- read.csv("StormEvents_details-ftp_v1.0_d1988_c20220425.csv", header = TRUE)


## Q2 column selection
col_list <- c("BEGIN_DAY", "BEGIN_YEARMONTH","END_YEARMONTH",
"END_DAY", "STATE", "STATE_FIPS", "CZ_TYPE", "CZ_FIPS", "MAGNITUDE")

flt_data <- data %>% 
  select(col_list)


#Q3 Arrange
arranged_data <- flt_data %>% 
  arrange(STATE)
 
#Q4 lowercase
cln_data <- arranged_data %>% 
  clean_names()

#Q5 filtering
country_data <- cln_data %>%
  filter(cz_type == "C") %>% 
  select(-cz_type)

#Q6 padding and mutate
#?str_pad

pad_data <- country_data %>%
  mutate(
    new_fips =
    paste0(str_pad(state_fips, 2, pad = "0"),
      str_pad(cz_fips, 3, pad = "0"))
  )

#Q7 rename
?rename_all 
lwr_data <- pad_data %>%
  rename_all(tolower)

#Q8 Select three columns
data("state")

state_data <- tibble(
  name = state.name,
  area = state.area,
  region = state.region
)

#Q9 groupby and join
evnt_data <- lwr_data %>% 
  group_by(state)%>%
  summarise(evnt_cnt = n())

evnt_lwr_data <- evnt_data %>%
  mutate(state = tolower(state))

state_lwr_data <- state_data %>%
  mutate(name = tolower(name))

joined_data <- left_join(evnt_lwr_data, state_lwr_data, by=c("state" = "name"))

#Q10
ggplot(data = joined_data, aes(x = area, y = evnt_cnt)) +
  geom_point(aes(colour = region))+
  labs(x ="Land area(square miles)", y = "# of storm events in 1988", 
       title = "Plot for Question 10")
