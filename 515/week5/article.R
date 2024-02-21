library(tidyverse)
library(ggplot2) 

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut))


diamonds %>% 
  count(cut)

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=carat), binwidth = 0.5)

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x=carat)) +
  geom_histogram(binwidth=0.1)

ggplot(data = smaller, mapping = aes(x=carat, colour = cut))+
  geom_freqpoly(binwidth = 0.1)

ggplot(data = faithful, mapping = aes(x=eruptions))+
  geom_histogram(binwidth = 0.25)


ggplot(diamonds)+
  geom_histogram(mapping = aes(x=y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>%
  select(price, x, y,z) %>%
  arrange(y)

print(unusual)

?diamonds 
# 7.3.4 exercise
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x=x))

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=price), binwidth = 100)

carat099 <- diamonds %>% 
  filter(carat == 0.99)%>% 
  count()

carat1 <- diamonds %>% 
  filter(carat == 1.0)%>% 
  count()

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x=x))+
  coord_cartesian(xlim = c(0,5), ylim=c(0,100))

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x=price),binwidth = 50)+
  coord_cartesian(ylim=c(0,100))

#7.4.1 exercise

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, aes(x=y))+
  geom_histogram()

ggplot(data = diamonds2)+
  geom_bar(mapping=aes(x=y))

mean(diamonds2$y)
mean(diamonds2$y, na.rm = TRUE)

sum(diamonds2$y)
sum(diamonds2$y, na.rm = TRUE)

# 7.5.1 Excercise
library(nycflights13)
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) 

# Calculating correlation of price with other numerical variables
correlations <- cor(diamonds %>% 
                      select(-c(cut, color, clarity)), use = "complete.obs"
                    )

correlations_price <- correlations["price",]

ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Carat") +
  ylab("Price")

# install.packages("ggstance")
library(ggstance)

diamonds

ggplot(data=diamonds, aes(x = cut, y= price))+
  geom_boxploth()
  

ggplot(data=diamonds, aes(x = cut, y= price))+
  geom_boxplot()+
  coord_flip()

# install.packages("lvplot")
library(lvplot)

ggplot(data=diamonds, aes(x = cut, y= price))+
  geom_boxplot()+
  geom_lv()

ggplot(data=diamonds)+
  geom_histogram(mapping = aes(x=price))+
  facet_wrap(~ cut)   



# install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.5) +
  geom_jitter(aes(colour = cut))