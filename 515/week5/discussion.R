setwd("D:/Workspace/McDaniel-Repository/515/week5")

housing_data <- read.csv("kc_house_data.csv")

housing_data$grade <- as.factor(housing_data$grade)

ggplot(data = housing_data, aes(x = sqft_living, y = price))+
  geom_point()+
  geom_jitter(aes(colour = grade))+
  scale_x_log10() + 
  scale_y_log10() +
  facet_wrap(~ grade)   
  ggtitle("Seattle housing price")