data(women)

head(women)
min(women$height)
max(women$height)
mean(women$height)

getwd()

str(women)
summary(women)


just_height <- (women$height)
head(just_height)
head(women)
subset(women, height>65)

newwomen <- subset(women, height>65)

nrow(women)
nrow(newwomen)

ncol(women)
ncol(newwomen)

dim(newwomen)
