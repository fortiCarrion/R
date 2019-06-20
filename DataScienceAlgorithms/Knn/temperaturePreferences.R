# Mary and her temperature preferences

#As an example, if we know that our friend Mary feels cold when it is 10 degrees Celsius, but
#warm when it is 25 degrees Celsius, then in a room where it is 22 degrees Celsius, the
#nearest neighbor algorithm would guess that our friend would feel warm, because 22 is
#closer to 25 than to 10.

#Suppose we would like to know when Mary feels warm and when she feels cold, as in the
#previous example, but in addition, wind speed data is also available when Mary was asked
#if she felt warm or cold:

#install.packages("tidyverse")

library(ggplot2)
library(class)

temp <- c(10,25,15,20,18,20,22,24)
speed <- c(0,0,5,3,7,10,5,6)
perception <- c("Cold", "Warm", "Cold", "Warm", "Cold", "Cold", "Warm", "Warm")

cols <- c("Warm" = "red", "Cold" = "blue", "Undefined" = "darkgreen")

df <- data.frame(perception, temp, speed)

trainData <- ggplot(df, aes(x = temp, y = speed, color = perception)) +
  ggtitle("Train data") +
  scale_colour_manual(values = c("blue", "red", "gray")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  geom_point(size = 10) +
  expand_limits(x = 5, y = 10)

trainData

# TEST DATA
testTemp <- c(22,1,11,9)
testSpeed <- c(8,2,0,3)
testPerception <- c("Undefined", "Undefined", "Undefined", "Undefined")

dfTest <- data.frame(perception = testPerception, temp = testTemp, speed = testSpeed)

dfBind <- rbind(df, dfTest)

testData <-  ggplot(dfBind, aes(x = temp, y = speed, color = perception)) +
  ggtitle("Test data") +
  scale_colour_manual(values = c("blue", "red", "gray")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  geom_point(size = 10) +
  expand_limits(x = 5, y = 10)
  
testData

k1 <- knn(train = df[-1], test = dfTest[2:3], cl = perception)  

dfTest2 <- data.frame(perception = as.character(k1), temp = testTemp, speed = testSpeed) 

dfBind2 <- rbind(df, dfTest2)

developData <-  ggplot(dfBind2, aes(x = temp, y = speed, color = perception)) +
  ggtitle("Mary and her temperature preferences") +
  scale_colour_manual(values = c("blue", "red", "gray")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  geom_point(size = 10) +
  expand_limits(x = 5, y = 10)

developData
