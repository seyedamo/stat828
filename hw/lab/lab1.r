
attach(faithful)
?faithful
names(faithful)
summary(faithful)
hist(eruptions)
hist(eruptions, seq(1.6, 5.2 ,0.2), prob=TRUE)
lines(density(eruptions, bw = 0.1))
?density
rug(eruptions)
?rug

plot(eruptions, waiting)
plot(eruptions, waiting, xlab = "Eruptions", ylab = "Waiting", pch = 3)
abline(lm(waiting ~ eruptions))
lines(c(1.5,5), c(50,90), lty = 2)
lines(c(1.5,5), c(45,90), lty = 2)
title(main = "Scatter Plot with Regression lines")

getwd()
weather <- read.table("/Users/yangzhou/STAT/stat828/hw/data/weather.txt", 
                      header = TRUE)
attach(weather)
names(weather)
weather$monthF <- factor(month)
is.factor(weather$monthF)
plot(weather$monthF, upper, xlab = "Month", ylab = "Temperature", 
     main = "Boxplot of upper temperatures for each month")


names(weather)
pairs(weather[-c(5,6)])
pairs(weather[-c(5,6)], main = "Scatter Plot Matrix of Weather Data", panel = panel.smooth)
