
## lab1
## graph from lecture notes

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

## Maindonald and Braun advance.R script
## search attached objects
search()

## working with date and time
dd <- as.Date(c("2015-02-28", "2014-02-28"))
diff(dd)
dd <- as.Date(c("2015-02-28", "2015-02-29"))
diff(dd)

as.Date("1/1/1999", format = "%d/%m/%Y")

julian(dd)
weekdays(dd)
months(dd)
quarters(dd)

format(dd, format = "%b %d %Y")
format(dd, format = "%a %b %d %Y")
