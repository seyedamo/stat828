#lec

library(ggplot2)
#library(dplyr)
library(data.table)

pid <- read.csv("../data/pima-indians-diabetes.csv", header = TRUE, stringsAsFactors = FALSE)
summary(pid)

pid <- data.table(pid)
#dt <- fread("../data/pima-indians-diabetes.csv")

pid$DBP <- pid$Diastolic.blood.pressure

plot <- ggplot(data = pid, aes(x = DBP))

plot + geom_histogram(fill = "white", colour = "black", binwidth = 3)
plot + geom_freqpoly(colour = "blue")
plot + geom_density()

pid$Class <- factor(pid$Class)

ggplot(data = pid, aes(x = c(1:length(DBP)), y = DBP)) + geom_line(colour = "blue")

ggplot(data = pid, aes(x = Class, y = DBP)) + geom_boxplot()
ggplot(data = pid, aes(x = Class, y = Age)) + geom_boxplot()

boxplot(pid$DBP)

tapply(pid$DBP, pid$Class, mean)
pid[, mean(DBP), by = Class]
pid[, .(mean(DBP), sd(DBP)), by = Class]
#ans <- pid[, c("mean","sd","n", "median") := list(mean(DBP), sd(DBP), .N, median(DBP)), by = Class]

pid[, .(mean = mean(DBP), sd = sd(DBP), N = .N, median = median(DBP)), by = Class]

ggplot(data = pid, aes(x = DBP, fill = Class)) + geom_histogram(binwidth = 2, position = "identity", alpha = .5)

ggplot(data = pid, aes(x = DBP)) + geom_histogram(binwidth = 2, colour = "black", fill = "white") + facet_grid(Class ~ .)

ggplot(data = pid, aes(x = Class, y = DBP)) + geom_boxplot()

library(GGally)
ggpairs(data = pid)

ggplot(data = pid, aes(x = Age, y = DBP)) + geom_point() + facet_grid(.~Class)

pid$pregnantF <- factor(pid$Number.of.times.pregnant)

ggplot(data = pid, aes(x = pregnantF, fill = Class)) + geom_bar()
ggplot(data = pid, aes(x = pregnantF, fill = Class)) + geom_bar(position = "dodge")

## start working on variables

ggplot(data = pid, aes(pregnantF)) + geom_bar()

ggplot(data = pid, aes(Plasma.glucose.concentration)) + geom_histogram(binwidth = 5)

pid[Plasma.glucose.concentration == 0, .N]
pid[, pgc := ifelse(Plasma.glucose.concentration == 0, NA, Plasma.glucose.concentration)]

ggplot(data = pid, aes(pgc)) + geom_histogram(binwidth = 5)

summary(pid$pgc)
summary(pid$Plasma.glucose.concentration)

ggplot(data = na.omit(pid), aes(x = pgc, y = Plasma.glucose.concentration)) + geom_line()

ggplot(data = na.omit(pid), aes(x = Class, y = Diastolic.blood.pressure)) + geom_boxplot()

## scatter plot with too many data points could conceal more than they can reveal
ggplot(data = pid, aes(x = Age, y = Body.mass.index, colour = factor(Class))) + geom_point() + stat_smooth()

