library(ggplot2)
library(dplyr)
library(Amelia)

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw", "vic", "vic", "act")

plot(factor(state))
statef <- factor(state)

levels(statef)
summary(statef)

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)

incmeans <- tapply(incomes, statef, mean)
incmeans

plot(statef, incomes)
hist(incomes)

df <- data_frame(state = state, income = incomes)

df %>%
    group_by(state) %>%
    summarise(incmeans = mean(income))

z <- c(1:3, NA); ind <- is.na(z)
ind
0/0;Inf-Inf
is.na(0/0)

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))

asx1 <- read.csv("../data/ASX_Jan07_UTF8.csv", stringsAsFactors = FALSE)
asx <- read.csv("../data/ASX_Jan07_UTF8.csv")
asx <- tbl_df(asx)

class(asx)
dim(asx)
names(asx)

c <- names(asx)
c[1] <- "ASXCode"
names(asx) <- c

summary(asx)

asx$Growth.Rank[!is.na(asx$Growth.Rank)]
hist(asx$Growth.Rank[!is.na(asx$Growth.Rank)])
with(asx, hist(Growth.Rank))

ggplot(data = asx, aes(x = Growth.Rank)) + geom_histogram()

asx$Growth.Rank[asx$Growth.Rank < 0] <- NA

summary(asx$Growth.Rank)

hist(asx$Risk.Rank)
asx$Risk.Rank[asx$Risk.Rank < 0] <- NA

asx$ROEquity <- as.numeric(sub('%','',as.character(asx$Return.on.Equity)))
asx$EarningsS <- as.numeric(sub('%','', as.character(asx$Earnings.stability)))
asx$PEGrowthRatio <- as.numeric(as.character(asx$P.E.Growth.ratio))

save(asx, file = "../data/lab2.asx.RData")

norm.z.score <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

asx$Price_z <- norm.z.score(asx$Price)

head(asx$Price_z)

asx$Price_z <- NULL

a.out <- amelia(asx, m = 2, idvars = c("X...ASXCode","Company.Name","Industry","P.E.Growth.ratio","Earnings.stability","Return.on.Equity"))

asx.impu.1 <- a.out$imputation[[1]]
asx.impu.2 <- a.out$imputation[[2]]

par(mfrow = c(3,1))
with(asx, hist(Growth.Rank))
with(asx.impu.1, hist(Growth.Rank))
with(asx.impu.2, hist(Growth.Rank))


# Missing data matrix and missing data pattern
# mechanisms lead to missing data
# missing completely at random, missing at random, missing not at random

# outlier - a set of data to be an observation(or subset of observations) which appears to be inconsistent with the remainder of that set of data
# what really matters is whether or not some observations are genuine members of the main population. If they are not, but are contaminants(arising from some other distributions), they may frustrate attempts to draw inference about the original population.


