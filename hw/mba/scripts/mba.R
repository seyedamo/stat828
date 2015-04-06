library(ggplot2)
library(data.table)
library(arules)
library(reshape2)
library(scales)
library(cluster)
library(fastcluster)


dt <- read.csv("../data/ColesData.csv", header = TRUE, stringsAsFactors = FALSE)
#dt <- fread("../data/ColesData.csv")
dt <- data.table(dt)
dim(dt)
names(dt)
str(dt)
summary(dt)

# creating a back up before data preperation
##dt_bk <- data.table(dt)

# factorization
dt$pmethod <- factor(dt$pmethod, levels = c(1,2,3,4), labels = c("cash", "creditCard", "eftpos", "other"))
levels(dt$pmethod)

dt$sex <- factor(dt$sex, ordered = TRUE, levels = c(1,2), labels = c("male","female"))
levels(dt$sex)

dt$homeown <- factor(dt$homeown, levels = c(1,2,3), labels = c("Yes", "No", "Unknown"))

dt$PostCode <- factor(dt$PostCode)

dt$nchildren <- factor(dt$nchildren, ordered = TRUE)

## factorization of products
## dt[,10:43] <- lapply(dt[, 10:43], as.factor)
## dt[,10:43] <- lapply(dt[, 10:43], FUN = function(x) factor(x, levels = c(0,1), labels = c("No", "Yes")))

ggplot(dt, aes(fruit)) + geom_bar()

dim(dt)

unique(dt$fruit)
dt[, .N, by = fruit][order(-N)]
dt <- dt[fruit == '0' | fruit == '1']
dt[, fruit := as.integer(fruit)]

# missing value
# age, PostCode, cereal
summary(dt$PostCode)
#unique(dt$PostCode[nchar(dt_bk$PostCode) != 4])
dt$PostCode[nchar(dt_bk$PostCode) != 4] <- NA
dt[, list(len = nchar(PostCode))][, .N, by = len][order(len)]

dt[banana == 1 & fruit != 1, .N]
dt[banana == 1 & fruit == 1, .N]


## New features
## New demographics features

names(dt)
quantile(dt$income)
q <- quantile(dt$income, probs = c(0, 0.3, 0.7, 1))
summary(dt$income)

dt[, income.low := as.integer(income < q[2])]
##dt[, a := unlist(lapply(income, function(x) if (x < 50000) 1 else 0))]
ggplot(data = dt, aes(factor(income.low))) + geom_bar()

## dt[, income.low := as.integer(income < q[2])]
## dt[, list(income.low = as.integer(income < q[2]), income.high = as.integer(income > q[3]))]
## dt[, c("income.low", "income.high") := list(as.integer(income < q[2]), as.integer(income > q[3]))]

dt[, c("income.low", "income.high", "income.medium") := list(as.integer(income < q[2]), as.integer(income > q[3]), as.integer(income >=q[2] & income <=q[3]))]

dt[, list(income.low, income.medium,income.high)]

dt[, income.level := cut(income, breaks = c(q[1],q[2],q[3],q[4]), labels = c("low","medium","high"))]

## Category
## why is banana seperated from fruit?
## you can mark fruits to include banana

## count of distinct items in one order
## to answer question like most common item in one-item order etc.
dt[, item.count :=]


## Cluster analysis

cluster <- na.omit(dt[,3:7, with = FALSE])
hc <- hclust(d = daisy(cluster), method = "ave")

cluster.sub <- cluster[1:1000]
d <- daisy(cluster.sub)
hc <- hclust(d = d, method = "ave")
plot(hc, labels = FALSE, hang = -1)
## Markest basket analysis

## descriptive analysis
## graph displays

n = dim(dt)[1]

names(dt)
ggplot(data = dt, aes(sex)) + geom_bar() + ggtitle("bar chart of sex")
table(dt$sex)
table(dt$sex) / n

ggplot(data = dt, aes(PostCode)) + geom_bar() + ggtitle("bar chart of postcode")

ggplot(data = dt, aes(homeown)) + geom_bar() + ggtitle("bar chart of homeown")
table(dt$homeown)
table(dt$homeown) / n

ggplot(data = dt, aes(nchildren)) + geom_bar() + ggtitle("bar chart of nchildren")
table(dt$nchildren)
table(dt$nchildren) / n

ggplot(data = dt, aes(pmethod)) + geom_bar() + ggtitle("bar chart of pmethod")
table(dt$pmethod)
table(dt$pmethod) / n

ggplot(data = dt, aes(Value)) + geom_histogram(binwidth = 20) + ggtitle("histogram of Value")
ggplot(data = dt, aes(Value)) + geom_histogram(binwidth = 50) + ggtitle("histogram of Value")
summary(dt$Value)

ggplot(data = dt, aes(factor(1), Value)) + geom_boxplot() + ggtitle("Value boxplot") + xlab(NULL) + ylab("Value")

ggplot(data = dt, aes(income)) + geom_histogram(binwidth = 5000) + ggtitle("histogram of income") + scale_x_continuous(label = comma)
summary(dt$income)

ggplot(data = dt, aes(factor(1), income)) + geom_boxplot() + scale_y_continuous(label = comma) + xlab(NULL) + ylab("income") + ggtitle("income boxplot")
ggplot(data = dt[income < 150000], aes(income)) + geom_histogram(binwidth = 5000) + ggtitle("histogram of income without the outlier")

ggplot(data = dt, aes(income, Value)) + geom_point(alpha = 0.01)

ggplot(data = dt, aes(x = income.level, y = Value)) + geom_boxplot()

ggplot(data = dt, aes(x = factor(1), y = income)) + geom_boxplot() + scale_y_continuous(label = comma)

ggplot(data = dt, aes(income)) + geom_histogram() + scale_x_continuous(label = comma)

dt[, lapply(.SD, sum), .SD = c("fruit","freshmeat", "dairy")]

bar <- dt[, lapply(.SD, function(x) sum(x,na.rm=TRUE)), .SD = c(10:46)]

bar <- melt(bar)

class(bar)
setnames(bar, c("product","count"))

bar

ggplot(data = bar, aes(x = reorder(factor(product), -count), y = count) ) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dt, aes(pmethod, Value)) + geom_boxplot()

## Unique number of items per order, avg() ?

## scatter plot (bubble plot) transaction value against number of items


## moisac display for several categorical variables

## Trellis display combining up to 8 variables in one plot

## Apriori

items_only <- dt[, 10:46, with = FALSE]
items_only <- data.matrix(na.omit(items_only))

summary(items_only)

?na.omit

str(items_only)

trans <- as(items_only, "transactions")
?transactions

trans

inspect(trans[1:10])

rules <- apriori(items_only, parameter = list(minlen = 2, supp = 0.1, conf = 0.8, maxlen = 5))
summary(rules)

rules.sorted <- sort(rules, by = "lift")
rules.sorted <- sort(rules, by = "confidence")

inspect(rules.sorted[1:10])

