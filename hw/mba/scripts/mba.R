library(ggplot2)
library(data.table)
library(arules)
library(reshape2)
library(scales)
library(cluster)
library(fastcluster)

## read file
dt <- read.csv("../data/ColesData.csv", header = TRUE, stringsAsFactors = FALSE)
#dt <- fread("../data/ColesData.csv")
dt <- data.table(dt)
## description
dim(dt)
## [1] 55937    43
names(dt)
##  [1] "ReceiptID"        "Value"            "pmethod"          "sex"
##  [5] "homeown"          "income"           "age"              "PostCode"
##  [9] "nchildren"        "fruit"            "freshmeat"        "dairy"
## [13] "MozerallaCheese"  "cannedveg"        "cereal"           "frozenmeal"
## [17] "frozendessert"    "PizzaBase"        "TomatoSauce"      "frozen.fish"
## [21] "bread"            "milk"             "softdrink"        "fruitjuice"
## [25] "confectionery"    "fish"             "vegetables"       "icecream"
## [29] "energydrink"      "tea"              "coffee"           "laundryPowder"
## [33] "householCleaners" "corn.chips"       "Frozen.yogurt"    "Chocolate"
## [37] "Olive.Oil"        "Baby.Food"        "Napies"           "banana"
## [41] "cat.food"         "dog.food"         "mince"

str(dt)
summary(dt)

## create two copies
dt.cluster <- dt[, 2:9 ,with = FALSE]
dt.arules <- dt[, c(1, 6, 10:43), with = FALSE]
names(dt.cluster)
names(dt.arules)

# factorization
dt.cluster$pmethod <- factor(dt$pmethod, levels = c(1,2,3,4), labels = c("cash", "creditCard", "eftpos", "other"))
dt.cluster$sex <- factor(dt$sex, ordered = TRUE, levels = c(1,2), labels = c("male","female"))
dt.cluster$homeown <- factor(dt$homeown, levels = c(1,2,3), labels = c("Yes", "No", "Unknown"))
dt.cluster$nchildren <- factor(dt$nchildren, ordered = TRUE)

## no need since Arules use integer value 1/0
## factorization of products
## dt[,10:43] <- lapply(dt[, 10:43], as.factor)
## dt[,10:43] <- lapply(dt[, 10:43], FUN = function(x) factor(x, levels = c(0,1), labels = c("No", "Yes")))

## fruit correction
ggplot(dt, aes(fruit)) + geom_bar()
unique(dt$fruit)
dt[, .N, by = fruit][order(-N)]

## dt.arules is the dataset for frequency analysis
dt.arules <- dt.arules[fruit == '0' | fruit == '1']
dt.arules[, fruit := as.integer(fruit)]

## fruit & banana conflicting info
## set fruit 1 when banana is 1
dt.arules[banana == 1 & fruit != 1, .N]
## [1] 14337
dt.arules[banana == 1 & fruit == 1, .N]
## [1] 29353
dt.arules[banana == 1 & fruit != 1, fruit := 1]
dt.arules[, .N, by = fruit]

# missing value
# age, PostCode, cereal
# postcode is not used
summary(dt$PostCode)
#unique(dt$PostCode[nchar(dt_bk$PostCode) != 4])
#dt$PostCode[nchar(dt_bk$PostCode) != 4] <- NA
dt[, list(len = nchar(PostCode))][, .N, by = len][order(len)]
dt.cluster[, PostCode := NULL]
names(dt.cluster)

# age NA remove
dt.cluster <- na.omit(dt.cluster)

# cereal NA remove
dt.arules <- na.omit(dt.arules)

# outliers
dt.cluster[income > 150000, income]
# create new categorical income variable to counter this

## New features
## New demographics features
## income level
quantile(dt$income)
## 0 - 30% low, 30% - 70% medium, 70% - 100% high for cluster analysis
q <- quantile(dt$income, probs = c(0, 0.3, 0.7, 1))
dt.cluster[, income.level := cut(income, breaks = c(q[1],q[2],q[3],q[4]), labels = c("low","medium","high"))]

## binary indicators for Arules
dt.arules[, c("income.low", "income.high", "income.medium") := list(as.integer(income < q[2]), as.integer(income > q[3]), as.integer(income >=q[2] & income <=q[3]))]
dt.arules[, income := NULL]

## some sanity checks
ggplot(data = dt.arules, aes(factor(income.low))) + geom_bar()
ggplot(data = dt.cluster, aes(income.level)) + geom_bar()


## Cluster analysis
## hierarchy cluster is used because the mixed categorical and continuous variables nature of the dataset
d <- daisy(dt.cluster)
hc <- hclust(d = d, method = "ave")

cluster.sub <- dt.cluster[1:10000]
d <- daisy(cluster.sub)
hc <- hclust(d = d, method = "ave")

plot(hc, labels = FALSE, hang = -1)

memb <- cutree(hc, k = c(2:10))

head(memb)

cluster.sub[, `:=`(group2 = memb[,1], group3 = memb[,2], group4 = memb[,3], group5 = memb[,4], group10 = memb[,9])]

names(cluster.sub)
cluster.sub[, lapply(.SD, mean), by = group2, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group3, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group4, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group5, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group10, .SD = c(1:8)]

## graphics
names(cluster.sub)
ggplot(cluster.sub, aes(income.level)) + geom_bar(aes(fill = factor(group10)), alpha = 0.5)
ggplot(cluster.sub, aes(sex)) + geom_bar(aes(fill = factor(group10)), alpha = 0.5)
ggplot(cluster.sub, aes(pmethod)) + geom_bar(aes(fill = factor(group10)), alpha = 0.5)
ggplot(cluster.sub, aes(homeown)) + geom_bar(aes(fill = factor(group10)), alpha = 0.5)
ggplot(cluster.sub, aes(homeown)) + geom_bar(aes(fill = factor(group10)), alpha = 0.5)
ggplot(cluster.sub, aes(nchildren)) + geom_bar(aes(fill = factor(group10)), alpha = 0.5)

ggplot(cluster.sub, aes(factor(group5))) + geom_bar(aes(fill = pmethod))
ggplot(cluster.sub, aes(factor(group5))) + geom_bar(aes(fill = income.level))
ggplot(cluster.sub, aes(factor(group5))) + geom_bar(aes(fill = sex))

## Examine clusters to determine characteristics unique to that cluster.
## Examine fields across clusters to determine how values are distributed among clusters.

## boxplot?


## trellis graph?

## key difference in clusters
## income.level, remove it then repeat

names(cluster.sub)
cluster.sub[, `:=`(income = NULL, income.level = NULL)]
d <- daisy(cluster.sub)
hc <- hclust(d = d, method = "ave")
memb <- cutree(hc, k = c(2:10))
cluster.sub[, `:=`(group2 = memb[,1], group3 = memb[,2], group4 = memb[,3], group5 = memb[,4], group10 = memb[,9])]
cluster.sub[, lapply(.SD, mean), by = group2, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group3, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group4, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group5, .SD = c(1:8)]
cluster.sub[, lapply(.SD, mean), by = group10, .SD = c(1:8)]
## sex is also crucial

## descriptive analysis

## count of distinct items in one order
tmp <- names(dt.arules)[2:35]


tmp <- dt.arules[,list(count = fruit+freshmeat+dairy+MozerallaCheese+cannedveg+cereal+frozenmeal+frozendessert+PizzaBase+TomatoSauce+frozen.fish+bread+milk+softdrink+fruitjuice+confectionery+fish+vegetables+icecream+energydrink+tea+coffee+laundryPowder+householCleaners+corn.chips+Frozen.yogurt+Chocolate+Olive.Oil+Baby.Food+Napies+banana+cat.food+dog.food+mince)][, .N, by = count][order(-count)]

## a normal shape distribution
ggplot(tmp, aes(count, N)) + geom_bar(stat = "identity")

## count vs value ?

## to answer question like most common item in one-item order etc.

names(dt.arules)
dt.arules[, list(sum(fruit), sum(freshmeat))]

tmp <- dt.arules[, lapply(.SD, sum), .SD = c(2:35)]
tmp

ggplot(melt(tmp), aes(x = reorder(factor(variable), -value), y = value)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("common items") + xlab("line item") + ylab("transaction count")

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

# ggplot(data = dt, aes(income, Value)) + geom_point(alpha = 0.01)

# ggplot(data = dt.cluster, aes(x = income.level, y = Value)) + geom_boxplot()

# ggplot(data = dt, aes(x = factor(1), y = income)) + geom_boxplot() + scale_y_continuous(label = comma)

# ggplot(data = dt, aes(income)) + geom_histogram() + scale_x_continuous(label = comma)

dt[, lapply(.SD, sum), .SD = c("fruit","freshmeat", "dairy")]

bar <- dt[, lapply(.SD, function(x) sum(x,na.rm=TRUE)), .SD = c(10:46)]

bar <- melt(bar)

class(bar)
setnames(bar, c("product","count"))

bar



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

