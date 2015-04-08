library(ggplot2)
library(data.table)
library(arules)
library(reshape2)
library(scales)
library(cluster)
library(fastcluster)
library(arulesViz)

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

## missing values
# age, PostCode, cereal
summary(dt)
# missing value
dt <- na.omit(dt)

# factorization
dt$pmethod <- factor(dt$pmethod, levels = c(1,2,3,4), labels = c("cash", "creditCard", "eftpos", "other"))
dt$sex <- factor(dt$sex, levels = c(1,2), labels = c("male","female"))
dt$homeown <- factor(dt$homeown, levels = c(1,2,3), labels = c("Yes", "No", "Unknown"))
# dt$nchildren <- factor(dt$nchildren, ordered = TRUE)

## no need since Arules use integer value 1/0
## factorization of products
## dt[,10:43] <- lapply(dt[, 10:43], as.factor)
## dt[,10:43] <- lapply(dt[, 10:43], FUN = function(x) factor(x, levels = c(0,1), labels = c("No", "Yes")))

## descriptive analysis
n = dim(dt)[1]
names(dt)
## sex
ggplot(data = dt, aes(sex)) + geom_bar() + ggtitle("bar chart of sex")
table(dt$sex)
table(dt$sex) / n
## male indicator
dt[, is.male := as.integer(sex == "male")]
## female indicator
dt[, is.female := as.integer(sex == "female")]

## postcode remove
# ggplot(data = dt, aes(PostCode)) + geom_bar() + ggtitle("bar chart of postcode")
# postcode is not used
summary(dt$PostCode)
#unique(dt$PostCode[nchar(dt_bk$PostCode) != 4])
#dt$PostCode[nchar(dt_bk$PostCode) != 4] <- NA
dt[, list(len = nchar(PostCode))][, .N, by = len][order(len)]
dt[, PostCode := NULL]

## age
ggplot(data = dt, aes(age)) + geom_histogram(binwidth = 5) + ggtitle("histogram of age")
## Age.group
quantile(dt$age)
## 0%  30%  70% 100%
## 11   35   41   9
## class by ABS
q <- c(0, 15, 64, 120)
dt[, age.group := cut(age, breaks = c(q[1],q[2],q[3],q[4]), labels = c("children","working","senior"), include.lowest = TRUE, ordered_result = TRUE)]
ggplot(dt, aes(age.group)) + geom_bar() + ggtitle("bar chart of age group")
## second try, combine children with young working
q <- c(0, 35, 64, 120)
dt[, age.group := cut(age, breaks = c(q[1],q[2],q[3],q[4]), labels = c("young.working","middle.working","senior"), include.lowest = TRUE, ordered_result = TRUE)]
ggplot(dt, aes(age.group)) + geom_bar() + ggtitle("bar chart of age group")

## indicator
dt[, c("young.working", "middle.working", "senior") := list(as.integer(age < q[2]), as.integer(age > q[3]), as.integer(age >=q[2] & age <=q[3]))]


## home own
ggplot(data = dt, aes(homeown)) + geom_bar() + ggtitle("bar chart of homeown")
table(dt$homeown)
table(dt$homeown) / n
## is.homeown indicator
dt[, is.home.own := as.integer(homeown == "Yes")]
dt[, .N, by = is.home.own]

## number of children
ggplot(data = dt, aes(nchildren)) + geom_bar() + ggtitle("bar chart of nchildren")
table(dt$nchildren)
table(dt$nchildren) / n
## num.kid
dt[, num.kid := cut(nchildren, breaks = c(0, 0.9, 2.1, 50), labels = c("none", "1-2", "3+"), include.lowest = TRUE, ordered_result = TRUE)]
dt[, mean(nchildren), by = num.kid]
unique(dt[, list(num.kid, nchildren)])[order(nchildren)]
summary(dt$num.kid)
ggplot(dt, aes(num.kid)) + geom_bar()

## indicator
dt[, no.kid := as.integer(nchildren == 0)]
dt[, one.kid := as.integer(nchildren == 1)]
dt[, more.than.two.kids := as.integer(nchildren == 2)]

dt[, .N, by = more.than.two.kids]
dt[, .N, by = no.kid]
dt[, .N, by = one.kid]

## payment method
ggplot(data = dt, aes(pmethod)) + geom_bar() + ggtitle("bar chart of pmethod")
table(dt$pmethod)
table(dt$pmethod) / n

## value
summary(dt$Value)
ggplot(data = dt, aes(Value)) + geom_histogram(binwidth = 20) + ggtitle("histogram of Value")
ggplot(data = dt, aes(Value)) + geom_histogram(binwidth = 50) + ggtitle("histogram of Value")
ggplot(data = dt, aes(factor(1), Value)) + geom_boxplot() + ggtitle("Value boxplot") + xlab(NULL) + ylab("Value")
## Value.level
## quantile(dt$Value)
## q <- quantile(dt$Value, probs = c(0, 0.3, 0.7, 1))
## dt[, value.level := cut(Value, breaks = c(q[1],q[2],q[3],q[4]), labels = c("low","medium","high"), include.lowest = TRUE, ordered_result = TRUE)]
## dt[, Value := NULL]
## transaction value does not represent customer level information?

## income
summary(dt$income)
ggplot(data = dt, aes(income)) + geom_histogram(binwidth = 5000) + ggtitle("histogram of income") + scale_x_continuous(label = comma)
ggplot(data = dt, aes(factor(1), income)) + geom_boxplot() + scale_y_continuous(label = comma) + xlab(NULL) + ylab("income") + ggtitle("income boxplot")
ggplot(data = dt[income < 150000], aes(income)) + geom_histogram(binwidth = 5000) + ggtitle("histogram of income without the outlier")
## income level
quantile(dt$income)
## 0 - 30% low, 30% - 70% medium, 70% - 100% high for cluster analysis
q <- quantile(dt$income, probs = c(0, 0.3, 0.7, 1))
dt[, income.level := cut(income, breaks = c(q[1],q[2],q[3],q[4]), labels = c("low","medium","high"), include.lowest = TRUE, ordered_result = TRUE)]
ggplot(dt, aes(income.level)) + geom_bar() + ggtitle("barchart of income.level")
## binary indicators for Arules
dt[, c("income.low", "income.high", "income.medium") := list(as.integer(income < q[2]), as.integer(income > q[3]), as.integer(income >=q[2] & income <=q[3]))]
dt[, .N, by = income.low]



## fruit, correction
ggplot(dt, aes(fruit)) + geom_bar()
unique(dt$fruit)
dt[, .N, by = fruit][order(-N)]
dt <- dt[fruit == '0' | fruit == '1']
dt[, fruit := as.integer(fruit)]

## fruit & banana conflicting info
## set fruit 1 when banana is 1
dt[banana == 1 & fruit != 1, .N]
## [1] 14337
dt[banana == 1 & fruit == 1, .N]
## [1] 29353
dt[banana == 1 & fruit != 1, fruit := 1]
dt[, .N, by = fruit]

# outliers
dt[income > 150000, income]
# create new categorical income variable to counter this

# ggplot(data = dt.cluster, aes(x = income.level, y = Value)) + geom_boxplot()

# ggplot(data = dt, aes(x = factor(1), y = income)) + geom_boxplot() + scale_y_continuous(label = comma)

# ggplot(data = dt, aes(income)) + geom_histogram() + scale_x_continuous(label = comma)

## payment method vs value
dt[, mean(Value), by = pmethod]
ggplot(dt, aes(pmethod, Value)) + geom_boxplot() + ggtitle("payment method vs amount")

## income vs value
dt[, mean(Value), by = income.level]
ggplot(dt, aes(income.level, Value)) + geom_boxplot() + ggtitle("income level vs amount")

## age vs value
dt[, mean(Value), by = age.group]
ggplot(dt, aes(age.group, Value)) + geom_boxplot() + ggtitle("age vs amount")

## nchild vs value
dt[, mean(Value), by = nchildren]
ggplot(dt, aes(ordered(nchildren), Value)) + geom_boxplot() + ggtitle("number of kids vs amount")
ggplot(dt, aes(ordered(num.kid), Value)) + geom_boxplot() + ggtitle("number of kids groups vs amount") + xlab("kids groups")

## count of distinct items in one order
tmp <- dt[,list(count = fruit+freshmeat+dairy+MozerallaCheese+cannedveg+cereal+frozenmeal+frozendessert+PizzaBase+TomatoSauce+frozen.fish+bread+milk+softdrink+fruitjuice+confectionery+fish+vegetables+icecream+energydrink+tea+coffee+laundryPowder+householCleaners+corn.chips+Frozen.yogurt+Chocolate+Olive.Oil+Baby.Food+Napies+banana+cat.food+dog.food+mince), Value]
tmp[, `:=`(order.num.per.count = .N, value.avg.per.count = mean(Value)), by = count]
tmp[, .N, by = count][order(-count)]

## 13 unique items per order
tmp[, list(mean(count), sd(count))]

## a normal shape distribution
ggplot(tmp[, .N, by = count][order(-count)], aes(count, N)) + geom_bar(stat = "identity") + ggtitle("bar chart of number of unique items per order")

## count vs value
tmp[, .(value.avg = mean(Value)), by = count][order(count)]
ggplot(tmp, aes(factor(count), value.avg.per.count)) + geom_bar(stat = "identity") + xlab("unique item per order") + ylab("average amount per order($)") + ggtitle("avg amount vs unique items per order")
## no obvious increase with increase of unique items per order
## without quantity of items, it's hard to explain if this is due to the quantity

## scatter plot (bubble plot) transaction value against number of items
ggplot(tmp, aes(factor(count), value.avg.per.count)) + geom_point(aes(size = order.num.per.count)) + xlab("unique items per order") + ylab("average amount per order($)") + ggtitle("avg amount vs unique items per order") + scale_size_continuous(range = c(1, 10))

## to answer question like most common item in all orders does not considering quantity
names(dt)
tmp <- dt[, lapply(.SD, sum), .SD = c(9:42)]
## energy drink is the least frequent item
support.energy.drink <- tmp$energydrink / dim(dt)[1]

ggplot(melt(tmp), aes(x = reorder(factor(variable), -value), y = value)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("item frequency") + xlab("line item") + ylab("transaction count")

## create copy for cluster analysis
names(dt)
dt.cluster <- dt[, list(pmethod, sex, homeown, num.kid, income.level, age.group)]

## Cluster analysis
## hierarchy cluster is used because the mixed categorical and continuous variables nature of the dataset

## dedup and distance
summary(dt.cluster)
dt.cluster.dedup <- unique(dt.cluster)
dim(dt.cluster.dedup)
str(dt.cluster.dedup)

## with mixed variables metric "gower" is used
d <- daisy(dt.cluster.dedup, metric = "gower")

hc.single <- hclust(d = d, method = "single")
hc.complete <- hclust(d = d, method = "complete")
hc.average <- hclust(d = d, method = "average")

plot(hc.single, labels = FALSE, hang = -1)
plot(hc.complete, labels = FALSE, hang = -1)
plot(hc.average, labels = FALSE, hang = -1)

memb.single <- cutree(hc.single, k = c(2:10))
memb.complete <- cutree(hc.complete, k = c(2:6))
memb.average <- cutree(hc.average, k = c(2:7))

head(memb.complete)

dt.cluster.dedup[, `:=`(group2 = memb.single[,1], group3 = memb.single[,2], group4 = memb.single[,3], group5 = memb.single[,4], group10 = memb.single[,9])]

dt.cluster.dedup[, `:=`(group2 = memb.complete[,1], group3 = memb.complete[,2], group4 = memb.complete[,3], group5 = memb.complete[,4], group6 = memb.complete[,5])]

dt.cluster.dedup[, `:=`(group2 = memb.average[,1], group3 = memb.average[,2], group4 = memb.average[,3], group5 = memb.average[,4], group6 = memb.average[,5], group7 = memb.average[,6])]

dim(dt.cluster)
dt.cluster.group <- dt.cluster[dt.cluster.dedup]

names(dt.cluster.dedup)
dt.cluster.dedup[, lapply(.SD, mean), by = group2, .SD = c(1:6)]
dt.cluster.dedup[, lapply(.SD, mean), by = group3, .SD = c(1:6)]
dt.cluster.dedup[, lapply(.SD, mean), by = group4, .SD = c(1:6)]
dt.cluster.dedup[, lapply(.SD, mean), by = group5, .SD = c(1:6)]
dt.cluster.dedup[, lapply(.SD, mean), by = group6, .SD = c(1:6)]
dt.cluster.dedup[, lapply(.SD, mean), by = group7, .SD = c(1:6)]

dt.cluster.dedup[, .N, by = group6]
dt.cluster.dedup[, .N, by = group7]

## graphics
names(dt.cluster.dedup)
ggplot(dt.cluster.dedup, aes(income.level)) + geom_bar(aes(fill = ordered(group10)), alpha = 0.5)
ggplot(dt.cluster.dedup, aes(sex)) + geom_bar(aes(fill = ordered(group10)), alpha = 0.5)
ggplot(dt.cluster.dedup, aes(pmethod)) + geom_bar(aes(fill = ordered(group10)), alpha = 0.5)
ggplot(dt.cluster.dedup, aes(homeown)) + geom_bar(aes(fill = ordered(group10)), alpha = 0.5)
ggplot(dt.cluster.dedup, aes(homeown)) + geom_bar(aes(fill = ordered(group10)), alpha = 0.5)
ggplot(dt.cluster.dedup, aes(nchildren)) + geom_bar(aes(fill = ordered(group10)), alpha = 0.5)

ggplot(dt.cluster.dedup, aes(ordered(group5))) + geom_bar(aes(fill = pmethod))
ggplot(dt.cluster.dedup, aes(ordered(group5))) + geom_bar(aes(fill = income.level))
ggplot(dt.cluster.dedup, aes(ordered(group5))) + geom_bar(aes(fill = sex))

## Examine clusters to determine characteristics unique to that cluster.
## Examine fields across clusters to determine how values are distributed among clusters.

## Markest basket analysis
## Apriori

## items only


dt.items.only <-dt[, list(fruit, freshmeat, dairy, MozerallaCheese, cannedveg, cereal, frozenmeal, frozendessert, PizzaBase, TomatoSauce, frozen.fish, bread, milk, softdrink, fruitjuice, confectionery, fish, vegetables, icecream, energydrink, tea, coffee, laundryPowder, householCleaners, corn.chips, Frozen.yogurt, Chocolate, Olive.Oil, Baby.Food, Napies, banana, cat.food, dog.food, mince)]

items.only <- data.matrix(dt.items.only)
trans <- as(items.only, "transactions")
summary(trans)
inspect(trans[1:10])

support.energy.drink

rules.items.only <- apriori(items.only, parameter = list(minlen = 2, supp = 0.14, conf = 0.8, maxlen = 4))
summary(rules.items.only)
rules.sorted <- sort(rules.items.only, by = "lift")
rules.sorted <- sort(rules.items.only, by = "confidence")
inspect(rules.sorted[1:50])

## items & demographics
## applicable when customer is identified i.e. online shopping, direct marketing
dt.item.demo <- dt[, .(fruit, freshmeat, dairy, MozerallaCheese, cannedveg, cereal, frozenmeal, frozendessert, PizzaBase, TomatoSauce, frozen.fish, bread, milk, softdrink, fruitjuice, confectionery, fish, vegetables, icecream, energydrink, tea, coffee, laundryPowder, householCleaners, corn.chips, Frozen.yogurt, Chocolate, Olive.Oil, Baby.Food, Napies, banana, cat.food, dog.food, mince, is.male, is.female, young.working, middle.working, senior, is.home.own, no.kid, one.kid, more.than.two.kids, income.low, income.high, income.medium)]

item.demo <- data.matrix(dt.item.demo)
trans <- as(item.demo, "transactions")
summary(trans)
inspect(trans[1:10])

support.energy.drink

rules.item.demo <- apriori(item.demo, parameter = list(minlen = 2, supp = 0.14, conf = 0.8, maxlen = 4))
summary(rules.item.demo)
rules.sorted <- sort(rules.item.demo, by = "lift")
rules.sorted <- sort(rules.item.demo, by = "confidence")
inspect(rules.sorted[1:10])

names(dt)

rules.sub <- subset(rules.item.demo, subset = rhs %in% "banana")
summary(rules.sub)
rules.sorted <- sort(rules.sub, by = "lift")
inspect(rules.sorted[1:10])

rules.sub <- subset(rules.item.demo, subset = rhs %in% "mince")
summary(rules.sub)
rules.sorted <- sort(rules.sub, by = "lift")
inspect(rules.sorted[1:10])

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

plot(rules.sorted)

summary(rules.sorted)
