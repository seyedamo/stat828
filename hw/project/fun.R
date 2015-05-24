library(caret)
library(data.table)



whole <- fread("./data/train-2.csv")


whole <- whole[Cover_Type %in% c(1,2)]
whole[Cover_Type == 2, Cover_Type := 0]


whole[, c(12:15) := lapply(.SD, as.factor), .SD = c(12:15)]

whole[, c(16:56) := lapply(.SD, FUN = function(x) factor(x, levels = c(0,1), labels = c("No", "Yes"))), .SD = c(16:56)]

whole[, .N, by = Cover_Type]

training <- data.frame(whole)
training$Id <- NULL


class(training)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = TRUE, allowParallel = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)


dim(training)
grid <- expand.grid(.mtry = c(2, 23, 30, 56))

ctrl <- trainControl(method = "none", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)

set.seed(43)

fit <- train(Cover_Type ~.
                 , data = training
                 , method = 'rf'
                 , ntrees = 100
                 , trControl = ctrl
                 , metric = 'ROC'
                 , importance = TRUE
             )



fit

varImp(fit)


##             , tuneGrid = grid

