library(ggplot2)
library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(scales)
library(reshape2)
library(ROCR)
library(randomForest)
library(caret)
library(doMC)

registerDoMC(cores = 4)

## dt <- read.csv("./data/covtype.with.header.data")
dt <- fread("./data/covtype.with.header.data")
str(dt)

dim(dt)
names(dt)

## Explore
##ggplot(dt, aes(Elevation)) + geom_histogram(fill = 'blue', colour = 'black') + ggtitle("histogram of Elevation")

## continuous variables
## histograms
name_list <- names(dt)[1:10]

histo_plot <- function(name, dt) {
    p <- ggplot(dt, aes_string(name)) + geom_histogram(fill = 'blue', colour = 'black') + ggtitle(paste0("histogram of ",name))
    ggsave(filename = paste0(name, ".png"), plot = p)
}

rs <- lapply(name_list, FUN = histo_plot, dt)


## categorical variables

## factorize
dt[, c(11:55):=lapply(.SD, as.factor), .SD = c(11:55), with = FALSE]
str(dt)

## barchart
##ggplot(dt, aes(Wilderness_Area1)) + geom_bar(fill = 'blue', colour = 'black') + scale_y_continuous(label = comma)
name_list <- names(dt)[11:55]

histo_plot <- function(name, dt) {
    p <- ggplot(dt, aes_string(name)) + geom_bar(fill = 'blue', colour = 'black') + ggtitle(paste0("barchart of ",name)) + scale_y_continuous(label = comma)
    ggsave(filename = paste0(name, ".png"), plot = p)
}

rs <- lapply(name_list, FUN = histo_plot, dt)

## New variables

## name1 <- "Wilderness_Area1"
## dt[get(name1) == 1]

## function factory for new variables
f <- function(new_v) {
    function(name, dt) {
        dt[get(name) == 1, (new_v) := name, with = FALSE]
    }
}

## Wilderness_Area
name_list <- names(dt)[11:14]
rs <- lapply(name_list, FUN = f("Wilderness_Area"), dt)
##dt$Wilderness_Area <- factor(dt$Wilderness_Area, levels = c("Wilderness_Area1", "Wilderness_Area2", "Wilderness_Area3", "Wilderness_Area4"))
dt[, Wilderness_Area := factor(Wilderness_Area, levels = c("Wilderness_Area1", "Wilderness_Area2", "Wilderness_Area3", "Wilderness_Area4"))]

p <- ggplot(dt, aes(Wilderness_Area)) + geom_bar(fill = 'blue', colour = 'black') + ggtitle("barchart of Wilderness_Area") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "./Wilderness_Area.png", plot = p)

## Soil Type
name_list <- names(dt)[15:54]
rs <- lapply(name_list, FUN = f("Soil_Type"), dt)

dt[, Soil_Type := factor(Soil_Type, levels = c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8","Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16","Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24","Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32","Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40"))]

p <- ggplot(dt, aes(Soil_Type)) + geom_bar(fill = 'blue', colour = 'black') + ggtitle("barchart of Soil_Type") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "./Soil_Type.png", plot = p)

## Relationship with the response
names(dt)
##p <- ggplot(dt, aes(Cover_Type, Elevation)) + geom_boxplot(colour = 'black', fill = 'blue') + ggtitle("boxplot of Elevation on different Cover Type")

## continuous variables
save.image("current.RData")
load("./current.RData")

name_list <- names(dt)[1:10]
box_plot <- function(name, dt) {
    p <- ggplot(dt, aes_string("Cover_Type", name)) + geom_boxplot(fill = 'blue', colour = 'black') + ggtitle(paste0("Boxplot of ", name, " by different Cover Type"))
    ggsave(filename = paste0("./",name,"_CoverType.png"), plot = p)
}
rs <- lapply(name_list, FUN = box_plot, dt)

## categorical variables
png(filename = "./Mosaic_CoverType_Wilderness.png")
mosaicplot(~Wilderness_Area + Cover_Type, data = dt, color = TRUE)
dev.off()
png(filename = "./Mosaic_CoverType_Soil_Type.png")
mosaicplot(~Soil_Type + Cover_Type, data = dt, color = TRUE)
dev.off()

## Cover Type 2 and 1 are most frequent, use them only
dt[Cover_Type %in% c(1,2)][,.N, by = Cover_Type][order(-N)]

dt <- dt[Cover_Type %in% c(1,2)]

dt[, Cover_Type := factor(Cover_Type, levels = c(1,2), labels = c("Cover_1", "Cover_2"))]

## Split to Train/Evaluation/Test

## Remove depreciated variables
names(dt)
dt[, c(11:54) := NULL]

set.seed(88)

splitRatio <- 5000 / dim(dt)[1]
dt[, split := sample.split(Cover_Type, SplitRatio = splitRatio)]
train <- dt[split == TRUE]
##rest <- dt[split == FALSE]
##rest[, split := sample.split(Cover_Type, SplitRatio = 0.5)]
##eval <- rest[split == TRUE]
##test <- rest[split == FALSE]
test <- dt[split == FALSE]


train[, split := NULL]
##eval[, split := NULL]
test[, split := NULL]


## Build models
## CART
str(train)

fit.rpart <- rpart(Cover_Type ~ ., data = train)
fit.rpart
summary(fit.rpart)
prp(fit.rpart)

eval.rpart <- predict(fit.rpart, type = "prob", newdata = test)
##eval.rpart <- predict(fit.rpart, type = "class", newdata = evaluation)
##table(evaluation$Cover_Type, eval.rpart)

## ROC/AUC
auc <- function(predict, target, plotname) {
    eval.rocr <- prediction(predict[, 2], target$Cover_Type)
    roc <- performance(eval.rocr, "tpr", "fpr")
    png(filename = plotname)
    plot(roc, colorize = TRUE)
    dev.off()
    performance(eval.rocr, "auc")@y.values
}

auc(eval.rpart, test, "./ROC_RPART.png")
## 0.7692566

## Random Forest
fit.rf <- randomForest(Cover_Type ~ ., data = train, importance = TRUE, ntree = 500)

varImpPlot(fit.rf)

eval.rf <- predict(fit.rf, type = "prob", newdata = test)

auc(eval.rf, test, "./ROC_RandomForest.png")
## 0.8847413


## train model using CARET
str(dt)

## split to train & test
splitRatio <- 5000 / dim(dt)[1]
inTraining <- createDataPartition(dt$Cover_Type, p = splitRatio, list = FALSE)
train <- as.data.frame(dt[inTraining[, 1]])
test <- as.data.frame(dt[-inTraining[, 1]])

## training control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)
grid <- expand.grid(.cp = seq(0.001, 0.005, length.out = 20))

rpart.fit <- train(Cover_Type ~.
                 , data = train
                 , method = 'rpart'
                 , trControl = ctrl
                 , tuneGrid = grid
                 , metric = 'ROC')

rpart.fit <- train(Cover_Type ~.
                 , data = train
                 , method = 'rpart'
                 , trControl = ctrl
                 , tuneLength = 30
                 , metric = 'ROC')


rpart.fit



training <- as.data.frame(training)
rf.fit <- train(Cover_Type ~., data = training, method = 'rf', trControl = ctrl, metric = 'ROC')
