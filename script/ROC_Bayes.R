library("pROC")
library("caret")
library("ggplot2")

# carico i dataset
train = read.csv("train.csv", header=TRUE)
train$label <- as.factor(train$label)
test = read.csv("test.csv", header=TRUE)
test$label <- as.factor(test$label)
# carico la formula 
fileName <- 'formula.txt'
formula <- readChar(fileName, file.info(fileName)$size)
formula <- gsub("\n", "", formula)
formula <- as.formula(formula)

# NAIVE BAYES CARET
model <- train(train, train$label, "nb", trControl = trainControl(method = "cv", number = 10))
pred <- as.numeric(predict(model, test))
roc.multi_test <- multiclass.roc(test$label, pred)
rs_test <- roc.multi_test[['rocs']]
roc.list <- list("1950-1960"=rs_test[[1]],"1950-1970"=rs_test[[2]],
                 "1950-1980"=rs_test[[3]],"1950-1990"=rs_test[[4]],
                 "1950-2000"=rs_test[[5]],"1960-1970"=rs_test[[6]],
                 "1960-1980"=rs_test[[7]],"1960-1990"=rs_test[[8]],
                 "1960-2000"=rs_test[[9]],"1970-1980"=rs_test[[10]],
                 "1970-1990"=rs_test[[11]],"1970-2000"=rs_test[[12]],
                 "1980-1990"=rs_test[[13]],"1980-2000"=rs_test[[14]],
                 "1990-2000"=rs_test[[15]])
ggroc(roc.list, legacy.axes = TRUE) + geom_abline() + ggtitle("Naive Bayes ROC curve") + geom_line(size=2.5)
