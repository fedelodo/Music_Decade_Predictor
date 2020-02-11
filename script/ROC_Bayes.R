library("pROC")
library("e1071")

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

# Naive Bayes

## METODO 1
model <- naiveBayes(train, train$label)
pred <- as.numeric(predict(model, test))
roc.multi_test <- multiclass.roc(test$label, pred)
rs_test <- roc.multi_test[['rocs']]
#sapply(2:length(rs_test), function(i) lines.roc(rs_test[[i]],col=i))
roc.list <- list("1950-1960"=rs_test[[1]],"1950-1970"=rs_test[[2]],
                 "1950-1980"=rs_test[[3]],"1950-1990"=rs_test[[4]],
                 "1950-2000"=rs_test[[5]],"1960-1970"=rs_test[[6]],
                 "1960-1980"=rs_test[[7]],"1960-1990"=rs_test[[8]],
                 "1960-2000"=rs_test[[9]],"1970-1980"=rs_test[[10]],
                 "1970-1990"=rs_test[[11]],"1970-2000"=rs_test[[12]],
                 "1980-1990"=rs_test[[13]],"1980-2000"=rs_test[[14]],
                 "1990-2000"=rs_test[[15]])
ggroc(roc.list, legacy.axes = TRUE) + geom_abline() + ggtitle("Naive Bayes ROC curve") + geom_line(size=2.5)

# ## METODO 2
# train$is1950 <- as.factor(train$label == '1950')
# train$is1960 <- as.factor(train$label == '1960')
# train$is1970 <- as.factor(train$label == '1970')
# train$is1980 <- as.factor(train$label == '1980')
# train$is1990 <- as.factor(train$label == '1990')
# train$is2000 <- as.factor(train$label == '2000')
# 
# test$is1950 <- as.factor(test$label == '1950')
# test$is1960 <- as.factor(test$label == '1960')
# test$is1970 <- as.factor(test$label == '1970')
# test$is1980 <- as.factor(test$label == '1980')
# test$is1990 <- as.factor(test$label == '1990')
# test$is2000 <- as.factor(test$label == '2000')
# 
# # 1950
# formula <- as.formula(paste("is1950", paste(rn, collapse=" + "), sep=" ~ "))
# model.1950 <- train(formula, train, "nb", trControl = trainControl(method = "cv", number = 10))
# predict_1950 <- as.numeric(predict(model.1950, test))
# roc_1950 <- roc(test$is1950, predict_1950)
# plot(ggroc(roc_1950, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw() + title(main = "ROC 1950"))
# 
# # 1960
# formula <- as.formula(paste("is1960", paste(rn, collapse=" + "), sep=" ~ "))
# model.1960 <- train(formula, train, "nb", trControl = trainControl(method = "cv", number = 10))
# predict_1960 <- as.numeric(predict(model.1960, test))
# roc_1960 <- roc(test$is1960, predict_1960)
# plot(ggroc(roc_1960, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw() + title(main = "ROC 1960"))
# 
# # 1970 
# formula <- as.formula(paste("is1970", paste(rn, collapse=" + "), sep=" ~ "))
# model.1970 <- train(formula, train, "nb", trControl = trainControl(method = "cv", number = 10))
# predict_1970 <- as.numeric(predict(model.1970, test))
# roc_1970 <- roc(test$is1970, predict_1970)
# plot(ggroc(roc_1970, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw() + title(main = "ROC 1970"))
# 
# # 1980 
# formula <- as.formula(paste("is1980", paste(rn, collapse=" + "), sep=" ~ "))
# model.1980 <- train(formula, train, "nb", trControl = trainControl(method = "cv", number = 10))
# predict_1980 <- as.numeric(predict(model.1980, test))
# roc_1980 <- roc(test$is1980, predict_1980)
# plot(ggroc(roc_1980, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw() + title(main = "ROC 1980"))
# 
# # 1990
# formula <- as.formula(paste("is1990", paste(rn, collapse=" + "), sep=" ~ "))
# model.1990 <- train(formula, train, "nb", trControl = trainControl(method = "cv", number = 10))
# predict_1990 <- as.numeric(predict(model.1990, test))
# roc_1990 <- roc(test$is1990, predict_1990)
# plot(ggroc(roc_1990, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw() + title(main = "ROC 1990"))
# 
# # 2000
# formula <- as.formula(paste("is2000", paste(rn, collapse=" + "), sep=" ~ "))
# model.2000 <- train(formula, train, "nb", trControl = trainControl(method = "cv", number = 10))
# predict_2000 <- as.numeric(predict(model.2000, test))
# roc_2000 <- roc(test$is2000, predict_2000)
# plot(ggroc(roc_2000, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw() + title(main = "ROC 2000"))
