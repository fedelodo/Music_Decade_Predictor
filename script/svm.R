library("e1071")
library("caret")

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

# SVM Model e1071, RADIAL
model.radial <- svm(formula,
                     data = train,
                     kernel='radial',
                     method='C-classification',
                     cost=10,
                     probability =TRUE)
summary(model.radial)
pred_svm.radial <- predict(model.radial, test)
cm_svm.radial <- confusionMatrix(test$label, pred_svm.radial)
confusion.matrix.svm.radial <- table(test$label, pred_svm.radial)
acc_svm.radial <- sum(diag(confusion.matrix.svm.radial)) / sum(confusion.matrix.svm.radial)

# SVM Model e1071, LINEAR
model.linear <- svm(formula,
                    data = train,
                    kernel='linear',
                    method='C-classification',
                    cost=1,
                    probability =TRUE)
summary(model.linear)
pred_svm.linear <- predict(model.linear, test)
cm_svm.linear <- confusionMatrix(test$label, pred_svm.linear)
confusion.matrix.svm.linear <- table(test$label, pred_svm.linear)
acc_svm.linear <- sum(diag(confusion.matrix.svm.linear)) / sum(confusion.matrix.svm.linear)

# SVM Model Caret, RADIAL
#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = TRUE)
set.seed(3233)
model <- train(formula, 
                    data = train, 
                    method = "svmRadial",
                    #trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    metric = "Accuracy")
pred <- predict(model, test)
cm <- table(pred, test$label)
acc <- sum(diag(cm)) / sum(cm)
