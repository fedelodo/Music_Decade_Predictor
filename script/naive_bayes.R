library("e1071")

train = read.csv("train.csv", header=TRUE)
train$label <- as.factor(train$label)
test = read.csv("test.csv", header=TRUE)
test$label <- as.factor(test$label)
# carico la formula 
fileName <- 'formula.txt'
formula <- readChar(fileName, file.info(fileName)$size)
formula <- gsub("\n", "", formula)
formula <- as.formula(formula)

# e1071
classifier = naiveBayes(train, train$label)
test.predicted.bayes = predict(classifier, test)
cm_bayes <- confusionMatrix(test$label, test.predicted.bayes)
confusion_matrix.bayes <- table(test$label, test.predicted.bayes)
acc_bayes <- sum(diag(confusion_matrix.bayes))/sum(confusion_matrix.bayes)

# CARET
classifier.bayes.caret = train(train, train$label, "nb", trControl = trainControl(method = "cv", number = 10))
test.predicted.bayes.caret = predict(classifier.bayes.caret, test)
cm_bayes.caret <- confusionMatrix(test$label, test.predicted.bayes.caret)
confusion_matrix.bayes.caret <- table(test$label, test.predicted.bayes.caret)
acc_bayes.caret <- sum(diag(confusion_matrix.bayes.caret))/sum(confusion_matrix.bayes.caret)
