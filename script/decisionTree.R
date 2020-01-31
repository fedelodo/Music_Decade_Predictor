library("rpart")
library("rpart.plot")
library("rattle")
library("RColorBrewer")
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

# Decision Tree with rpart
decisionTree <- rpart(formula,
                     data=train, method="class")
fancyRpartPlot(decisionTree)
pred_tree <- predict(decisionTree, test, type="class")
cm_tree <- confusionMatrix(test$label, pred_tree)
confusion_matrix.tree = table(test$label, pred_tree)
acc_tree <- sum(diag(confusion_matrix.tree))/sum(confusion_matrix.tree)

# Decision Tree with caret
fitControl <- trainControl(method = "cv", number = 10)
dtree <- train( formula, data = train, method = "ctree", trControl = fitControl, tuneLength = 10)
pred_tree.caret <- predict(dtree, test)
cm_tree.caret <- confusionMatrix(test$label, pred_tree.caret)
confusion_matrix.tree.caret = table(test$label, pred_tree.caret)
acc_tree.caret <- sum(diag(confusion_matrix.tree.caret))/sum(confusion_matrix.tree.caret)
