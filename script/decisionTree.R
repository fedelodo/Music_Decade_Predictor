library("rpart")
library("rpart.plot")
library("rattle")
library("RColorBrewer")
library("caret")
library("ggplot2")
library("dplyr")

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
model.decisiontree.rpart <- rpart(formula,
                     data=train, method="class")
fancyRpartPlot(model.decisiontree.rpart)
pred.decisiontree.rpart <- predict(model.decisiontree.rpart, test, type="class")
cm_decisiontree.rpart <- confusionMatrix(test$label, pred.decisiontree.rpart)
confusion_matrix.decisiontree.rpart = table(test$label, pred.decisiontree.rpart)
# accuracy, precision, recall, f-measure
acc_decisiontree.rpart <- sum(diag(confusion_matrix.decisiontree.rpart))/sum(confusion_matrix.decisiontree.rpart)
prec_decisiontree.rpart <- diag(confusion_matrix.decisiontree.rpart) / rowSums(confusion_matrix.decisiontree.rpart)
rec_decisiontree.rpart <- diag(confusion_matrix.decisiontree.rpart) / colSums(confusion_matrix.decisiontree.rpart)
f1_decisiontree.rpart <- 2 * (prec_decisiontree.rpart * rec_decisiontree.rpart) / (prec_decisiontree.rpart + rec_decisiontree.rpart)

# Decision Tree with caret
traincontroll.decisiontree.caret <- trainControl(method = "cv", number = 10)
model.decisiontree.caret <- train( formula, data = train, method = "ctree", trControl = traincontroll.decisiontree.caret, tuneLength = 10)
pred.decisiontree.caret <- predict(model.decisiontree.caret, test)
cm_decisiontree.caret <- confusionMatrix(test$label, pred.decisiontree.caret)
confusion_matrix.decisiontree.caret = table(test$label, pred.decisiontree.caret)
# accuracy, precision, recall, f-measure
acc_decisiontree.caret <- sum(diag(confusion_matrix.decisiontree.caret))/sum(confusion_matrix.decisiontree.caret)
prec_decisiontree.caret <- diag(confusion_matrix.decisiontree.caret) / rowSums(confusion_matrix.decisiontree.caret)
rec_decisiontree.caret <- diag(confusion_matrix.decisiontree.caret) / colSums(confusion_matrix.decisiontree.caret)
f1_decisiontree.caret <- 2 * (prec_decisiontree.caret * rec_decisiontree.caret) / (prec_decisiontree.caret + rec_decisiontree.caret)

## plot confusion matrix
# plot rpart confusion matrix
table.decisiontree.rpart <- data.frame(cm_decisiontree.rpart$table)

plotTable.decisiontree.rpart <- table.decisiontree.rpart %>%
  mutate(goodbad = ifelse(table.decisiontree.rpart$Prediction == table.decisiontree.rpart$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable.decisiontree.rpart, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.decisiontree.rpart$Reference))) +
  ggtitle("Confusion Matrix: DECISION TREE with rpart (on Normalize)")

# plot caret confusion matrix
table.decisiontree.caret <- data.frame(cm_decisiontree.caret$table)

plotTable.decisiontree.caret <- table.decisiontree.caret %>%
  mutate(goodbad = ifelse(table.decisiontree.caret$Prediction == table.decisiontree.caret$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable.decisiontree.caret, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.decisiontree.caret$Reference))) +
  ggtitle("Confusion Matrix: DECISION TREE with Caret (on Normalize)")