library("e1071")
library("caret")
library("ggplot2")
library("dplyr")

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
model.bayes.e1071 = naiveBayes(train, train$label)
pred.bayes.e1071 = predict(model.bayes.e1071, test)
cm_bayes.e1071 <- confusionMatrix(test$label, pred.bayes.e1071)
confusion_matrix.bayes.e1071 <- table(test$label, pred.bayes.e1071)
# accuracy, precision, recall, f-measure
acc_bayes.e1071 <- sum(diag(confusion_matrix.bayes.e1071))/sum(confusion_matrix.bayes.e1071)
prec_bayes.e1071 <- diag(confusion_matrix.bayes.e1071) / rowSums(confusion_matrix.bayes.e1071)
rec_bayes.e1071 <- diag(confusion_matrix.bayes.e1071) / rowSums(confusion_matrix.bayes.e1071)
f1_bayes.e1071 <- 2 * (prec_bayes.e1071 * rec_bayes.e1071) / (prec_bayes.e1071 + rec_bayes.e1071)

# CARET
model.bayes.caret = train(train, train$label, "nb", trControl = trainControl(method = "cv", number = 10))
pred.bayes.caret = predict(model.bayes.caret, test)
cm_bayes.caret <- confusionMatrix(test$label, pred.bayes.caret)
confusion_matrix.bayes.caret <- table(test$label, pred.bayes.caret)
# accuracy, precision, recall, f-measure
acc_bayes.caret <- sum(diag(confusion_matrix.bayes.caret))/sum(confusion_matrix.bayes.caret)
prec_bayes.caret <- diag(confusion_matrix.bayes.caret) / rowSums(confusion_matrix.bayes.caret)
rec_bayes.caret <- diag(confusion_matrix.bayes.caret) / colSums(confusion_matrix.bayes.caret)
f1_bayes.caret <- 2 * (prec_bayes.caret * rec_bayes.caret) / (prec_bayes.caret + rec_bayes.caret)

##plot confusion matrix
# plot caret confusion matrix
table.bayes.caret <- data.frame(cm_bayes.caret$table)

plotTable.bayes.caret <- table.bayes.caret %>%
  mutate(goodbad = ifelse(table.bayes.caret$Prediction == table.bayes.caret$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable.bayes.caret, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.bayes.caret$Reference))) +
  ggtitle("Confusion Matrix: NAIVE BAYES with Caret (on Normalize)")

# plot e1071 confusion matrix
table.bayes.e1071 <- data.frame(cm_bayes.e1071$table)

plotTable.bayes.e1071 <- table.bayes.e1071 %>%
  mutate(goodbad = ifelse(table.bayes.e1071$Prediction == table.bayes.e1071$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable.bayes.e1071, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.bayes.e1071$Reference))) +
  ggtitle("Confusion Matrix: NAIVE BAYES with e1071 (on Normalize)")
