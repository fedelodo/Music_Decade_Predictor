library("e1071")
library("caret")
library("ggplot2")
library("dplyr")
library("doMC")
library("pROC")

# train = read.csv("train.csv", header=TRUE)
# train$label <- as.factor(train$label)
# test = read.csv("test.csv", header=TRUE)
# test$label <- as.factor(test$label)

train = read.csv("train_pca.csv", header=TRUE)
train$label <- as.factor(train$label)
test = read.csv("test_pca.csv", header=TRUE)
test$label <- as.factor(test$label)

# carico la formula 
fileName <- 'formula.txt'
formula <- readChar(fileName, file.info(fileName)$size)
formula <- gsub("\n", "", formula)
formula <- as.formula(formula)

registerDoMC()
# e1071
model.bayes.e1071 = naiveBayes(train, train$label)
pred.bayes.e1071 = predict(model.bayes.e1071, test)
cm_bayes.e1071 <- confusionMatrix(test$label, pred.bayes.e1071)
confusion_matrix.bayes.e1071 <- table(test$label, pred.bayes.e1071)
# accuracy, precision, recall, f-measure
acc_bayes.e1071 <- sum(diag(confusion_matrix.bayes.e1071))/sum(confusion_matrix.bayes.e1071)
prec_bayes.e1071 <- diag(confusion_matrix.bayes.e1071) / rowSums(confusion_matrix.bayes.e1071)
rec_bayes.e1071 <- diag(confusion_matrix.bayes.e1071) / colSums(confusion_matrix.bayes.e1071)
f1_bayes.e1071 <- 2 * (prec_bayes.e1071 * rec_bayes.e1071) / (prec_bayes.e1071 + rec_bayes.e1071)

registerDoMC()
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
  ggtitle("Confusion Matrix: NAIVE BAYES with Caret")

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
  ggtitle("Confusion Matrix: NAIVE BAYES with e1071")


## ROC/ AUC Plots
#naive bayes e1071
pred.roc.bayes.e1071 <- as.numeric(pred.bayes.e1071)
roc.multi_test.bayes.e1071 <- multiclass.roc(test$label, pred.roc.bayes.e1071)
rs_test.bayes.e1071 <- roc.multi_test.bayes.e1071[['rocs']]
roc.list.bayes.e1071 <- list("1950-1960"=rs_test.bayes.e1071[[1]],"1950-1970"=rs_test.bayes.e1071[[2]],
                                    "1950-1980"=rs_test.bayes.e1071[[3]],"1950-1990"=rs_test.bayes.e1071[[4]],
                                    "1950-2000"=rs_test.bayes.e1071[[5]],"1960-1970"=rs_test.bayes.e1071[[6]],
                                    "1960-1980"=rs_test.bayes.e1071[[7]],"1960-1990"=rs_test.bayes.e1071[[8]],
                                    "1960-2000"=rs_test.bayes.e1071[[9]],"1970-1980"=rs_test.bayes.e1071[[10]],
                                    "1970-1990"=rs_test.bayes.e1071[[11]],"1970-2000"=rs_test.bayes.e1071[[12]],
                                    "1980-1990"=rs_test.bayes.e1071[[13]],"1980-2000"=rs_test.bayes.e1071[[14]],
                                    "1990-2000"=rs_test.bayes.e1071[[15]])
ggroc(roc.list.bayes.e1071, legacy.axes = TRUE) + geom_abline() + ggtitle("NAIVE BAYES ROC curve e1071") + geom_line(size=1)

#naive bayes caret
pred.roc.bayes.caret <- as.numeric(pred.bayes.caret)
roc.multi_test.bayes.caret <- multiclass.roc(test$label, pred.roc.bayes.caret)
rs_test.bayes.caret <- roc.multi_test.bayes.caret[['rocs']]
roc.list.bayes.caret <- list("1950-1960"=rs_test.bayes.caret[[1]],"1950-1970"=rs_test.bayes.caret[[2]],
                                    "1950-1980"=rs_test.bayes.caret[[3]],"1950-1990"=rs_test.bayes.caret[[4]],
                                    "1950-2000"=rs_test.bayes.caret[[5]],"1960-1970"=rs_test.bayes.caret[[6]],
                                    "1960-1980"=rs_test.bayes.caret[[7]],"1960-1990"=rs_test.bayes.caret[[8]],
                                    "1960-2000"=rs_test.bayes.caret[[9]],"1970-1980"=rs_test.bayes.caret[[10]],
                                    "1970-1990"=rs_test.bayes.caret[[11]],"1970-2000"=rs_test.bayes.caret[[12]],
                                    "1980-1990"=rs_test.bayes.caret[[13]],"1980-2000"=rs_test.bayes.caret[[14]],
                                    "1990-2000"=rs_test.bayes.caret[[15]])
ggroc(roc.list.bayes.caret, legacy.axes = TRUE) + geom_abline() + ggtitle("NAIVE BAYES ROC curve caret") + geom_line(size=1)
