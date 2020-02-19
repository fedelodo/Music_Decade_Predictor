library("e1071")
library("caret")
library("ggplot2")
library("dplyr")
library("doMC")
library("doMC")
library("pROC")

# # carico i dataset
# train = read.csv("train.csv", header=TRUE)
# train$label <- as.factor(train$label)
# test = read.csv("test.csv", header=TRUE)
# test$label <- as.factor(test$label)

# carico i dataset PCA
train = read.csv("train_pca.csv", header=TRUE)
train$label <- as.factor(train$label)
test = read.csv("test_pca.csv", header=TRUE)
test$label <- as.factor(test$label)

# SVM e1071 radial
registerDoMC()
model.svm.e1071.radial <- svm(label ~.,
                              data = train,
                              kernel='radial',
                              method='C-classification',
                              cost=10,
                              probability =TRUE)
summary(model.svm.e1071.radial)
pred.svm.e1071.radial <- predict(model.svm.e1071.radial, test)
cm_svm.e1071.radial <- confusionMatrix(test$label, pred.svm.e1071.radial)
confusion_matrix.svm.e1071.radial <- table(test$label, pred.svm.e1071.radial)
# accuracy, precision, recall, f-measure
acc_svm.e1071.radial <- sum(diag(confusion_matrix.svm.e1071.radial)) / sum(confusion_matrix.svm.e1071.radial)
prec_svm.e1071.radial <- diag(confusion_matrix.svm.e1071.radial) / rowSums(confusion_matrix.svm.e1071.radial)
rec_svm.e1071.radial <- diag(confusion_matrix.svm.e1071.radial) / colSums(confusion_matrix.svm.e1071.radial)
f1_svm.e1071.radial <- 2 * (prec_svm.e1071.radial * rec_svm.e1071.radial) / (prec_svm.e1071.radial + rec_svm.e1071.radial)

#SVM caret radial
registerDoMC()
trctrl <- trainControl(method = "cv", number = 10)
set.seed(3233)
model.svm.caret.radial <- train(label ~.,
                                data = train,
                                method = "svmRadial",
                                trControl=trctrl)
pred.svm.caret.radial <- predict(model.svm.caret.radial, test)
cm_svm.caret.radial <- confusionMatrix(test$label, pred.svm.caret.radial)
confusion_matrix.svm.caret.radial <- table(test$label, pred.svm.caret.radial)
# accuracy, precision, recall, f-measure
acc_svm.caret.radial <- sum(diag(confusion_matrix.svm.caret.radial)) / sum(confusion_matrix.svm.caret.radial)
prec_svm.caret.radial <- diag(confusion_matrix.svm.caret.radial) / rowSums(confusion_matrix.svm.caret.radial)
rec_svm.caret.radial <- diag(confusion_matrix.svm.caret.radial) / colSums(confusion_matrix.svm.caret.radial)
f1_svm.caret.radial <- 2 * (prec_svm.caret.radial * rec_svm.caret.radial) / (prec_svm.caret.radial + rec_svm.caret.radial)


## Plot Confusion Matrix
# e1071
table.svm.e1071.radial <- data.frame(cm_svm.e1071.radial$table)
plotTable.svm.e1071.radial <- table.svm.e1071.radial %>%
  mutate(goodbad = ifelse(table.svm.e1071.radial$Prediction == table.svm.e1071.radial$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))
ggplot(data = plotTable.svm.e1071.radial, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.svm.e1071.radial$Reference))) +
  ggtitle("Confusion Matrix: SVM e1071 radial")

# caret
table.svm.caret.radial <- data.frame(cm_svm.caret.radial$table)
plotTable.svm.caret.radial <- table.svm.caret.radial %>%
  mutate(goodbad = ifelse(table.svm.caret.radial$Prediction == table.svm.caret.radial$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))
ggplot(data = plotTable.svm.caret.radial, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.svm.caret.radial$Reference))) +
  ggtitle("Confusion Matrix: SVM caret radial")


## ROC/ AUC Plots
# e1071
pred.roc.svm.e1071.radial <- as.numeric(pred.svm.e1071.radial)
roc.multi_test.svm.e1071.radial <- multiclass.roc(test$label, pred.roc.svm.e1071.radial)
rs_test.svm.e1071.radial <- roc.multi_test.svm.e1071.radial[['rocs']]
roc.list.svm.e1071.radial <- list("1950-1960"=rs_test.svm.e1071.radial[[1]],"1950-1970"=rs_test.svm.e1071.radial[[2]],
                                    "1950-1980"=rs_test.svm.e1071.radial[[3]],"1950-1990"=rs_test.svm.e1071.radial[[4]],
                                    "1950-2000"=rs_test.svm.e1071.radial[[5]],"1960-1970"=rs_test.svm.e1071.radial[[6]],
                                    "1960-1980"=rs_test.svm.e1071.radial[[7]],"1960-1990"=rs_test.svm.e1071.radial[[8]],
                                    "1960-2000"=rs_test.svm.e1071.radial[[9]],"1970-1980"=rs_test.svm.e1071.radial[[10]],
                                    "1970-1990"=rs_test.svm.e1071.radial[[11]],"1970-2000"=rs_test.svm.e1071.radial[[12]],
                                    "1980-1990"=rs_test.svm.e1071.radial[[13]],"1980-2000"=rs_test.svm.e1071.radial[[14]],
                                    "1990-2000"=rs_test.svm.e1071.radial[[15]])
ggroc(roc.list.svm.e1071.radial, legacy.axes = TRUE) + geom_abline() + ggtitle("SVM ROC curve e1071 radial") + geom_line(size=2.5)

# caret
pred.roc.svm.caret.radial <- as.numeric(pred.svm.caret.radial)
roc.multi_test.svm.caret.radial <- multiclass.roc(test$label, pred.roc.svm.caret.radial)
rs_test.svm.caret.radial <- roc.multi_test.svm.caret.radial[['rocs']]
roc.list.svm.caret.radial <- list("1950-1960"=rs_test.svm.caret.radial[[1]],"1950-1970"=rs_test.svm.caret.radial[[2]],
                                    "1950-1980"=rs_test.svm.caret.radial[[3]],"1950-1990"=rs_test.svm.caret.radial[[4]],
                                    "1950-2000"=rs_test.svm.caret.radial[[5]],"1960-1970"=rs_test.svm.caret.radial[[6]],
                                    "1960-1980"=rs_test.svm.caret.radial[[7]],"1960-1990"=rs_test.svm.caret.radial[[8]],
                                    "1960-2000"=rs_test.svm.caret.radial[[9]],"1970-1980"=rs_test.svm.caret.radial[[10]],
                                    "1970-1990"=rs_test.svm.caret.radial[[11]],"1970-2000"=rs_test.svm.caret.radial[[12]],
                                    "1980-1990"=rs_test.svm.caret.radial[[13]],"1980-2000"=rs_test.svm.caret.radial[[14]],
                                    "1990-2000"=rs_test.svm.caret.radial[[15]])
ggroc(roc.list.svm.caret.radial, legacy.axes = TRUE) + geom_abline() + ggtitle("SVM ROC curve caret radial") + geom_line(size=2.5)

