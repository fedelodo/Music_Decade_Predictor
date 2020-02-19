library("rpart")
library("rpart.plot")
library("rattle")
library("RColorBrewer")
library("caret")
library("ggplot2")
library("dplyr")
library("doMC")
library("pROC")

# carico i dataset
# train = read.csv("train.csv", header=TRUE)
# train$label <- as.factor(train$label)
# test = read.csv("test.csv", header=TRUE)
# test$label <- as.factor(test$label)

# carico i dataset PCA
train = read.csv("train_pca.csv", header=TRUE)
train$label <- as.factor(train$label)
test = read.csv("test_pca.csv", header=TRUE)
test$label <- as.factor(test$label)

# Decision Tree rpart
registerDoMC()
model.decisiontree.rpart <- rpart(label ~.,
                                  data=train,
                                  control = rpart.control(cp = 0.001),
                                  method="class")
fancyRpartPlot(model.decisiontree.rpart)
pred.decisiontree.rpart <- predict(model.decisiontree.rpart, test, type="class")
cm_decisiontree.rpart <- confusionMatrix(test$label, pred.decisiontree.rpart)
confusion_matrix.decisiontree.rpart = table(test$label, pred.decisiontree.rpart)
# accuracy, precision, recall, f-measure
acc_decisiontree.rpart <- sum(diag(confusion_matrix.decisiontree.rpart))/sum(confusion_matrix.decisiontree.rpart)
prec_decisiontree.rpart <- diag(confusion_matrix.decisiontree.rpart) / rowSums(confusion_matrix.decisiontree.rpart)
rec_decisiontree.rpart <- diag(confusion_matrix.decisiontree.rpart) / colSums(confusion_matrix.decisiontree.rpart)
f1_decisiontree.rpart <- 2 * (prec_decisiontree.rpart * rec_decisiontree.rpart) / (prec_decisiontree.rpart + rec_decisiontree.rpart)

# Decision Tree caret
registerDoMC()
traincontroll.decisiontree.caret <- trainControl(method = "cv", number = 10)
model.decisiontree.caret <- train( label ~., data = train, method = "ctree", trControl = traincontroll.decisiontree.caret, tuneLength = 10)
pred.decisiontree.caret <- predict(model.decisiontree.caret, test)
cm_decisiontree.caret <- confusionMatrix(test$label, pred.decisiontree.caret)
confusion_matrix.decisiontree.caret = table(test$label, pred.decisiontree.caret)
# accuracy, precision, recall, f-measure
acc_decisiontree.caret <- sum(diag(confusion_matrix.decisiontree.caret))/sum(confusion_matrix.decisiontree.caret)
prec_decisiontree.caret <- diag(confusion_matrix.decisiontree.caret) / rowSums(confusion_matrix.decisiontree.caret)
rec_decisiontree.caret <- diag(confusion_matrix.decisiontree.caret) / colSums(confusion_matrix.decisiontree.caret)
f1_decisiontree.caret <- 2 * (prec_decisiontree.caret * rec_decisiontree.caret) / (prec_decisiontree.caret + rec_decisiontree.caret)

## Plot Confusion Matrix
# rpart
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
  ggtitle("Confusion Matrix: Decision Tree rpart")

# caret
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
  ggtitle("Confusion Matrix: Decision Tree caret")

## ROC/ AUC Plots
# rpart
pred.roc.decisiontree.rpart <- as.numeric(pred.decisiontree.rpart)
roc.multi_test.decisiontree.rpart <- multiclass.roc(test$label, pred.roc.decisiontree.rpart)
rs_test.decisiontree.rpart <- roc.multi_test.decisiontree.rpart[['rocs']]
roc.list.decisiontree.rpart <- list("1950-1960"=rs_test.decisiontree.rpart[[1]],"1950-1970"=rs_test.decisiontree.rpart[[2]],
                                    "1950-1980"=rs_test.decisiontree.rpart[[3]],"1950-1990"=rs_test.decisiontree.rpart[[4]],
                                    "1950-2000"=rs_test.decisiontree.rpart[[5]],"1960-1970"=rs_test.decisiontree.rpart[[6]],
                                    "1960-1980"=rs_test.decisiontree.rpart[[7]],"1960-1990"=rs_test.decisiontree.rpart[[8]],
                                    "1960-2000"=rs_test.decisiontree.rpart[[9]],"1970-1980"=rs_test.decisiontree.rpart[[10]],
                                    "1970-1990"=rs_test.decisiontree.rpart[[11]],"1970-2000"=rs_test.decisiontree.rpart[[12]],
                                    "1980-1990"=rs_test.decisiontree.rpart[[13]],"1980-2000"=rs_test.decisiontree.rpart[[14]],
                                    "1990-2000"=rs_test.decisiontree.rpart[[15]])
ggroc(roc.list.decisiontree.rpart, legacy.axes = TRUE) + geom_abline() + ggtitle("Decision Tree ROC curve rpart") + geom_line(size=2.5)

# caret
pred.roc.decisiontree.caret <- as.numeric(pred.decisiontree.caret)
roc.multi_test.decisiontree.caret <- multiclass.roc(test$label, pred.roc.decisiontree.caret)
rs_test.decisiontree.caret <- roc.multi_test.decisiontree.caret[['rocs']]
roc.list.decisiontree.caret <- list("1950-1960"=rs_test.decisiontree.caret[[1]],"1950-1970"=rs_test.decisiontree.caret[[2]],
                                    "1950-1980"=rs_test.decisiontree.caret[[3]],"1950-1990"=rs_test.decisiontree.caret[[4]],
                                    "1950-2000"=rs_test.decisiontree.caret[[5]],"1960-1970"=rs_test.decisiontree.caret[[6]],
                                    "1960-1980"=rs_test.decisiontree.caret[[7]],"1960-1990"=rs_test.decisiontree.caret[[8]],
                                    "1960-2000"=rs_test.decisiontree.caret[[9]],"1970-1980"=rs_test.decisiontree.caret[[10]],
                                    "1970-1990"=rs_test.decisiontree.caret[[11]],"1970-2000"=rs_test.decisiontree.caret[[12]],
                                    "1980-1990"=rs_test.decisiontree.caret[[13]],"1980-2000"=rs_test.decisiontree.caret[[14]],
                                    "1990-2000"=rs_test.decisiontree.caret[[15]])
ggroc(roc.list.decisiontree.caret, legacy.axes = TRUE) + geom_abline() + ggtitle("Decision Tree ROC curve caret") + geom_line(size=2.5)
