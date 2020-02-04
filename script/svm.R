library("e1071")
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

# SVM Model e1071, RADIAL
model.svm.e1071.radial <- svm(formula,
                     data = train,
                     kernel='radial',
                     method='C-classification',
                     cost=10,
                     probability =TRUE)
summary(model.svm.e1071.radial)
pred.svm.e1071.radial <- predict(model.svm.e1071.radial, test)
cm_svm.e1071.radial <- confusionMatrix(test$label, pred.svm.e1071.radial)
confusion_matrix.svm.e1071.radial <- table(test$label, pred.svm.e1071.radial)
acc_svm.e1071.radial <- sum(diag(confusion_matrix.svm.e1071.radial)) / sum(confusion_matrix.svm.e1071.radial)

# SVM Model e1071, LINEAR
model.svm.e1071.linear <- svm(formula,
                    data = train,
                    kernel='linear',
                    method='C-classification',
                    cost=1,
                    probability =TRUE)
summary(model.svm.e1071.linear)
pred.svm.e1071.linear <- predict(model.svm.e1071.linear, test)
cm_svm.e1071.linear <- confusionMatrix(test$label, pred.svm.e1071.linear)
confusion_matrix.svm.e1071.linear <- table(test$label, pred.svm.e1071.linear)
acc_svm.e1071.linear <- sum(diag(confusion_matrix.svm.e1071.linear)) / sum(confusion_matrix.svm.e1071.linear)

# # SVM Model Caret, RADIAL
# #trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = TRUE)
# set.seed(3233)
# model <- train(formula, 
#                     data = train, 
#                     method = "svmRadial",
#                     #trControl=trctrl,
#                     preProcess = c("center", "scale"),
#                     tuneLength = 10,
#                     metric = "Accuracy")
# pred <- predict(model, test)
# cm <- table(pred, test$label)
# acc <- sum(diag(cm)) / sum(cm)


##plot confusion matrix
# plot caret confusion matrix
table.svm.e1071.linear <- data.frame(cm_svm.e1071.linear$table)

plotTable.svm.e1071.linear <- table.svm.e1071.linear %>%
  mutate(goodbad = ifelse(table.svm.e1071.linear$Prediction == table.svm.e1071.linear$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable.svm.e1071.linear, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table.svm.e1071.linear$Reference))) +
  ggtitle("Confusion Matrix: SVM - Linear with e1071 (on Normalize)")

# plot e1071 confusion matrix
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
  ggtitle("Confusion Matrix: SVM - Radial with e1071 (on Normalize)")
