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
train = read.csv("train.csv", header=TRUE)
train$label <- as.factor(train$label)
test = read.csv("test.csv", header=TRUE)
test$label <- as.factor(test$label)
# carico la formula 
fileName <- 'formula.txt'
formula <- readChar(fileName, file.info(fileName)$size)
formula <- gsub("\n", "", formula)
formula <- as.formula(formula)

pca.train <- subset(train, select = -label)
prin_comp <- prcomp(pca.train)
train.data <- data.frame(label = train$label, prin_comp$x)

registerDoMC()
# CARET
model.bayes.caret = train(label ~., data = train.data, "nb", trControl = trainControl(method = "cv", number = 10))
test.data.bayes.caret <- predict(prin_comp, newdata = subset(test, select = -label))

pred.bayes.caret <- predict(model.bayes.caret, test.data.bayes.caret)
cm_bayes.caret <- confusionMatrix(test$label, pred.bayes.caret)
confusion_matrix.bayes.caret <- table(test$label, pred.bayes.caret)
acc_bayes.caret <- sum(diag(confusion_matrix.bayes.caret))/sum(confusion_matrix.bayes.caret)


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
ggroc(roc.list.bayes.caret, legacy.axes = TRUE) + geom_abline() + ggtitle("ROC Curve: NAIVE BAYES with Caret") + geom_line(size=2.5)

