library("FactoMineR") 
library("factoextra")
library("corrplot")
library("groupdata2")
library("BBmisc")
library("dplyr")

data <- read.csv("year_prediction.csv", header=TRUE)

cdf <- data
cdf <- cdf[cdf$label > 1949 & cdf$label < 2010,]
cdf$label[cdf$label >= 1950 & cdf$label <= 1959] <- 1950
cdf$label[cdf$label >= 1960 & cdf$label <= 1969] <- 1960
cdf$label[cdf$label >= 1970 & cdf$label <= 1979] <- 1970
cdf$label[cdf$label >= 1980 & cdf$label <= 1989] <- 1980
cdf$label[cdf$label >= 1990 & cdf$label <= 1999] <- 1990
cdf$label[cdf$label >= 2000 & cdf$label <= 2009] <- 2000
cdf$label <- as.factor(cdf$label)
plot(cdf$label)

# MATRICE DI CORRELAZIONE
#M <- cor(cdf)
#corrplot(M, method="circle")

rdata <- downsample(cdf, cat_col="label")
plot(rdata$label)
rdata <- normalize(rdata, method = "range", range = c(0, 1))

# MATRICE DI CORRELAZIONE
# M <- cor(rdata[,-1])
# corrplot(M, method="square")

## DATA SPLITTING
# split dataset function
split.data = function(data, p, s = 1){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train=train, test=test))
}
# split 70 - 30
allset = split.data(rdata, p=0.7)
train = allset$train
test = allset$test
plot(train$label)
plot(test$label)

## PCA
train.active <- train[, c(2:91)]
res.pca <- PCA(train.active, scale.unit=TRUE, graph=FALSE, ncp=90)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#fviz_pca_var(res.pca, col.var = "black")
eig.val <- get_eigenvalue(res.pca)
dimdf <- as.data.frame(res.pca$ind$coord)
train.data.pca <- dimdf %>% select(1:44)
test.data.pca <- predict(res.pca, subset(test, select = -label))
test.data.pca <- as.data.frame(test.data.pca$coord)
test.data.pca <- test.data.pca %>% select(1:44)


train.data.pca <- data.frame(label = train$label, train.data.pca)
test.data.pca <- data.frame(label = test$label, test.data.pca)

# scrittura file csv  
write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)

write.csv(train.data.pca, "train_pca.csv", row.names = FALSE)
write.csv(test.data.pca, "test_pca.csv", row.names = FALSE)
