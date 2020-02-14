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
mtx <- data.frame(res.pca$var$cos2)
dimdf <- as.data.frame(res.pca$ind$coord)
train.data.pca <- dimdf %>% select(1:44)
test.data.pca <- predict(res.pca, subset(test, select = -label))
test.data.pca <- as.data.frame(test.data.pca$coord)
test.data.pca <- test.data.pca %>% select(1:44)
# test.data.pca <- test.data.pca %>% rename(Dim.1 = coord.Dim.1, Dim.2 = coord.Dim.2, Dim.3 = coord.Dim.3, Dim.4 = coord.Dim.4, Dim.5 = coord.Dim.5,
#                                           Dim.6 = coord.Dim.6, Dim.7 = coord.Dim.7, Dim.8 = coord.Dim.8, Dim.9 = coord.Dim.9, Dim.10 = coord.Dim.10,
#                                           Dim.11 = coord.Dim.11, Dim.12 = coord.Dim.12, Dim.13 = coord.Dim.13, Dim.14 = coord.Dim.14, Dim.15 = coord.Dim.15,
#                                           Dim.16 = coord.Dim.16, Dim.17 = coord.Dim.17, Dim.18 = coord.Dim.18, Dim.19 = coord.Dim.19, Dim.20 = coord.Dim.20,
#                                           Dim.21 = coord.Dim.21, Dim.22 = coord.Dim.22, Dim.23 = coord.Dim.23, Dim.24 = coord.Dim.24, Dim.25 = coord.Dim.25,
#                                           Dim.26 = coord.Dim.26, Dim.27 = coord.Dim.27, Dim.28 = coord.Dim.28, Dim.29 = coord.Dim.29, Dim.30 = coord.Dim.30,
#                                           Dim.31 = coord.Dim.31, Dim.32 = coord.Dim.32, Dim.33 = coord.Dim.33, Dim.34 = coord.Dim.34, Dim.35 = coord.Dim.35,
#                                           Dim.36 = coord.Dim.36, Dim.37 = coord.Dim.37, Dim.38 = coord.Dim.38, Dim.39 = coord.Dim.39, Dim.40 = coord.Dim.40,
#                                           Dim.41 = coord.Dim.41, Dim.42 = coord.Dim.42, Dim.43 = coord.Dim.43, Dim.44 = coord.Dim.44, Dim.45 = coord.Dim.45
#                                           )

train.data.pca <- data.frame(label = train$label, train.data.pca)
test.data.pca <- data.frame(label = test$label, test.data.pca)

# scrittura file csv  
write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)

write.csv(train.data.pca, "train_pca.csv", row.names = FALSE)
write.csv(test.data.pca, "test_pca.csv", row.names = FALSE)
