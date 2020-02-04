library("FactoMineR") 
library("factoextra")
library("corrplot")
library("groupdata2")
library("BBmisc")

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

## DATA DOWNSAMPLING
# ns = 5000
# col = "label"
# x<-as.factor(cdf[,col])
# freq_x<-table(x)
# prob_x<-freq_x/sum(freq_x)
# df_prob = prob_x[as.factor(cdf[,col])]
# nr=nrow(cdf)
# sLevels = levels(as.factor(cdf[,col]))
# nLevels = length(sLevels)
# rat = ns/nr
# rdata = NULL
# for (is in seq(1,nLevels)) {
#    ldata <- cdf[cdf[,col]==sLevels[is],]
#    ndata <- nrow(ldata)
#    nsdata = max(ndata*rat,1)
#    srows <- sample(seq(1,ndata),nsdata,replace=rat>1)
#    sdata <- ldata[srows,]
#    rdata <- rbind(rdata,sdata)
# }

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
res.pca <- PCA(train.active, scale.unit=TRUE, graph=FALSE, ncp = 30)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#fviz_pca_var(res.pca, col.var = "black")
eig.val <- get_eigenvalue(res.pca)
mtx <- data.frame(res.pca$var$cos2)
dims <- mtx[mtx$Dim.1>0.2 | mtx$Dim.2>0.2 | mtx$Dim.3>0.2 | mtx$Dim.4>0.2 | 
              mtx$Dim.5>0.2 | mtx$Dim.6>0.2 | mtx$Dim.7>0.2 | mtx$Dim.8>0.2 |
              mtx$Dim.9>0.2 | mtx$Dim.10>0.2 | mtx$Dim.11>0.2 | mtx$Dim.12>0.2 |
              mtx$Dim.13>0.2 | mtx$Dim.14>0.2 | mtx$Dim.15>0.2 | mtx$Dim.16>0.2 |
              mtx$Dim.17>0.2 | mtx$Dim.18>0.2 | mtx$Dim.19>0.2 | mtx$Dim.20>0.2 |
              mtx$Dim.21>0.2 | mtx$Dim.22>0.2 | mtx$Dim.23>0.2 | mtx$Dim.24>0.2 |
              mtx$Dim.25>0.2, ]
rn <- rownames(dims)
formula <- paste("label", paste(rn, collapse=" + "), sep=" ~ ")

# scrittura file csv  
write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)
# scrittura file formula
fileConn<-file("formula.txt")
writeLines(formula, fileConn)
close(fileConn)
