# Music Decade Predictor

## Machine Learning Project 2019/2020 - Università degli Studi di Milano-Bicocca

# Introduction
The aim of the project is to apply different machine learning techniques to a dataset to make a prediction.

## Group 
* **Curnis Giovanni:** 807424 - Master Student in Computer Science
* **Lodovici Federico:** 816040 - Master Student in computer Science
* **Radice Carlo:** 807159 - Master Student in Computer Science

## Dataset

[Audio features of songs ranging from 1922 to 2011](https://www.kaggle.com/uciml/msd-audio-features) from kaggle.

## Aim
Predict the decade of release of a song.

# Usage
First thing to do, is to run:
* ``` data_preprocessing_&_correlation_PCA.R ```: preprocessing of the dataset, train/test split and [**PCA**] analysis. Libraries used are [FactorMineR](https://www.rdocumentation.org/packages/FactoMineR/versions/2.1), [factorextra](https://www.rdocumentation.org/packages/factoextra/versions/1.0.3) and [corrplot](https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html).

After, these scripts can be run in random order. They represent the models used for the project.

* ``` decisionTree.R ```: libraries used are [**rpart**](https://www.rdocumentation.org/packages/rpart/versions/4.1-15/topics/rpart) and [**caret**](http://topepo.github.io/caret/index.html)

* ``` svm.R ```: libraries used are [**e1071**](https://www.rdocumentation.org/packages/e1071/versions/1.7-3) and [**caret**](http://topepo.github.io/caret/index.html)

* ``` naive_bayes.R ```: libraries used are [**e1071**](https://www.rdocumentation.org/packages/e1071/versions/1.7-3) and [**caret**](http://topepo.github.io/caret/index.html)

# Results
These are accuracy results for each model:

### **Decision Tree**
| DecisionTree - rpart | DecisionTree - caret |
|:--- |:--- |
| 0.34 | 0.33 |

### **SVM**
| SVM - e1071 (radial) SVM - caret (radial) |
|:--- |:--- | 
| 0.45 | 0.46 |

### **Naive Bayes**
| Naive Bayes - e1071 | Naive Bayes - caret |
|:--- |:--- |
| 0.93 | 0.95 |
