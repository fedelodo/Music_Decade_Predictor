# Music Decade Predictor

## Machine Learning Project 2019/2020 - Università degli Studi di Milano-Bicocca

Ogni gruppo doveva identificare un dominio di suo interesse per il quale intende indurre un modello di classificazione supervisionata e/o non supervisionata ed individuare il relativo dataset. 


# Introduction

## Group 
* **Curnis Giovanni:** 807424 - Master Student in Computer Science
* **Lodovici Federico:** 816040 - Master Student in computer Science **(più o meno)**
* **Radice Carlo:** 807159 - Master Student in Computer Science

## Dataset

[Audio features of songs ranging from 1922 to 2011](https://www.kaggle.com/uciml/msd-audio-features) from kaggle.\
Nel dataset troviamo come features i timbri musicali **(?)** ... spiegare gli attributi che abbiamo...

## Target

Il nostro obbiettivo è quello di predire l'anno in cui un brano è stato scritto.

...link alla relazione...

# Usage
La prima cosa da fare è sistemare il dataset eseguendo il seguente scriptfile:

* ``` data_preprocessing_&_correlation_PCA.R ```: questo script serve per elaborare i dati preparandoli per la fase di training, fare un downsampling per bilanciare il dataset, splittare i dati in train e test set e effettuare la **PCA**, in quest'ultima parte sono state utilizzate le librerie [FactorMineR](https://www.rdocumentation.org/packages/FactoMineR/versions/2.1), [factorextra](https://www.rdocumentation.org/packages/factoextra/versions/1.0.3) e [corrplot](https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html).

Una volta che il dataset è stato elaborato e splittato in *train.csv* per il trainset e *test.csv* per il testset possono essere eseguiti i vari script (in ordine qualsiasi):

* ``` decisionTree.R ```: in questo script usiamo gli **alberi di decisione** per indurre dei modelli di machine learning, le librerie utilizzate sono [**rpart**](https://www.rdocumentation.org/packages/rpart/versions/4.1-15/topics/rpart) e [**caret**](http://topepo.github.io/caret/index.html)

* ``` svm.R ```: in questo script usiamo **svm** per indurre dei modelli di machine learning, le librerie utilizzate sono [**e1071**](https://www.rdocumentation.org/packages/e1071/versions/1.7-3) e [**caret**](http://topepo.github.io/caret/index.html)

* ``` naive_bayes.R ```: in questo script usiamo **naive bayes** per indurre dei modelli di machine learning, le librerie utilizzate sono [**e1071**](https://www.rdocumentation.org/packages/e1071/versions/1.7-3) e [**caret**](http://topepo.github.io/caret/index.html)

> Ogni script contiene anche la generazione delle matrici di confusione per la visualizzazione e l'analisi dei risultati.

# Result

Qui sotto viene riportata la tabella con una media dei valori di accuratezza ottenuti per ogni modello provato

### **Decision Tree**
| DecisionTree - rpart | DecisionTree - caret (ctree) |
|:--- |:--- |
| 0.27 | 0.33 |

### **SVM**
| SVM - e1071 (linear)| SVM - e1071 (radial) | SVM - caret (linear)| SVM - caret (radial) |
|:--- |:--- |:--- |:--- | 
| 0.40 | 0.45 | 0.41 | 0.46 |

### **Naive Bayes**
| Naive Bayes - e1071 | Naive Bayes - caret |
|:--- |:--- |
| 0.93 | 0.95 |
