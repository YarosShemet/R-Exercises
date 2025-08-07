rm(list=ls())
list_of_packages <- c("readr" # wczytywanie danych
                      ,"dplyr" # manipulacja danymi
                      ,"rpart" # drzewa decyzyjne
                      ,"rpart.plot" # Ĺ‚adne wykresy dla drzew
                      ,"randomForest" # lasy losowe
                      ,"ROCR" # ocena jakoĹ›ci modelu - krzywa ROC, AUC, itd.
                      ,"MASS" # dobĂłr zmiennych do modelu
)                      
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)
lapply(list_of_packages, library, character = TRUE)
library(mlbench)
data("PimaIndiansDiabetes")
#library(readr)
#startup_data <- read_csv("D:/STUDIA/SGH/V semester/Indukowane regu?y decyzyjne/start_ups/startup data.csv")
customer_churn <- read_csv('D:/STUDIA/SGH/V semester/Indukowane reguły decyzyjne/bank_churn/Bank Customer Churn Prediction.csv')
#View(startup_data)
customer_churn <- customer_churn[2:12]

# a ---------------------------------------------------------------
diabetes_f <- data.frame(customer_churn)
#diabetes_f <- data.frame(startup_data)
diabetes_data <- na.omit(diabetes_f)
#uczący i treningowy zbiór
set.seed(25)
test_prop <- 0.25
test.set.index <- (runif(nrow(diabetes_data)) < test_prop)
diabetes_data.test <- diabetes_data[test.set.index, ]
diabetes_data.train <- diabetes_data[!test.set.index, ]


# b ---------------------------------------------------------------
#drzewo
dia_tree <- rpart(churn ~ .,
              data = diabetes_data.train,
              method = "class")
rpart.plot(dia_tree, under = FALSE, tweak = 1.1, fallen.leaves = TRUE)
#las
dia_forest <- randomForest(churn ~ .,
                           data = diabetes_data.train)
varImpPlot(dia_forest)

# c -----------------------------------------------------------------------
dia_tree$variable.importance 
dia_forest$importance
#glukoza ma największy wpływ na prawdopodobieństwo wystąpienia cukrzycy 

# d -----------------------------------------------------------------------
CM <- list()
CM[["dia_tree"]] <- table(predict(dia_tree, new = diabetes_data.test, type = "class"), diabetes_data.test$diabetes)
CM[["dia_forest"]]  <- table(predict(dia_forest, new = diabetes_data.test, type = "class"), diabetes_data.test$diabetes)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  false_negative <- classif_mx[1, 2]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])

  accuracy <- (true_positive + true_negative) / sum(classif_mx) #accuracy
  sensitivity <- true_positive / condition_positive #recall
  miss_rate <- false_negative / condition_positive  #false negative rate  #lub miss_rate = 1 - sensitivity
  return(list(accuracy = accuracy,
              sensitivity = sensitivity,
              miss_rate = miss_rate))
}

sapply(CM, EvaluateModel)
#ze względu na lepszy FNR model "tree" prognozuje lepiej

# e -----------------------------------------------------------------------

preds <- list()

preds[["dia_tree"]] <- as.vector(predict(dia_tree, newdata = diabetes_data.test)[, 2])
preds[["dia_forest"]] <- as.vector(predict(dia_forest, newdata = diabetes_data.test, type = "prob")[, 2])
#AUC
(performance(prediction(preds[["dia_tree"]], diabetes_data.test$diabetes), "auc")@y.values[[1]])
for (i in 1:length(preds)){
  cat(names(preds)[i], ": ", performance(prediction(preds[[i]], diabetes_data.test$diabetes), "auc")@y.values[[1]], "\n")
}
#ze względu na auc, model "forest" jest lepszy, ale kontynuuje zgodnie z poleceniem z "tree"
#ROC
plot(performance(prediction(preds[["dia_tree"]], diabetes_data.test$diabetes), "tpr", "fpr"), lwd = 2, colorize = T) 
#lift
plot(performance(prediction(preds[["dia_tree"]], diabetes_data.test$diabetes), "lift", "rpp"), lwd = 2, col = "red")
