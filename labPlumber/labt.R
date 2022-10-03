library(rpart)
library(rattle)
library(rpart.plot)
library(dplyr)
library(readr)

train <- read_csv("train.csv")
test <- read_csv("test.csv")

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp +
               Parch + Fare + Embarked,
             data = train,
             method = "class")

fancyRpartPlot(fit)

Prediccion <- predict(fit, test, type = "class")
Prediccion

saveRDS(fit, "modelo_final.rds")
