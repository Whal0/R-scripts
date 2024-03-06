library(rpart)
library(tidyverse)
library(dplyr)
library(party)
library(rpart.plot)

df = read.csv("./Movie_classification.csv", sep = ",")
#sprawdzamy co jest w srodku
summary(df)
head(df)
#sprawdzamy brakujace wartosci
sum(is.na(df))
df = na.omit(df)
sum(is.na(df))

#tworzymy zbiory train/test

proba <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[proba, ]
test   <- df[!proba, ]

# tworzymy drzewo
model <- rpart(Start_Tech_Oscar~., data = train, method = "class")

model
# graficzna reprezentacja
rpart.plot(model)
# 
przewid <-predict(model, test, type = 'class')
#macierz pomylek
com_m <- table(test$Start_Tech_Oscar, przewid)
com_m
#policzmy dokladnosc modelu
accuracy_test <- sum(diag(com_m)) / sum(com_m)

print(paste('Dokładność modelu dla danych testowych wynosi', accuracy_test))