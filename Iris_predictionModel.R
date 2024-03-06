library(tidyverse)
library(dplyr)
library(corrplot)
library(caret)
library(ggplot2)
library(rpart.plot)

#użyjmy jednego z najbardziej popularnych zestawów danych - iris
data(iris)

head(iris)
#Jak widzimy każdy kwiat zawiera on długośc i szerokośc kielicha i płatków oraz gatunek do którego należą
summary(iris)
# Są 3 klasy po 50 obserwacji z każdego gatunku kwiatów  - 150 łącznie
str(iris)
# 4 liczbowe i 1 kategoryczna z klasą kwiatu
# Spróbujmy zrobić drzewo decyzyjne które pozwoli nam określić gatunek irysu na podstawie jego "wymiarów"


#Z summary wynika, że nie ma NA, ale upewnijmy się:
empty <- sum(is.na(iris))
if (empty > 0) {
  iris <- na.omit(iris)
}

k = cor(iris[1:4])
corrplot(k, method = 'color')
#Wszystkie zmienne mocno skolerowane, z wyjątkiem szerokośc kielicha (negatywnie skolerowana i w przedziale [-0.1,-0.45])



# Podzielmy zestaw dane na trening test za pomocą bilioteki cater
set.seed(37853)
probka <- createDataPartition(y = iris$Species, p= 0.7, list = FALSE)
test <- iris[-probka,]    # 30% test / 70% train
train <- iris[probka,]

control <- trainControl(method = "cv", # walidacja krzyżowa
                        verboseIter = TRUE,
                        savePredictions = "final",
                        classProbs = TRUE) 

model <- train(Species ~ ., data = iris,
               trControl = control,
               maximize = TRUE,
               method = "rpart",
               parms = list(split = "information")
               )


model
#wizualizacja drzewa 
prp(model$finalModel, extra = 1, faclen=0,  nn = T,
    box.col=c("orange", "blue"))

confusionMatrix.train(model)
# Średnia celnośc modelu 92.7%


iris_pred <- predict(model, newdata = test)
confusionMatrix(data = iris_pred, reference = test$Species)
# Model umie z dokładnośćią 95% skategoryzować gatunki versicolor i virginica. Zawsze poprawnie skategoryzował setosy

#sprawdzmy które cechy były najważniejsze w modelu
varImp(model)


# Wnioski można wyciągnać następujące:
# Bardzo łatwo jest zkategoryzować irys na podstawie wielkośći jego płatków kwiatów
# Jak najłatwiej rozpoznać dany gatunek
# Jakie wartośći tych wielkośći należą do danego gatunku
# Szerokośc kielicha jest bezużyteczna przy klasyfikacji