library(tidyverse)
library(dplyr)
library(caTools)
library(caret)
library(rpart)
library(ipred) 
library(gbm)
library(randomForest)
library(foreach)
library(adabag)


movie = read.csv("./Movie_classification.csv", sep = ",")
head(movie)

summary(movie)
# zmiana zmiennej na booliana (psuje cześć rzeczy potem, wykomentowane)
movie$X3D_available <- ifelse(movie$X3D_available=="YES",1,0)
# movie$X3D_available <- as.logical(movie$X3D_available)

movie$Genre <- as.factor(movie$Genre)

# zastepujemy wartości NA średnią
movie = movie %>% 
      mutate(Time_taken = replace_na(Time_taken,mean(Time_taken, na.rm = TRUE)))

summary(movie)

# podział danych
set.seed(1366)

movie$Start_Tech_Oscar <- factor(movie$Start_Tech_Oscar)

class(movie$Start_Tech_Oscar)

sample <- sample.split(movie$Start_Tech_Oscar, SplitRatio = 0.75)
train  <- subset(movie, sample == TRUE)
test   <- subset(movie, sample == FALSE)

prop.table(table(train$Start_Tech_Oscar))
prop.table(table(test$Start_Tech_Oscar))


#bagging

#train$Start_Tech_Oscar <- factor(train$Start_Tech_Oscar)
#test$Start_Tech_Oscar <- factor(test$Start_Tech_Oscar)


model_bag <- bagging(
  formula = Start_Tech_Oscar ~ .,
  data = train,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

#pred_bag <- predict(model_bag, test)

train$pred.class <- predict(model_bag, train)

confusionMatrix(data=factor(train$pred.class),
                reference=factor(train$Start_Tech_Oscar),
                positive='1')
#cos jest nie tak bo wychodzi 100% dokładności - przeuczenie


#Random Forest


# TODO DATAFRAME ZAMIAST TIBBLE
model_rf <- randomForest(Start_Tech_Oscar ~ .,  data = train)

model_rf

pred_rf <- predict(model_rf, test)

confusionMatrix(as.factor(pred_rf) ,as.factor(test$Start_Tech_Oscar))

#Boosting
# UWAGA: na maszynie uczelni działalo, ale lokalnie nie mogę sprawdzić bo moje Rstudio ma problem z biblioteką adabag
model_boost <- boosting(Start_Tech_Oscar ~ ., data = train,
                                 boos = TRUE, mfinal = 10,
                                 control = rpart.control(cp = 0.01, minsplit = 3))

model_boost

pred_boost <- predict(model_boost, test)

confusionMatrix(pred_boost ,test$Start_Tech_Oscar)

#Sprawdzanie
#confusionMatrix(pred_bag,test)
#confusionMatrix(pred_rf,test)
#confusionMatrix(pred_boost,test)