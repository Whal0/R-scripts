library(tidyverse)
library(dplyr)
library(tidyr)
library(performanceEstimation)
library(class)
library(caTools)

df = read.csv("./donors.csv", sep = ",")
glimpse(df)

df_num = df %>%
  select(is.numeric | respondedMailing) 

summary(df_num)
# Brakujące wartości w: age, numberChildren, incomeRating, wealthRating

df_num$age[is.na(df_num$age)] <- mean(df_num$age,na.rm=TRUE)
df_num$numberChildren[is.na(df_num$numberChildren)] <- median(df_num$numberChildren,na.rm=TRUE)
df_num = df_num %>% 
    filter(!is.na(incomeRating)) %>%
    filter(!is.na(wealthRating)) %>%
    filter(wealthRating != 0)

summary(df_num)


normalizacja <- function(x) { (x - min(x)) / (max(x)-min(x)) }

dfnorm = df_num %>%
  mutate(across(1:12, normalizacja))



set.seed(1366)
sample <- sample.split(df_num, SplitRatio = 0.75)
train  <- subset(df_num, sample == TRUE)
test   <- subset(df_num, sample == FALSE)

table(train$respondedMailing)
table(test$respondedMailing)



balanceddf = smote(respondedMailing ~ ., data = train)

table(balanceddf$respondedMailing)



train_label = as.factor(pull(train,respondedMailing))
test_label = as.factor(pull(test,respondedMailing))

train_end = train[,-13]
test_end = test[,-13]




pred_knn = knn(train_end, test_end, cl = train_label, k = 5)

head(pred_knn)

com_m <- table(pred_knn, test_label)
com_m

accuracy_test <- sum(diag(com_m)) / sum(com_m)

print(paste('Dokładność wynosi', accuracy_test))

# punkt 12 dodac do zbioru dane i jescze raz wykonac preducje



pred_knn2 = knn()
