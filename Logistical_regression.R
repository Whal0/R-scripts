library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)

df = read.csv("./lab4-dane_a.csv", sep = ";")
#przy wczytywaniu encoding sie psuje na lokalnej maszynie
colnames(df)[3] <- "Plec" 
# ^Ignore^
head(df)
summary(df)
#Pare kategorycznych, sprawdzimy liczebnosc
table(df$GRUPA)
table(df$MiejsceZam)
table(df$Plec)
table(df$PoronSamo)
table(df$InfOddech)
table(df$WyksztM)
# usuwanie NA, omijamy krok wypelniania i usuwania NA
# df <- na.omit(df)
#histogramy waznych liczbowych zmiennych
hist(df$Wiek.M)
hist(df$MasaUr)
#Najczesciej dziecko ma miedzy 3 a 4 kg przy porodzie, a najczestszy wiek matki to 25-30, 
# wiek matki przypomina rozklad normalny z brakiem danych po lewej stronie wierzcholka

#zmieniamy wartosci na binarne w GRUPIE.
#df$GRUPA <- replace(df$GRUPA, df$GRUPA == "badana", 0)

# Kategorie na liczby zeby zrobic model
df <- df %>%
  mutate(GRUPA = plyr::revalue(GRUPA,c("badana" = 0, "kontrolna" = 1))) %>%
  mutate(MiejsceZam = plyr::revalue(MiejsceZam, c("miasto" = 1, "wies" = 0))) %>%
  mutate(Plec = plyr::revalue(Plec, c("K" = 1, "M" = 0))) %>%
  mutate(PoronSamo = plyr::revalue(PoronSamo, c("tak" = 1, "nie" = 0))) %>%
  mutate(InfOddech = plyr::revalue(InfOddech, c("tak" = 1, "nie" = 0))) %>%
  mutate(Palenie = plyr::revalue(Palenie, c("tak" = 1, "nie" = 0))) %>%
  mutate(WyksztM = plyr::revalue(WyksztM, c("podst" = 1, "srednie" = 2, "wyzsze" = 3, "zawod" = 4)))

summary(df)
#zamiana kategorycznych na numeryczne

df <- df %>% 
  mutate_at(c('GRUPA', 'MiejsceZam', 'Plec', 'PoronSamo', 'InfOddech', 'Palenie', 'WyksztM'), as.numeric)

glm.fit <-  glm(GRUPA~., data = df, family = binomial)

summary(glm.fit)

#Wyciagamy wplywowe zmienne do kolejnego modelu
model1 <-  glm(GRUPA~Plec+MasaUr+Wiek.M+KolCiazy+InfOddech+Palenie, data = df, family = binomial)

summary(model1)
# sprawdzamy wspoliniowosc zmiennych
vif(model1)
df2 <- data.frame(df$Plec,df$MasaUr,df$Wiek.M,df$KolCiazy,df$InfOddech,df$Palenie)
#na chwile usuwamy zeby uzyskac caly macierz koleracji ()
df_corr <- cor(na.omit(df2))
corrplot(df_corr,method='number',is.corr = F)
#Wniosek: brak wspolliniowosci zmiennych


# Analiza modelu (analiza szans)
exp(coef(model1))
# Plec dziecka zwieksza brak wystapienia wady o 66% u dziewczynek, 
# najbardziej wplywowo na pojawienie sie wady wplywa czy matka jest palaczem, jesli jest, ryzyko pojawienia sie wady wzrasta o 80%