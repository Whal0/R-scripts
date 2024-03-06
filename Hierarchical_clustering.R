library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)

load("nasiona.Rdata")

head(nasiona)
summary(nasiona)

k = cor(nasiona)
corrplot(k, method = 'color')

# wspolczynnik_asymetrii i zwiezlosc maja maly korelacje wiec tez je odrzucam.
nasiona_norm <- scale(nasiona[, c("powierzchnia", "obwod","dlugosc_ziarna","szerokosc_ziarna", "dlugosc_wyzlobienia_ziarna")])
summary(nasiona_norm)


dist_matrix <- dist(nasiona_norm, method = "euclidean")
dist_matrix

# tworzymy drzewo
hclust_result <- hclust(dist(nasiona_norm), method = "ward.D2")

# wykres, dosyć spory, mało informacji z niego
plot(hclust_result, main = "Dendrogram")

# Grupujemy
clusters <- cutree(hclust_result, k = 5)
rect.hclust(hclust_result, k = 5, border = 2:6)

# dodajemy label grup
nasiona$group <- clusters

# zbieramy i liczymy po grupie
summary_table <- aggregate(. ~ group, data = nasiona[, -1], mean)
summary_table$count <- table(nasiona$group)
print(sum_table)

# Wizualizacja zaleznosci
ggplot(nasiona, aes(x = powierzchnia, y = obwod, color = factor(group))) +
  geom_point() +
  labs(title = "Wykres zależności powierzchni od obwodu", x = "Powierzchnia", y = "Obwód") +
  labs(color='Legenda')