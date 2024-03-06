library(tidyverse)
library(corrplot)

reklamy <- read.csv("Advertising.csv",colClasses=c("NULL",NA,NA,NA,NA))

reklamy
summary(reklamy)
#same liczbowe zmienne

cor(reklamy)
# istnieje korelacja miedzy sprzedażą a mediami

cor_tv <- cor(reklamy$TV, reklamy$sales)
cor_radio <- cor(reklamy$radio, reklamy$sales)
cor_newspaper <- cor(reklamy$newspaper, reklamy$sales)

cor_reklamy = cor(reklamy)
corrplot(rowery_korelacje, type = "upper", method = "number")
# zależnośći mediów od sprzedaży 
# TV: 0.782, Radio: 0.576, Gazeta: 0.228


#funcja z mojego zadania z kwadratu anscombe
fit_models <- function(ans, title) {
  # ans jest ramka danych z kolumnami x oraz y
  f <- lm(y~x, data=ans) # dopasuj model liniowy
  print(f$coefficients) # wspolczynniki
  plot(ans$x, ans$y, ylab="Sales", xlab=title, main=title) # wykres
  abline(f, col="red") # linia regresji
  return(f)
}

tv = data.frame(x = reklamy$TV, y= reklamy$sales)
radio = data.frame(x = reklamy$radio, y= reklamy$sales)
gazeta = data.frame(x = reklamy$newspaper, y= reklamy$sales)

fit_models(tv, "TV")
fit_models(radio, "Radio")
fit_models(gazeta, "Gazeta")
#Sprzedaż jest zależna liniowo od TV

test_tv <- lm(reklamy$sales~reklamy$TV)
test_radio = lm(reklamy$sales~reklamy$radio)
#patrzymy na te dwie,bo gazeta na pewno nie jest liniowa (mozna zobaczyc wykresu)

par(mfrow = c(2, 2))
plot(test_tv)
summary(test_tv)

par(mfrow = c(2, 2))
plot(test_radio)
summary(test_radio)