library(tidyverse)
library(dplyr) 
library(reshape2)

gios <- read_csv("gios-pjp-data1.csv",col_types = cols(...1 = col_skip()))
names(gios)[2:8] <- c("PM25", "PM10", "NO2", "NO", "NOx", "CO", "C6H6") # ta sama struktura nazw co poprzednie pliki

gios_d <- gios %>%
  mutate(dzien = as.Date(Data)) %>% # jako date
  group_by(dzien) %>% #grupowanie po dniu
  summarize(srednie_PM25 = mean(PM25,na.rm=TRUE), srednie_PM10 = mean(PM10, na.rm=TRUE), srednie_NO2 = mean(NO2, na.rm=TRUE),srednie_NO = mean(NO, na.rm=TRUE),srednie_NOx = mean(NOx, na.rm=TRUE),srednie_CO = mean(CO, na.rm=TRUE), srednie_C6H6 = mean(C6H6, na.rm=TRUE))
# mozna uzyc apply zeby ominac powtorny kod 


gios_d %>%
  filter(weekdays(dzien) == "Tuesday") %>% #wyieramy 1 dzien tygodnia
  summarise(dzien_tyg = mean(srednie_NO2)) #srednia 
