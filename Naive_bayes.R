library(tidyverse)
library(dplyr)
library(stringr)

email = read.csv("./email.csv", sep = ",")
head(email)

transform(email, message_label = as.factor(message_label))
class(email$message_label)

email1 = gather(email, key= "word", value = "count", -message_index, -message_label)


class(email$count)

result <- email1 %>%
  group_by(word) %>%
  summarise(occurrence = sum(count)) %>%
  arrange(desc(occurrence))

top10_all <- slice(result, 1:10)

result_spam <- email1 %>%
  group_by(word, message_label) %>%
  summarise(occurrence = sum(count)) %>%
  arrange(message_label, desc(occurrence))

top_10_spam <- result_spam %>%
  filter(message_label == 'spam')

top_10_ham <- result_spam %>%
  filter(message_label == 'ham') %>%
  slice(1:10)