rm(list = ls())
graphics.off()


library(tidyverse)

df <- read_csv("./data/upwork_data_0.csv")

glimpse(df)

df %>% 
  ggplot(aes(x = over_50k, y = age)) +
  geom_boxplot()



df %>% 
  ggplot(aes(x = over_50k, y = education_num)) +
  geom_boxplot()





df %>% 
  ggplot(aes(fill = over_50k, x = workclass)) +
  geom_bar(position = "dodge")+
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  #scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()


df %>% 
  ggplot(aes(x = education_num)) +
  geom_histogram()

df %>% 
  filter(abs(capital_gain) < 5000) %>% 
  ggplot(aes(x = capital_gain)) +
  geom_histogram()


df %>% 
  filter(abs(capital_loss) < 5000) %>% 
  ggplot(aes(x = capital_loss)) +
  geom_histogram()


summary(df)



df %>% 
  select_if(is.character) %>% 
  lapply(X = ., FUN = function(x) table(x))


df %>% 
  select_if(is.numeric) %>% 
  lapply(X = ., FUN = function(x) sum(is.na(x)))



df %>% 
  ggplot(aes(x = hours_per_week)) +
  geom_histogram()


df %>% 
  ggplot(aes(x = over_50k, y = hours_per_week)) +
  geom_violin()


