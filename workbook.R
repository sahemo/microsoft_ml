rm(list = ls())
graphics.off()


library(tidyverse)

library(janitor)

df <- read_csv("./data/upwork_data_0.csv") %>% 
  filter(!is.na(over_50k))







df %>% 
  select_if(is.character) %>% 
  map(.x = names(select(., -over_50k)), .f = ~tabyl(df, !!sym(.x), over_50k) %>%  
        adorn_percentages() %>% 
        adorn_pct_formatting() %>% 
        adorn_ns())









df_cleaned <- df %>% 
  mutate(hours_per_week = replace(x = hours_per_week, list = hours_per_week > 100, values = NA),
         age = replace(x = age, list = age < 18, values = NA),
         relationship = case_when(
           relationship %in% c("Husband", "Wife") ~ "with spouse",
           relationship %in% c("Not-in-family", "Unmarried") ~ relationship,
           TRUE ~ "Without spouse"
         ),
         occupation = case_when(
           occupation %in% c('?', 'Armed-Forces', 'Farming-fishing', 'Handlers-cleaners', 'Other-service', 'Priv-house-serv') ~ 'Low Salary Jobs',
           TRUE ~ occupation
         ),
         marital_status = case_when(
           marital_status %in% c("Married-civ-spouse", "Divorced", "Never-married") ~ marital_status,
           marital_status %in% c("Separated", "Widowed") ~ "Sep or widowed",
           TRUE ~ "Others"
         ),
         workclass = case_when(
           workclass %in% c("?", "Never-worked", "Without-pay") ~ "Others",
           TRUE ~ workclass
         ),
         native_country = case_when(
           native_country %in% c("Canada", "China", "Cuba", "India", "Philippines", "United-States") ~ native_country,
           TRUE ~ "Others"
         ),
         race = str_replace_all(race, " ", "")) %>% 
  select(-starts_with("capital"), -education)





df_cleaned %>% 
  select_if(is.character) %>% 
  map(.x = names(select(., -over_50k)), .f = ~tabyl(df_cleaned, !!sym(.x), over_50k) %>%  adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns())






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


summary(df %>% select_if(is.numeric))



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


df %>% 
  select_if(is.character) %>% 
  map(.x = names(select(., -over_50k)), .f = ~tabyl(df, !!sym(.x), over_50k) %>%  adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns())





df %>% 
  select_if(is.character) %>% 
  ggplot(aes(fill = over_50k, x = workclass)) +
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()


