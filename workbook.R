rm(list = ls())
graphics.off()


library(tidyverse)

library(janitor)

library(tidymodels)

library(skimr)


library(vip)


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
  select(-starts_with("capital"), -education) %>% 
  distinct(id, .keep_all = T) 


skim(df_cleaned)



df_cleaned %>% 
  select_if(is.character) %>% 
  map(.x = names(select(., -over_50k)), .f = ~tabyl(df_cleaned, !!sym(.x), over_50k) %>%  adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns())




# 
# 
# glimpse(df)
# 
# df %>% 
#   ggplot(aes(x = over_50k, y = age)) +
#   geom_boxplot()
# 
# 
# 
# df %>% 
#   ggplot(aes(x = over_50k, y = education_num)) +
#   geom_boxplot()
# 
# 
# 
# 
# 
# df %>% 
#   ggplot(aes(fill = over_50k, x = workclass)) +
#   geom_bar(position = "dodge")+
#   # or:
#   # geom_bar(position = position_fill(), stat = "identity") 
#   #scale_y_continuous(labels = scales::percent_format()) +
#   theme_classic()
# 
# 
# df %>% 
#   ggplot(aes(x = education_num)) +
#   geom_histogram()
# 
# df %>% 
#   filter(abs(capital_gain) < 5000) %>% 
#   ggplot(aes(x = capital_gain)) +
#   geom_histogram()
# 
# 
# df %>% 
#   filter(abs(capital_loss) < 5000) %>% 
#   ggplot(aes(x = capital_loss)) +
#   geom_histogram()
# 
# 
# summary(df %>% select_if(is.numeric))
# 
# 
# 
# df %>% 
#   select_if(is.character) %>% 
#   lapply(X = ., FUN = function(x) table(x))
# 
# 
# df %>% 
#   select_if(is.numeric) %>% 
#   lapply(X = ., FUN = function(x) sum(is.na(x)))
# 
# 
# 
# df %>% 
#   ggplot(aes(x = hours_per_week)) +
#   geom_histogram()
# 
# 
# df %>% 
#   ggplot(aes(x = over_50k, y = hours_per_week)) +
#   geom_violin()
# 
# 
# df %>% 
#   select_if(is.character) %>% 
#   map(.x = names(select(., -over_50k)), .f = ~tabyl(df, !!sym(.x), over_50k) %>%  adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns())
# 
# 
# 
# 
# 
# df %>% 
#   select_if(is.character) %>% 
#   ggplot(aes(fill = over_50k, x = workclass)) +
#   geom_bar(position = "fill")+
#   scale_y_continuous(labels = scales::percent_format()) +
#   theme_classic()
# 



###########################################
# # # # # # # # # # # # # # # # # # # # # #
############ Modeling Begin Here ##########
# # # # # # # # # # # # # # # # # # # # # #
###########################################


set.seed(seed = 2020) 


# Splitting dataset in training and test set

train_test_split <-
  rsample::initial_split(
    data = df_cleaned,
    strata = over_50k,
    prop = 0.80   
  ) 


train_test_split


trees_train <- train_test_split %>% training() 

trees_test  <- train_test_split %>% testing()


# Function to Transforming variables for modeling


tree_rec <- recipe(over_50k ~ ., data = trees_train) %>%
  update_role(id, new_role = "ID") %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_string2factor(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_knnimpute(all_predictors(), neighbors = 3)


# Prepping data

tree_prep <- prep(tree_rec)

# Juice the recipe

# train_baked <- bake(tree_rec, new_data = train_tbl)
# 
# test_baked  <- bake(tree_rec, new_data = test_tbl)


# Function for cross validation and tuning


# rf_fun <- function(split, id, try, tree) {
#   
#   analysis_set <- split %>% analysis()
#   analysis_prepped <- analysis_set %>% recipe_simple()
#   analysis_baked <- analysis_prepped %>% bake(new_data = analysis_set)  
#   model_rf <- rand_forest(
#       mode = "classification",
#       mtry = try,
#       trees = tree
#     ) %>%
#     set_engine("ranger",
#                importance = "impurity"
#     ) %>%
#     fit(over_50k ~ ., data = analysis_baked)  
#   
#   assessment_set <- split %>% assessment()
#   assessment_prepped <- assessment_set %>% recipe_simple()
#   assessment_baked <- assessment_prepped %>% bake(new_data = assessment_set)  
#   
#   tibble(
#     "id" = id,
#     "truth" = assessment_baked$over_50k,
#     "prediction" = model_rf %>% predict(new_data = assessment_baked) %>% unlist()
#   )
#   
# }
# 
# 
# # Performance assessment
# 
# pred_rf <- map2_df(
#   .x = cross_val_tbl$splits,
#   .y = cross_val_tbl$id,
#   ~ rf_fun(split = .x, id = .y, try = 3, tree = 200)
# )
# 


# model specification for a random forest

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 200,
  min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# put these together in a workflow(), 

tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)






# Setting up cross validation 

trees_folds <- vfold_cv(trees_train, v = 10)

all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)

cl <- makePSOCKcluster(all_cores-1)

doParallel::registerDoParallel(cl)

tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")




# Regular grid search

rf_grid <- grid_regular(
  mtry(range = c(6, 15)),
  min_n(range = c(30, 45)),
  levels = 5
)

rf_grid

tail(rf_grid)



regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

regular_res


regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")




# Choosing best model


best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf


# Variable importance plot


tree_fit <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(over_50k ~ .,
      data = juice(tree_prep) %>% select(-id)
  )


final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(over_50k ~ .,
      data = juice(tree_prep) %>% select(-id)
  ) %>%
  vip(geom = "point", num_features = 20)


final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(train_test_split)

final_res %>%
  collect_metrics()


write_csv(df_cleaned, "dataset-cleaned-final.csv")

prediction_df <- df_cleaned %>% 
  mutate(prediction = tree_fit %>% predict(new_data = bake(object = tree_prep, new_data = df_cleaned)) %>% unlist()) %>% 
  select(id, prediction, over_50k)

head(prediction_df)

tabyl(prediction_df, prediction, over_50k)


prediction_df %>% 
  select(-over_50k) %>% 
  write_csv("prediction-dataset.csv")
