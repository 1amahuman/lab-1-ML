# loading libraries
library(tidyverse)
library(tidymodels)
library(janitor)

#loading the data
stdnts <- read_csv("studentInfo.csv")
stdnts <- stdnts %>%
  mutate(pss = ifelse(final_result == "Pass", 1, 0)) %>%
  mutate(pss = as.factor(pss))

stdnts <- stdnts %>%
  mutate(dsb = as.factor(disability))

#checking the data
stdnts

stdnts <- stdnts %>%
  mutate(imb = factor(imd_band, levels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))) %>%
  mutate(imb = as.integer(imb))

set.seed(20230712)
train_test_splt <- initial_split(stdnts, prop = 0.80)
data_train <- training(train_test_splt)
data_test <- testing(train_test_splt)

# creating a recipe and model
my_rcp <- recipe(pss ~ dsb + imb, data = data_train)
my_mdl <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

#adding model and recipe to workflow
my_wf <- 
  workflow() %>% 
  add_model(my_mdl) %>% 
  add_recipe(my_rcp)

fttd_model <- fit(my_wf, data = data_train)
tst_splt <- rsample::initial_split(data_test, prop = 0.8)

# creating final fit 
final_ft <- last_fit(my_wf, split = tst_splt)
final_ft
final_ft %>%
  collect_predictions()
final_ft %>%
  collect_predictions() %>%
  select(.pred_class, pss) %>%
  mutate(correct = .pred_class == pss) %>%
  tabyl(correct)
