library(caret)
library(tidyverse)
library(lubridate)

coffee_shop_trans <- read_csv("data/Data_Science_Candidate_Analysis_Project.csv")

## A simple RFM model is used to fit the model

## recency is the units of 30 days from June 26 2017

today <- ymd("2017-06-26")

coffee_shop_trans_rfm <- coffee_shop_trans %>% 
  mutate(recency = floor(as.numeric(today - Order_date) / 30)) %>%
  select(-Order_date, - Customer_Postal_code) %>%
  group_by(Customer_id, recency) %>%
  summarise(Order_value_AUD = sum(Order_value_AUD), frequency = n()) %>%
  ungroup() %>%
  group_by(Customer_id) %>%
  arrange(Customer_id, recency) %>%
  mutate(
    recency_diff = recency - lag(recency),
    buy = ifelse(recency_diff == 1, 1, 0),
    monetary = lag(Order_value_AUD)
  ) %>%
  select(-Order_value_AUD, -recency_diff) 

coffee_shop_trans_rfm_model <- coffee_shop_trans_rfm %>% filter(!is.na(buy))



## model 1: likelihood of user purchace within 30 days ----

model_classify <- glm(buy ~ recency + frequency,family=quasibinomial(link='logit'),data = coffee_shop_trans_rfm_model)

coffee_shop_trans_rfm_convert_0 <- coffee_shop_trans_rfm %>% filter(recency == 0)

pred_classify_convert_0 <- predict(model_classify, coffee_shop_trans_rfm_convert_0, type = "response")

pred_classify_convert_0_df <- data_frame(Customer_id = coffee_shop_trans_rfm_convert_0$Customer_id, buy_likelihood = as.numeric(pred_classify_convert_0))

pred_classify_no_convert_0 <- predict(model_classify, data_frame(recency = 0, frequency = 0), type = "response")

## model 2: likely value of the order ----
## fit a tree rather than a linear regression with cross validation for some fun and higher accuracy

controlObject <- trainControl(method = "repeatedcv",
                              repeats = 5,
                              number = 10)

model_regression <- train(monetary ~ recency + frequency,
                          data = coffee_shop_trans_rfm_model,
                          method = "rpart",
                          tuneLength = 30,
                          trControl = controlObject)
pred_regression_convert_0 <- predict(model_regression, coffee_shop_trans_rfm_convert_0)
pred_regression_convert_0_df <- data_frame(Customer_id = coffee_shop_trans_rfm_convert_0$Customer_id, 
                                         buy_likelihood = as.numeric(pred_regression_convert_0))

pred_regression_no_convert_0 <- predict(model_regression, data_frame(recency = 0, frequency = 0))

