library(tidyverse)
library(ggmap)
library(stringr)
library(xts)


coffee_shop_trans <- read_csv("data/Data_Science_Candidate_Analysis_Project.csv")

census <- read_csv("data/2011Census_B01_AUST_POA.csv")

postcode_lat_lon <- read_csv("data/Australian_Post_Codes_Lat_Lon.csv")

postcode_lat_lon_simple <- postcode_lat_lon %>% select(postcode, lat, lon) %>% group_by(postcode) %>% filter(1 == row_number()) %>% ungroup()

coffee_shop_trans_by_postcode <- coffee_shop_trans %>%
  group_by(Customer_Postal_code) %>%
  summarise(
    total_order_count = n(),
    total_order_value = sum(Order_value_AUD))

coffee_shop_trans_by_postcode %>% arrange(desc(total_order_value)) %>% head(10)


# Assume the coffee per cup price is stable, then total_order_value is proportional to the co
coffee_shop_trans_by_postcode_census <- coffee_shop_trans_by_postcode %>%
  left_join(census, by = c("Customer_Postal_code" = "region_id")) %>%
  left_join(postcode_lat_lon_simple, by = c("Customer_Postal_code" = "postcode"))




  mutate(state = ifelse((Customer_Postal_code >= 2000 & Customer_Postal_code < 2600) |
                          (Customer_Postal_code >= 2620 & Customer_Postal_code < 2900) |
                          (Customer_Postal_code >= 2921 & Customer_Postal_code < 3000)))

# suppose the target coffee drinker ages from 15 to 74 years old
coffee_shop_trans_by_postcode_census_log_weight <-
coffee_shop_trans_by_postcode_census %>%
  group_by_(.dots = names(coffee_shop_trans_by_postcode_census)) %>%
  mutate(coffee_drinker_P = sum(Age_15_19_yr_P,
                                Age_20_24_yr_P,
                                Age_25_34_yr_P,
                                Age_35_44_yr_P,
                                Age_45_54_yr_P,
                                Age_55_64_yr_P,
                                Age_65_74_yr_P)) %>%
  ungroup() %>%
  filter(coffee_drinker_P != 0) %>%
  mutate(coffee_drinker_log_weight = log(total_order_value / coffee_drinker_P)) %>%
  arrange(desc(coffee_drinker_log_weight))

map <- get_map(location = "Australia", zoom = 4)
ggmap(map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight %>% head(100) ,
                        aes(x = lon, y = lat, size = coffee_drinker_log_weight, alpha = 0.2))
sydney_map <- get_map(location = "Sydney", zoom = 10)
ggmap(sydney_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight %>% head(100),
                               aes(x = lon, y = lat, size = coffee_drinker_log_weight ), alpha = 0.5)
melbourne_map <- get_map(location = "Melbourne", zoom = 10)
ggmap(melbourne_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight %>% head(100),
                               aes(x = lon, y = lat, size = coffee_drinker_log_weight ), alpha = 0.5)
perth_map <- get_map(location = "Perth", zoom = 10)
ggmap(perth_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight %>% head(100),
                                  aes(x = lon, y = lat, size = coffee_drinker_log_weight ), alpha = 0.5)
brisbane_map <- get_map(location = "Brisbane", zoom = 10)
ggmap(brisbane_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight %>% head(100),
                              aes(x = lon, y = lat, size = coffee_drinker_log_weight, col ), alpha = 0.5)



coffee_shop_trans_by_state <- coffee_shop_trans %>%
  mutate(
    first_digi = as.numeric(str_sub(Customer_Postal_code, 1, 1)),
    state = ifelse(first_digi == 2,
                   "NSW & ACT",
                   ifelse(first_digi == 3,
                          "VIC",
                          ifelse(first_digi == 4,
                                 "QLD",
                                 ifelse(first_digi == 5, "SA",
                                        ifelse(first_digi == 6, "WA",
                                               ifelse(first_digi == 7, "TAS", NA))))))
  ) %>% group_by(state, Order_date) %>%
  summarize(total_order_value_AUD = sum(Order_value_AUD, na.rm = TRUE))

state_list <- c("NSW & ACT", "VIC", "QLD", "SA", "WA", "TAS")

state_list %>% map(
  function(x) {
    coffee_shop_trans_state <- coffee_shop_trans_by_state %>% filter(state == x);
    coffee_shop_trans_state_ts <- xts(coffee_shop_trans_state$total_order_value_AUD, order.by = coffee_shop_trans_state$Order_date, frequency = 7)
    coffee_shop_trans_state_ts_month <- apply.monthly(coffee_shop_trans_state_ts,FUN=sum)
    plot(coffee_shop_trans_state_ts_month, title = x)
  })


coffee_shop_trans_nsw_act <- coffee_shop_trans_by_state %>% filter(state == "NSW & ACT")
coffee_shop_trans_nsw_act_ts <- xts(coffee_shop_trans_nsw_act$total_order_value_AUD, order.by = coffee_shop_trans_nsw_act$Order_date, frequency = 7)
coffee_shop_trans_nsw_act_ts_month <- apply.monthly(coffee_shop_trans_nsw_act_ts,FUN=sum)
plot(coffee_shop_trans_nsw_act_ts_month)

coffee_shop_trans_vic <- coffee_shop_trans_by_state %>% filter(state == "VIC")
coffee_shop_trans_vic_ts <- xts(coffee_shop_trans_vic$total_order_value_AUD, order.by = coffee_shop_trans_vic$Order_date, frequency = 7)
coffee_shop_trans_vic_ts_month <- apply.monthly(coffee_shop_trans_vic_ts,FUN=sum)
plot(coffee_shop_trans_vic_ts_month)

coffee_shop_trans_qld <- coffee_shop_trans_by_state %>% filter(state == "QLD")
coffee_shop_trans_qld_ts <- xts(coffee_shop_trans_qld$total_order_value_AUD, order.by = coffee_shop_trans_qld$Order_date, frequency = 7)
coffee_shop_trans_qld_ts_month <- apply.monthly(coffee_shop_trans_qld_ts,FUN=sum)
plot(coffee_shop_trans_qld_ts_month)

coffee_shop_trans_wa <- coffee_shop_trans_by_state %>% filter(state == "WA")
coffee_shop_trans_wa_ts <- xts(coffee_shop_trans_wa$total_order_value_AUD, order.by = coffee_shop_trans_wa$Order_date, frequency = 7)
coffee_shop_trans_wa_ts_month <- apply.monthly(coffee_shop_trans_wa_ts,FUN=sum)
plot(coffee_shop_trans_qld_ts_month)



# Target audience for coffee shop should usually be over 19 years old


