library(tidyverse)
library(ggmap)
library(stringr)
library(xts)

## load data ----

## load coffee shop transactional data
coffee_shop_trans <- read_csv("data/Data_Science_Candidate_Analysis_Project.csv")
## load census data
census <- read_csv("data/2011Census_B01_AUST_POA.csv")
## load post code coordinate data
postcode_lat_lon <- read_csv("data/Australian_Post_Codes_Lat_Lon.csv")
## simplify coordinate data for table joining purpose
postcode_lat_lon_simple <- postcode_lat_lon %>% select(postcode, lat, lon) %>% group_by(postcode) %>% filter(1 == row_number()) %>% ungroup()

## insight 1 ----

## aggregate coffee shop transaction
coffee_shop_trans_by_postcode <- coffee_shop_trans %>%
  group_by(Customer_Postal_code) %>%
  summarise(
    total_order_count = n(),
    total_order_value = sum(Order_value_AUD))

coffee_shop_trans_by_postcode %>% arrange(desc(total_order_value)) %>% head(10)


## Assume the coffee per cup price is stable, then total_order_value is proportional to the co
coffee_shop_trans_by_postcode_census <- coffee_shop_trans_by_postcode %>%
  left_join(census, by = c("Customer_Postal_code" = "region_id")) %>%
  left_join(postcode_lat_lon_simple, by = c("Customer_Postal_code" = "postcode"))

## suppose the target coffee drinkers age from 15 to 74 years old
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
## maps show the popularity of coffee shops at different post code
## the bigger the radius, the more popular the coffee shop is
map <- get_map(location = "Australia", zoom = 4)
ggmap(map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight,
                        aes(x = lon, y = lat, size = coffee_drinker_log_weight), col = "red", alpha = 0.2)
sydney_map <- get_map(location = "Sydney", zoom = 10)
ggmap(sydney_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight %>% head(100),
                               aes(x = lon, y = lat, size = coffee_drinker_log_weight), col = "red", alpha = 0.5)
melbourne_map <- get_map(location = "Melbourne", zoom = 10)
ggmap(melbourne_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight,
                               aes(x = lon, y = lat, size = coffee_drinker_log_weight ), col = "red", alpha = 0.5)
perth_map <- get_map(location = "Perth", zoom = 10)
ggmap(perth_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight,
                                  aes(x = lon, y = lat, size = coffee_drinker_log_weight ), col = "red", alpha = 0.5)
brisbane_map <- get_map(location = "Brisbane", zoom = 10)
ggmap(brisbane_map) + geom_point(data = coffee_shop_trans_by_postcode_census_log_weight,
                              aes(x = lon, y = lat, size = coffee_drinker_log_weight), col = "red", alpha = 0.5)
coffee_shop_trans_by_postcode_census_log_weight %>% arrange(desc(coffee_drinker_log_weight)) %>% head()
## Coffee shops are most welcome at post codes include 3062, 6106, 2129, 6090, 3026
coffee_shop_trans_by_postcode_census_log_weight %>% arrange(coffee_drinker_log_weight) %>% head()
## At post codes including 5089, 3184, 2090, 6023 etc where coffee shops are not popular, it's necessary to evaluate
## why it was not widely accepted, and necessary to close the shop
## If senior management foresee the potential of these post codes, marketing strategies e.g. discounts, promotions would 
## be useful



## Insight 2 ---- 
## Time series for transaction by state
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

## coffee shop transaction in NSW and ACT
coffee_shop_trans_nsw_act <- coffee_shop_trans_by_state %>% filter(state == "NSW & ACT")
coffee_shop_trans_nsw_act_ts <- xts(coffee_shop_trans_nsw_act$total_order_value_AUD, order.by = coffee_shop_trans_nsw_act$Order_date, frequency = 7)
coffee_shop_trans_nsw_act_ts_month <- apply.monthly(coffee_shop_trans_nsw_act_ts,FUN=sum)
plot(coffee_shop_trans_nsw_act_ts_month, main = "NSW ACT total value")
## coffee shop transaction in VIC
coffee_shop_trans_vic <- coffee_shop_trans_by_state %>% filter(state == "VIC")
coffee_shop_trans_vic_ts <- xts(coffee_shop_trans_vic$total_order_value_AUD, order.by = coffee_shop_trans_vic$Order_date, frequency = 7)
coffee_shop_trans_vic_ts_month <- apply.monthly(coffee_shop_trans_vic_ts,FUN=sum)
plot(coffee_shop_trans_vic_ts_month, main = "VIC total value")
## coffee shop transaction in QLD
coffee_shop_trans_qld <- coffee_shop_trans_by_state %>% filter(state == "QLD")
coffee_shop_trans_qld_ts <- xts(coffee_shop_trans_qld$total_order_value_AUD, order.by = coffee_shop_trans_qld$Order_date, frequency = 7)
coffee_shop_trans_qld_ts_month <- apply.monthly(coffee_shop_trans_qld_ts,FUN=sum)
plot(coffee_shop_trans_qld_ts_month, main = "QLD total value")
## coffee shop transaction in WA
coffee_shop_trans_wa <- coffee_shop_trans_by_state %>% filter(state == "WA")
coffee_shop_trans_wa_ts <- xts(coffee_shop_trans_wa$total_order_value_AUD, order.by = coffee_shop_trans_wa$Order_date, frequency = 7)
coffee_shop_trans_wa_ts_month <- apply.monthly(coffee_shop_trans_wa_ts,FUN=sum)
plot(coffee_shop_trans_qld_ts_month, main = "WA total value")

## Comments
## All four time series plot shows the similar pattern
## 1. plot shows the downward trend, shows the necessary to initiate a survey to the customers to understand the reason
## Are there new competitors, or coffee quality is not as good as it used to be?
## 2. peaks are usually around mid-year, while valleys are usually at year end
## Bring more promotion to customers at year end around Christmas would increas sale
## 3. plot shows seasonality in the plot, thus 
## 3.1 ets() can be applied directly to the plot to forecast 
## 3.2 When applying ARIMA models, seanality should be included
## to 


