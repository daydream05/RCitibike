library(readr)
library(dplyr)
library(lubridate)

file = "green_tripdata_2016-01.csv"

taxi <- read_csv(file)

dim(taxi)

taxi <- taxi %>% mutate_each(funs(as.factor),
                             VendorID,
                             RateCodeID,
                             Store_and_fwd_flag,
                             Payment_type)

taxi <- taxi %>% filter(RateCodeID != 99)


trips_per_ratecode <- taxi %>% group_by(RateCodeID) %>% 
  summarise(total = n()) %>% 
  mutate(proportion = total/sum(total))

trips_per_passenger_count <- taxi %>% group_by(Passenger_count) %>% 
  summarise(total = n()) %>% 
  mutate(proportion = total/sum(total))

trips_per_payment_type <- taxi %>% group_by(Payment_type) %>% 
  summarise(total = n()) %>% 
  mutate(proportion = total/sum(total))

negative_fare_amount <- taxi %>% filter(Fare_amount <= 0) %>% 
  summarise(total = n()) %>% 
  mutate(proportion = total/nrow(taxi))

positive_fare_amount <- taxi %>% filter(Fare_amount > 0,
                                        Passenger_count > 0) %>% 
  mutate(percent_tip = round(100*Tip_amount/Fare_amount, 2)) %>% 
  summarise(minimum_tip = min(minimum_tip),
            maximum_tip = mean(minimum_tip),
            average_tip = average(average_tip))
