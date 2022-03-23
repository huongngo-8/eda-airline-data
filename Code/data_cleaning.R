# Loading in libraries
library(tidyverse)
library(mice)

# Read in data
profit_2019 <- read_csv("~/Desktop/SPA DRP - Research/eda-airline-data/Data/profit_2019.csv")
profit_2020 <- read_csv("~/Desktop/SPA DRP - Research/eda-airline-data/Data/profit_2020.csv")
profit_2021 <- read_csv("~/Desktop/SPA DRP - Research/eda-airline-data/Data/profit_2021.csv")

traffic_capacity_2019 <- read_csv("~/Desktop/SPA DRP - Research/eda-airline-data/Data/traffic_capacity_2019.csv")
traffic_capacity_2020 <- read_csv("~/Desktop/SPA DRP - Research/eda-airline-data/Data/traffic_capacity_2020.csv")
traffic_capacity_2021 <- read_csv("~/Desktop/SPA DRP - Research/eda-airline-data/Data/traffic_capacity_2021.csv")

# Excluding charter and cargo air carriers
carrier_names_profit <- profit_2019 %>% 
  distinct(UNIQUE_CARRIER_NAME) %>%
  filter(str_detect(UNIQUE_CARRIER_NAME, "Cargo") == FALSE, 
         str_detect(UNIQUE_CARRIER_NAME, "LLC") == FALSE, 
         str_detect(UNIQUE_CARRIER_NAME, "Charter") == FALSE, 
         str_detect(UNIQUE_CARRIER_NAME, "Corporation") == FALSE)

carrier_names <- carrier_names_profit %>% 
  filter(!(UNIQUE_CARRIER_NAME %in% c("Federal Express Corporation", 
                                      "United Parcel Service",
                                      "Western Global",
                                      "Air Transport International",
                                      "Jet Aviation Flight Services, Inc.",
                                      "Atlas Air Inc.",
                                      "Asia Pacific",
                                      "Amerijet International",
                                      "Miami Air International",
                                      "USA Jet Airlines Inc.",
                                      "Southern Air Inc.",
                                      "ABX Air Inc")))

profit_2019 <- profit_2019 %>% 
  filter(UNIQUE_CARRIER_NAME %in% c(carrier_names$UNIQUE_CARRIER_NAME))

profit_2020 <- profit_2020 %>% 
  filter(UNIQUE_CARRIER_NAME %in% c(carrier_names$UNIQUE_CARRIER_NAME))

profit_2021 <- profit_2021 %>% 
  filter(UNIQUE_CARRIER_NAME %in% c(carrier_names$UNIQUE_CARRIER_NAME))

traffic_capacity_2019 <- traffic_capacity_2019 %>% 
  filter(UNIQUE_CARRIER_NAME %in% c(carrier_names$UNIQUE_CARRIER_NAME))

traffic_capacity_2020 <- traffic_capacity_2020 %>% 
  filter(UNIQUE_CARRIER_NAME %in% c(carrier_names$UNIQUE_CARRIER_NAME))

traffic_capacity_2021 <- traffic_capacity_2021 %>% 
  filter(UNIQUE_CARRIER_NAME %in% c(carrier_names$UNIQUE_CARRIER_NAME))

# Remove ...26 and ...27 in traffic_capacity and profit tables respectively
profit_2019 <- profit_2019[1:26]
profit_2020 <- profit_2020[1:26]
profit_2021 <- profit_2021[1:26]

traffic_capacity_2019 <- traffic_capacity_2019[1:25]
traffic_capacity_2020 <- traffic_capacity_2020[1:25]
traffic_capacity_2021 <- traffic_capacity_2021[1:25]

# Binding same datasets over time by rows 
profit_data <- bind_rows(profit_2019, 
                         profit_2020, 
                         profit_2021)
traffic_capacity_data <- bind_rows(traffic_capacity_2019, 
                                   traffic_capacity_2020, 
                                   traffic_capacity_2021)
traffic_capacity_data <- traffic_capacity_data[, -13]
traffic_capacity_data <- traffic_capacity_data[, -20]

# Looking at missing variables by column (variable) for profit_data
colMeans(is.na(profit_data))

# Looking at missing variables by column (variable) for traffic_capacity_data
colMeans(is.na(traffic_capacity_data))

# Impute other variables using mice in profit_data
mice_res_profit <- mice(profit_data, method = "cart")
clean_profit_data <- complete(mice_res_profit, 1)

# Impute other variables using mice in traffic_capacity_data
mice_res_tc <- mice(traffic_capacity_data, method = "cart")
clean_tc_data <- complete(mice_res_tc, 1)

# Lower casing column names in profit_data and traffic_capacity_data
colnames(clean_profit_data) <- tolower(colnames(clean_profit_data))
colnames(clean_tc_data) <- tolower(colnames(clean_tc_data))

write_csv(clean_profit_data, "~/Desktop/SPA DRP - Research/eda-airline-data/Data/profit_data.csv")
write_csv(clean_tc_data, "~/Desktop/SPA DRP - Research/eda-airline-data/Data/traffic_capacity_data.csv")



