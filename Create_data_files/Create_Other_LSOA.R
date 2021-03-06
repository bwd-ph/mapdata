library(tidyverse)
library(readxl)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column

#  Read metadata
metadata <- read_csv("Metadata.csv")

################ Fuel Poverty ##################
PennineFuelPov <- read_excel("Fuel-poverty-sub-regional-tables-2020-2018-data.xlsx",sheet="Table 3",range="A3:H32847",col_names = TRUE) %>% filter(`LA Name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
add_column(IndID = "Fuel_Poverty")

# Append the England average (stored in metadata as 'DivergePoint') for comparison
PennineFuelPov <- PennineFuelPov %>% left_join(metadata,by = "IndID") %>%
  select(IndID,polycode = `LSOA Code`,value = `Proportion of households fuel poor (%)`,FPhhlds =`Number of households in fuel poverty1`,England=DivergePoint) %>%
  mutate(label = paste0("LSOA: ",polycode,"<br/>",
                        FPhhlds," households in this LSOA are fuel poor<br/>",
                        "which is ",paste0(round(value,digits=1),"% of all households<br/>"),
                        "(England average = ",paste0(round(England,digits=1),"%)")))  %>%
  select(IndID,polycode,value,label)
# write_csv(PennineFuelPov,"PennineOther_LSOA.csv")

################# House Prices #################
options(scipen = 999) # don't want scientific notation
HousePrices <- read_excel("hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.xls", sheet = 'Data', range = "A6:CV34759", na = ":") %>% filter(`Local authority name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) 
latestyear <- tail(names(HousePrices),1) # i.e. name of last column, something like 'Year ending Mar 2019'
HousePrices <- HousePrices %>%
  select(polycode = `LSOA code`,value = tail(names(.), 1)) %>% # i.e. take value from last column
  add_column(IndID = "House_Prices_LSOA") %>%
  left_join(metadata, by = "IndID") %>%
  select(IndID,polycode,value,England=DivergePoint) %>%
  mutate(label = paste0("LSOA: ",polycode,"<br/>",
                        "Median house price for","<br/>",latestyear,": ",
                        ifelse(is.na(value),"<br/>N/A (fewer than 5 sales)",
                               paste0("�",prettyNum(value,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE))),"<br/>",
                        "(England average = �",prettyNum(England*1000,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE),")")) %>%
  mutate(value = round(value/1000)) %>%
  select(IndID,polycode,value,label)

PennineOther <- bind_rows(PennineFuelPov,HousePrices)
write_csv(PennineOther,"PennineOther_LSOA.csv")
