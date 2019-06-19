library(tidyverse)
library(readxl)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column

#  Read metadata
metadata <- read_csv("Metadata.csv")

PennineFuelPov <- read_excel("Fuel_poverty_sub-regional_tables_2019.xlsx",sheet="Table 3",range="A3:H32847",col_names = TRUE) %>% filter(`LA Name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
add_column(IndID = "Fuel_Poverty")

# Append the England average (stored in metadata as 'DivergePoint') for comparison
PennineFuelPov <- PennineFuelPov %>% left_join(metadata,by = "IndID") %>%
  select(IndID,polycode = `LSOA Code`,value = `Proportion of households fuel poor (%)`,FPhhlds =`Estimated number of fuel poor households`,England=DivergePoint) %>%
  mutate(label = paste0("LSOA: ",polycode,"<br/>",
                        FPhhlds," households in this LSOA are fuel poor<br/>",
                        "which is ",paste0(round(value,digits=1),"% of all households<br/>"),
                        "(England average = ",paste0(round(England,digits=1),"%)")))  %>%
  select(IndID,polycode,value,label)
write_csv(PennineFuelPov,"PennineOther_LSOA.csv")


