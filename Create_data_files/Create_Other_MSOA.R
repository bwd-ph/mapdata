# Written when the MSOA-level NCMP data up to 2014/15-2016/17 was available at 
# https://www.gov.uk/government/statistics/child-obesity-and-excess-weight-small-area-level-data
# but not yet on LocalHealth

library(tidyverse)
library(sqldf)
library(stringr)
library(readxl)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column
metadata <- read_csv("Metadata.csv")

# Reading NCMP data. NB - there are more years of obesity data than of excess weight data (goes up to column AM rather than column AC)
excessRecep <- read_excel("NCMP_data_MSOA_update_2018.xlsx",sheet=2,range = "A3:AC6794") %>% add_column(IndID = "Excess_Reception") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__4`,LCI = `LCI__4`,UCI = `UCI__4`)
obeseRecep <- read_excel("NCMP_data_MSOA_update_2018.xlsx",sheet=3,range = "A3:AM6794") %>% add_column(IndID = "Obese_Reception") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__6`,LCI = `LCI__6`,UCI = `UCI__6`)
excessYear6 <- read_excel("NCMP_data_MSOA_update_2018.xlsx",sheet=4,range = "A3:AC6794") %>% add_column(IndID = "Excess_Year6") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__4`,LCI = `LCI__4`,UCI = `UCI__4`)
obeseYear6 <- read_excel("NCMP_data_MSOA_update_2018.xlsx",sheet=5,range = "A3:AM6794") %>% add_column(IndID = "Obese_Year6") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__6`,LCI = `LCI__6`,UCI = `UCI__6`)
NCMP <- bind_rows(excessRecep,obeseRecep,excessYear6,obeseYear6) 
PennineNCMP <- NCMP %>% filter(`LA name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
  mutate(value = as.numeric(value),LCI = as.numeric(LCI),UCI = as.numeric(UCI))

# Append the England average (stored in metadata as 'DivergePoint') for comparison
PennineNCMP <- PennineNCMP %>% left_join(metadata,by = "IndID") %>%
  select(IndID,polycode,value,LCI,UCI,England=DivergePoint) %>%
  mutate(label = paste0("MSOA: ",polycode,"<br/>",
                        ifelse(is.na(value),"",
                        ifelse(IndID == "Excess_Reception","Prevalence of excess weight in Reception = ",
                        ifelse(IndID == "Obese_Reception","Prevalence of obesity in Reception = ",
                        ifelse(IndID == "Excess_Year6","Prevalence of excess weight in Year 6 = ","Prevalence of obesity in Year 6 = "                          )))),
                        ifelse(is.na(value),"No data for this area",paste0(round(value,digits=1),"%")),"<br/>",
                        ifelse(LCI > England,paste0("Worse than England (= ",round(England,digits=1),"%)"),
                        ifelse(UCI < England,paste0("Better than England (= ",round(England,digits=1),"%)"),
                        ifelse(!is.na(LCI) & !is.na(UCI),paste0("Similar to England (= ",round(England,digits=1),"%)"),""))))) %>%
  select(IndID,polycode,value,label)

write_csv(PennineNCMP,"PennineNCMP_MSOA.csv")

