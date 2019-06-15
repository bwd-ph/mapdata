library(tidyverse)
library(sqldf)
library(stringr)
library(readxl)
library(readr)

##################################################################
# Child obesity data (NCMP) 
##################################################################
# Originally written when the MSOA-level NCMP data up to 2014/15-2016/17 was available at 
# https://www.gov.uk/government/statistics/child-obesity-and-excess-weight-small-area-level-data
# but not yet on LocalHealth

# Amended 13/6/19 when the MSOA-level NCMP data was available up to 2015/16-2017/18
# (Was also by then available on LocalHealth, but didn't see the point of making fundamental changes)



setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column
metadata <- read_csv("Metadata.csv")

# Reading NCMP data. NB - there are more years (i.e. columns) of obesity data than of excess weight data

# NB - data now supplied as a .ods file, but read_ods is SLOW!!! 
# So have downloaded the file and then saved it in Excel format.

excessRecep <- read_excel("NCMP_data_MSOA_update_2019.xlsx",sheet=2,skip=2) %>% add_column(IndID = "Excess_Reception") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__5`,LCI = `LCI__5`,UCI = `UCI__5`)
obeseRecep <- read_excel("NCMP_data_MSOA_update_2019.xlsx",sheet=3,skip=2) %>% add_column(IndID = "Obese_Reception") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__7`,LCI = `LCI__7`,UCI = `UCI__7`)
excessYear6 <- read_excel("NCMP_data_MSOA_update_2019.xlsx",sheet=4,skip=2) %>% add_column(IndID = "Excess_Year6") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__5`,LCI = `LCI__5`,UCI = `UCI__5`)
obeseYear6 <- read_excel("NCMP_data_MSOA_update_2019.xlsx",sheet=5,skip=2) %>% add_column(IndID = "Obese_Year6") %>%
  select(IndID,`LA name`,polycode =`MSOA code`,value = `%__7`,LCI = `LCI__7`,UCI = `UCI__7`)
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

# write_csv(PennineNCMP,"PennineNCMP_MSOA.csv")

#################################################################################################
# Now reading the ONS MSOA-level modelled income estimates (Total Annual Household Income only) #
#################################################################################################

# NB - not using read.csv because it doesn't handle the commas in the amounts of money properly

income <- read_csv("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2016/1totalannualincome.csv",skip=3,n_max=7201,
                   col_names = c("MSOA code","MSOA name","LA code","LA name","Region code","Region name",
                                 "TotIncome","X8","X9","X10","X11","X12","X13","X14"))
income <- income %>%
  select(`MSOA code`,`MSOA name`,`LA code`,`LA name`,`Region code`,`Region name`,`TotIncome`) %>%
  filter(`Region name` != "Wales")

income$rank <- as.integer(floor(rank(as.numeric(income$TotIncome))))
income$decile <- pmin(((income$rank-1)%/%679)+1,10)
income$decile <- factor(income$decile)
PennineIncome <- income %>% filter(`LA name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>% add_column(IndID = "Tot_annual_income")

PennineIncome <- PennineIncome %>%
  mutate(label = paste0("MSOA: ", `MSOA code`,"<br/>",
                        "Household Income: £",TotIncome,"<br>",
                        "Rank: ",rank," (out of 6791)","<br>",
                        "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = lowest income)")) %>%
  select(IndID,polycode =`MSOA code`,value = decile,label)

# write_excel_csv(PennineIncome,'Income_MSOA.csv')

#######################################################
# Write all the above to single PennineOther_MSOA.csv file
#######################################################

PennineOther <- rbind(PennineNCMP,PennineIncome)
write_excel_csv(PennineOther,'PennineOther_MSOA.csv')
