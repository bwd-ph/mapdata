library(tidyverse)
library(sqldf)
library(stringr)
library(readxl)
library(readr)
library(curl)

# Read in House of Commons Library MSOA names

# Method of downloading and opening .csv file taken from Colin Angus's example at https://github.com/VictimOfMaths/Experiments/blob/master/COVIDLAHeatmap.R

temp <- tempfile()
source <- "https://visual.parliament.uk/msoanames/static/MSOA-Names-v1.1.0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
msoanames <- read.csv(temp) %>% rename(polycode = 1) %>% # 1st column name is corrupted
select(polycode,msoa11hclnm)

##################################################################
# Child obesity data (NCMP) 
##################################################################
# Originally written when the MSOA-level NCMP data up to 2014/15-2016/17 was available at 
# https://www.gov.uk/government/statistics/child-obesity-and-excess-weight-small-area-level-data
# but not yet on LocalHealth

# Amended 13/6/19 when the MSOA-level NCMP data was available up to 2015/16-2017/18
# (Was also by then available on LocalHealth, but didn't see the point of making fundamental changes)

# Updated again 21/6/20 to reflect the fact that the only downloadable file now seems to be the one available from Fingertips



setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column
metadata <- read_csv("Metadata.csv")

# Reading NCMP data. NB - went to 'Download' tab of the NCMP profile, and downloaded the 'Data for MSOA' file 
# under the heading 'Domain: NCMP small area data'

PennineNCMP <- read.csv("NCMP_data_MSOA_update_2020.csv", 
  colClasses = c("numeric","NULL","NULL","character","character","NULL","character",
     "NULL","NULL","NULL","NULL","NULL","numeric","numeric","numeric",
     "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","numeric","NULL","NULL")) %>% 
  filter(`Area.Type` == "MSOA") %>%
  filter(`Time.period.Sortable` == max(`Time.period.Sortable`)) %>% # only want the latest data
  mutate(IndID = case_when(
    Indicator.ID == 93105 ~ "Obese_Reception",
    Indicator.ID == 93106 ~ "Excess_Reception",
    Indicator.ID == 93107 ~ "Obese_Year6",
    Indicator.ID == 93108 ~ "Excess_Year6",
    TRUE ~ "" # shouldn't occur
  )) %>%
 filter(`Parent.Name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%  
 select(IndID,polycode =`Area.Code`,value = Value,LCI = `Lower.CI.95.0.limit`,UCI = `Upper.CI.95.0.limit`)   

# Attach House of Commons MSOA names
PennineNCMP <- PennineNCMP %>% left_join(msoanames,by = "polycode")
# Append the England average (stored in metadata as 'DivergePoint') for comparison
PennineNCMP <- PennineNCMP %>% left_join(metadata,by = "IndID") %>%
  select(IndID,polycode,value,LCI,UCI,England=DivergePoint,msoa11hclnm) %>%
  mutate(label = paste0("MSOA: ",polycode,"<br/>",
                        "(aka ",sQuote(msoa11hclnm),")<br/>",
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

income <- read_csv("https://www.ons.gov.uk/file?uri=%2femploymentandlabourmarket%2fpeopleinwork%2fearningsandworkinghours%2fdatasets%2fsmallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales%2ffinancialyearending2018/totalannualincome2018.csv",skip=5,n_max=7201,
                   col_names = c("MSOA code","MSOA name","LA code","LA name","Region code","Region name",
                                 "TotIncome","X8","X9","X10"))
income <- income %>%
  select(`MSOA code`,`MSOA name`,`LA code`,`LA name`,`Region code`,`Region name`,`TotIncome`) %>%
  filter(`Region name` != "Wales")

income$rank <- as.integer(floor(rank(as.numeric(income$TotIncome))))
income$decile <- pmin(((income$rank-1)%/%679)+1,10)
income$decile <- factor(income$decile)
PennineIncome <- income %>% filter(`LA name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>% add_column(IndID = "Tot_annual_income")

# Attach House of Commons MSOA names
PennineIncome <- PennineIncome %>% left_join(msoanames,by = c("MSOA code" = "polycode"))

PennineIncome <- PennineIncome %>%
  mutate(label = paste0("MSOA: ", `MSOA code`,"<br/>",
                        "(aka ",sQuote(msoa11hclnm),")<br/>",
                        "Household Income: £",TotIncome,"<br>",
                        "Rank: ",rank," (out of 6791)","<br>",
                        "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = lowest income)")) %>%
  select(IndID,polycode =`MSOA code`,value = decile,label)

# write_excel_csv(PennineIncome,'Income_MSOA.csv')

###############################################
# Now reading the ONS MSOA-level house prices #
###############################################
options(scipen = 999) # don't want scientific notation
HousePrices <- read_excel("HPSSA Dataset 2 - Median price paid by MSOA.xls", sheet = '1a', range = "A6:CV7207", na = ":") %>% filter(`Local authority name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) 
latestyear <- tail(names(HousePrices),1) # i.e. name of last column, something like 'Year ending Mar 2019'
HousePrices <- HousePrices %>%
  select(polycode = `MSOA code`,value = tail(names(.), 1)) %>% # i.e. take value from last column
  add_column(IndID = "House_Prices_MSOA") %>%
  left_join(metadata, by = "IndID") %>%
  select(IndID,polycode,value,England=DivergePoint) %>%
  left_join(msoanames,by = "polycode") %>% # Attach House of Commons MSOA names
  mutate(label = paste0("MSOA: ",polycode,"<br/>",
                        "(aka ",sQuote(msoa11hclnm),")<br/>",
                        "Median house price for","<br/>",latestyear,": ",
                        ifelse(is.na(value),"<br/>N/A (fewer than 5 sales)",
                               paste0("£",prettyNum(value,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE))),"<br/>",
                        "(England average = £",prettyNum(England*1000,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE),")")) %>%
  mutate(value = round(value/1000)) %>%
  select(IndID,polycode,value,label)

#######################################################
# Write all the above to single PennineOther_MSOA.csv file
#######################################################

PennineOther <- rbind(PennineNCMP,PennineIncome, HousePrices)
write_excel_csv(PennineOther,'PennineOther_MSOA.csv')
