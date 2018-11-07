library(tidyverse)
library(sqldf)
library(stringr)
library(readxl)
library(data.table)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column

#  Read metadata
metadata <- read_csv("Metadata.csv")

# Read mid-2017 LSOA population estimates
url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimatesnationalstatistics/mid2017/sape20dt13mid2017lsoabroadagesestimatesunformatted.zip"
download.file(url, "pop2017.zip") # download file
unzip("pop2017.zip") # unzip files
pop <- read_xls("SAPE20DT13-mid-2017-lsoa-Broad_ages-estimates-unformatted.XLS",sheet = "Mid-2017 Persons", skip=4)
file.remove("pop2017.zip") # tidy up by removing the zip file

pop <- pop %>% filter(`Area Names` %like% "Blackburn with Darwen" |
                      `Area Names` %like% "Burnley" |
                      `Area Names` %like% "Hyndburn" |
                      `Area Names` %like% "Pendle" |
                      `Area Names` %like% "Ribble Valley" |
                      `Area Names` %like% "Rossendale") %>%
       mutate(`16-64` = `16-29` + `30-44` + `45-64`) %>%
       select(polycode = `Area Codes`, `16-64`)

# Reading Stat-Xplore data on ESA by condition
# ESA data for Feb 2018 was extracted from Stat-Xplore as two separate files - one for the five main conditions, and one for all conditions combined - and then merged in Excel 

# NB - the DWP is still using 2001 LSOAs. 

PennineESA <- read_csv("ESAbyCause2001LSOA_Feb2018.csv")

# We have two instances where two 2001 LSOAs merged to form one 2011 LSOA:

#  E01025043 + E01025047 -> E01033231 (in Hyndburn)
polycode <- c("E01033231")
NewHyndburnLSOA <- cbind(polycode,PennineESA %>% filter(`2001LSOA` %in% c("E01025043","E01025047")) %>% select(-`2001LSOA`) %>% summarise_all(funs(sum)))
PennineESA <- PennineESA %>% filter(!`2001LSOA` %in% c("E01025043","E01025047"))

#  E01025372 + E01025386 -> E01033281 (in Rossendale)
polycode <- c("E01033281")
NewRossendaleLSOA <-cbind(polycode,PennineESA %>% filter(`2001LSOA` %in% c("E01025372","E01025386")) %>% select(-`2001LSOA`) %>% summarise_all(funs(sum)))
PennineESA <- PennineESA %>% filter(!`2001LSOA` %in% c("E01025372","E01025386"))

# and also one instance (in BwD) where two old LSOAs (E01012627 + E01012626) are reconfigured into two new LSOAs (E01032485 + E01032486)
polycode <- c("BwDcorner")
NewBwDLSOAs <- cbind(polycode,PennineESA %>% filter(`2001LSOA` %in% c("E01012627","E01012626")) %>% select(-`2001LSOA`) %>% summarise_all(funs(sum)))
PennineESA <- PennineESA %>% filter(!`2001LSOA` %in% c("E01012627","E01012626"))

cornerpop <- cbind(polycode,pop %>% filter(polycode %in% c("E01032485","E01032486")) %>% select(-`polycode`) %>% summarise_all(funs(sum)))
pop <- pop %>% filter(!polycode %in% c("E01032485","E01032486"))

PennineESA <- PennineESA %>% rename(polycode = `2001LSOA`) %>% bind_rows(NewHyndburnLSOA,NewRossendaleLSOA,NewBwDLSOAs)
pop <- bind_rows(pop,cornerpop)

# Join with population denominator on polycode
PennineESA <- left_join(PennineESA,pop)

# Convert to long format
PennineESAlong <- PennineESA %>% rename(ESA_total = Total, ESA_MH = Mental, ESA_Nervous = Nervous,
                                        ESA_RespCirc = RespCirc,ESA_MSK = Musc,ESA_Other = Other) %>%
        gather(key = IndID, value = Count,ESA_total,ESA_MH,ESA_Nervous,ESA_RespCirc,ESA_MSK,ESA_Other) %>%
        mutate(value = Count/`16-64`*100, 
               label = paste0("LSOA: ",polycode,"<br/>",
              "In February 2018 there were<br/>",Count," people claiming ESA<br/>",
               ifelse(IndID == "ESA_MH","because of Mental Health disorders",
               ifelse(IndID == "ESA_Nervous","because of Nervous System disorders",
               ifelse(IndID == "ESA_RespCirc","because of Respiratory or Circulatory disorders",
               ifelse(IndID == "ESA_MSK","because of Musculoskeletal disorders",
               ifelse(IndID == "ESA_Other","because of other conditions","for any reason"))))),",<br/>which is approximately ",
               round(value,digits=1),"%<br/>of the local 16-64 population")) %>%
        select(IndID,polycode,value,label)

# Deal with SE corner of BwD
PennineESAlong <- PennineESAlong %>% mutate(label = ifelse(polycode == "BwDcorner",paste0("Boundary changes mean it is not<br/>possible to calculate ESA rates<br/>for the two LSOAs in the SE corner<br/>of Blackburn with Darwen individually.<br/>For these two LSOAs <strong>combined</strong>, the results are as follows:<br/><br/>",str_sub(label,start=15)),label))

firstNewLSOA <- filter(PennineESAlong,polycode == "BwDcorner") %>% mutate(polycode = "E01032485")
secondNewLSOA <- firstNewLSOA %>% mutate(polycode = "E01032486")
PennineESAlong <- filter(PennineESAlong,polycode != "BwDcorner") %>% bind_rows(firstNewLSOA,secondNewLSOA)

write_csv(PennineESAlong,"PennineESA_LSOA.csv")

