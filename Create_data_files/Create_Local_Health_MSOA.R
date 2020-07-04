library(tidyverse)
library(sqldf)
library(stringr)
library(curl)
library(readr)

# Read in House of Commons Library MSOA names

# Method of downloading and opening .csv file taken from Colin Angus's example at https://github.com/VictimOfMaths/Experiments/blob/master/COVIDLAHeatmap.R

temp <- tempfile()
source <- "https://visual.parliament.uk/msoanames/static/MSOA-Names-v1.1.0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
msoanames <- read.csv(temp) %>% rename(AreaCode = 1) %>% # 1st column name is corrupted
  select(AreaCode,msoa11hclnm)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column
metadata <- read_csv("Metadata.csv")

# NB - Old version of Local Health let you download all the data in ods format. I opened it in Excel and saved it as a .csv, because ods files take an age to read in R.
# LocalHealth <- read.csv("Create_data_files/Local_Health_MSOA_data_June_2019.csv")
# PennineLH <- LocalHealth %>% 
#  filter(`Parent.Name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
#  filter(Category == "")

# However, new version of Local Health does not seem to have an (obvious) option for downloading all the data in that way. 
# Therefore using fingertipsR instead.
library(fingertipsR)
our_inds <- c(93250,93252,93253,93254,93255,93256,93257,93259,93260,93480,
              93227,93229,93231,93232,93233,93239,93240,93241,
              93234,93235,93236,93237,93238,
              93283,93298,
              93115,93219,93114,93224,93116)
PennineLH <- fingertips_data(ProfileID = 143, AreaTypeID = 3, IndicatorID = our_inds) %>%
  filter(ParentName %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale'),AreaType == "MSOA")

fingertips.switch <- function(x,y=NULL)
{
  ifelse(x == 93250,"Death_allcause_allage",
  ifelse(x == 93252,"Death_allcause_under75",
  ifelse(x == 93253,"Death_cancer_allage",
  ifelse(x == 93254,"Death_cancer_under75",
  ifelse(x == 93255,"Death_CVD_allage",
  ifelse(x == 93256,"Death_CVD_under75",
  ifelse(x == 93257,"Death_CHD_allage",
  ifelse(x == 93259,"Death_stroke_allage",
  ifelse(x == 93260,"Death_resp_allage",
  ifelse(x == 93480,"Death_preventable_allage",
  ifelse(x == 93227,"Em_admit_allcause",
  ifelse(x == 93229,"Em_admit_CHD",
  ifelse(x == 93231,"Em_admit_stroke",
  ifelse(x == 93232,"Em_admit_MI",
  ifelse(x == 93233,"Em_admit_COPD",       
  ifelse(x == 93239,"Hosp_stay_selfharm",       
  ifelse(x == 93240,"Hosp_stay_alcohol",  
  ifelse(x == 93241,"Em_admit_hipfrac_65plus",       
  ifelse(x == 93234,"Inc_all_cancer",
  ifelse(x == 93235,"Inc_breast_cancer",
  ifelse(x == 93236,"Inc_bowel_cancer",
  ifelse(x == 93237,"Inc_lung_cancer",
  ifelse(x == 93238,"Inc_prostate_cancer",
  ifelse(x == 93115,"Em_admit_under5",
  ifelse(x == 93114,"Injury_admit_under5",
  ifelse(x == 93219,"Injury_admit_under15",
  ifelse(x == 93224,"Injury_admit_15to24",
  ifelse(x == 93116,"AE_attend_under5",
  ifelse(x == 93283 & y == "Male","LE_male",
  ifelse(x == 93283 & y == "Female","LE_female",
  ifelse(x == 93298 & y == "Male","HLE_male",
  ifelse(x == 93298 & y == "Female","HLE_female",""       
  ))))))))))))))))))))))))))))))))
}

# Attach House of Commons MSOA names
PennineLH <- PennineLH %>% left_join(msoanames,by = "AreaCode")

# Just selecting mortality measures expressed as SMRs 
PennineMortality <- PennineLH %>% filter(`IndicatorID` %in% c(93250,93252,93253,93254,93255,93256,93257,93259,93260,93480))
PennineMortality <- PennineMortality %>% transmute(IndID = fingertips.switch(`IndicatorID`),
                                      polycode = `AreaCode`,
                                      value = Value,
                                      label = paste0("MSOA: ",`AreaCode`,"<br/>",
                                                     "(aka ",sQuote(msoa11hclnm),")<br/>",
                                                     ifelse(is.na(value),"","Standardised Mortality Ratio (SMR): "),
                                                     ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                                    "Similar","Similar to England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                                    "Higher", "Higher than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==       
                                                             "Lower", "Lower than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                            "Better", "Better than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                            "Worse", "Worse than England (= 100)","")))))))

# Just selecting admissions expressed as SARs
PennineAdmissions <- PennineLH %>% filter(`IndicatorID` %in% c(93227,93229,93231,93232,93233,93239,93240,93241))
PennineAdmissions <- PennineAdmissions %>% transmute(IndID = fingertips.switch(`IndicatorID`),
                                      polycode = `AreaCode`,
                                      value = Value,
                                      label = paste0("MSOA: ",`AreaCode`,"<br/>",
                                                     "(aka ",sQuote(msoa11hclnm),")<br/>",
                                                     ifelse(is.na(value),"","Standardised Admission Ratio (SAR): "),
                                                     ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                                    "Similar","Similar to England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                                     "Higher", "Higher than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==       
                                                             "Lower", "Lower than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                            "Better", "Better than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                            "Worse", "Worse than England (= 100)","")))))))

# Just selecting incidences expressed as SIRs
PennineIncidences <- PennineLH %>% filter(`IndicatorID` %in% c(93234,93235,93236,93237,93238))
PennineIncidences <- PennineIncidences %>% transmute(IndID = fingertips.switch(`IndicatorID`),
                                                     polycode = `AreaCode`,
                                                     value = Value,
                                                     label = paste0("MSOA: ",`AreaCode`,"<br/>",
                                                     "(aka ",sQuote(msoa11hclnm),")<br/>",
                                                     ifelse(is.na(value),"","Standardised Incidence Ratio (SIR): "),
                                                     ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                      "Similar","Similar to England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                      "Higher", "Higher than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==       
                                                          "Lower", "Lower than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                         "Better", "Better than England (= 100)",
                                                     ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                          "Worse", "Worse than England (= 100)","")))))))

# Just selecting expectancies expressed in years
PennineExpectancies <- PennineLH %>% filter(`IndicatorID` %in% c(93283,93298))
PennineExpectancies <- PennineExpectancies %>% transmute(IndID = fingertips.switch(`IndicatorID`,Sex),
                                      polycode = `AreaCode`,
                                      value = Value,
                                      label = paste0("MSOA: ",`AreaCode`,"<br/>",
                                                    "(aka ",sQuote(msoa11hclnm),")<br/>",
                                                    ifelse(is.na(value),"",
                                                    ifelse(`IndicatorID` == 93283,"Life Expectancy: ","Healthy Life Expectancy: ")),
                                                    ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                      "Similar","Similar to England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                    "Higher", "Higher than England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==       
                                                       "Lower", "Lower than England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                       "Better", "Better than England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                       "Worse", "Worse than England","")))))))

# If we have data for the chosen MSOA, append the England average (stored in metadata as 'DivergePoint') to the label for comparison
PennineExpectancies <- PennineExpectancies %>% left_join(metadata,by = "IndID") %>%
              mutate(label2 = ifelse(str_detect(label,"England$"),paste0(label," (= ",round(DivergePoint,digits=1),")"),label)) %>%
              select(IndID,polycode,value,label=label2)

# Just selecting child admissions and attendances
PennineChildAdmit <- PennineLH %>% filter(`IndicatorID` %in% c(93115,93219,93114,93224,93116))
PennineChildAdmit <- PennineChildAdmit %>% transmute(IndID = fingertips.switch(`IndicatorID`),
                                      polycode = `AreaCode`,
                                      value = Value,
                                      label = paste0("MSOA: ",`AreaCode`,"<br/>",
                                                    "(aka ",sQuote(msoa11hclnm),")<br/>",
                                                    ifelse(is.na(value),"",
                                                    ifelse(`IndicatorID` %in% c(93115,93116),"Crude rate per 1000: ","Crude rate per 10,000: ")),
                                                    ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                                  "Similar","Similar to England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                                  "Higher", "Higher than England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==       
                                                        "Lower", "Lower than England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                        "Better", "Better than England",
                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                        "Worse", "Worse than England","")))))))
# If we have data for the chosen MSOA, append the England average (stored in metadata as 'DivergePoint') to the label for comparison
PennineChildAdmit <- PennineChildAdmit %>% left_join(metadata,by = "IndID") %>%
  mutate(label2 = ifelse(str_detect(label,"England$"),paste0(label," (= ",round(DivergePoint,digits=1),")"),label)) %>%
  select(IndID,polycode,value,label=label2)

PennineLH <- bind_rows(PennineMortality,PennineAdmissions,PennineIncidences,PennineExpectancies,PennineChildAdmit)
write_excel_csv(PennineLH,"PennineLH_MSOA.csv")

