library(tidyverse)
library(sqldf)
library(stringr)
library(excel.link)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

LocalHealth <- xl.read.file("Create_data_files/LocalHealth_All_indicators_MSOA_data.xlsb",header=TRUE)

PennineLH <- LocalHealth %>% 
  filter(
#    `Area Name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale','England') |
     `Parent Name` %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
  filter(is.na(Category))

fingertips.switch <- function(x)
{
  ifelse(x == 93250,"Death_allcause_allage",
  ifelse(x == 93251,"Death_allcause_under65",
  ifelse(x == 93252,"Death_allcause_under75",
  ifelse(x == 93253,"Death_cancer_allage",
  ifelse(x == 93254,"Death_cancer_under75",
  ifelse(x == 93255,"Death_CVD_allage",
  ifelse(x == 93256,"Death_CVD_under75",
  ifelse(x == 93257,"Death_CHD_allage",
  ifelse(x == 93258,"Death_CHD_under75",
  ifelse(x == 93259,"Death_stroke_allage",
  ifelse(x == 93260,"Death_resp_allage",
  ifelse(x == 93227,"Em_admit_allcause",
  ifelse(x == 93228,"El_admit_allcause",
  ifelse(x == 93229,"Em_admit_CHD",
  ifelse(x == 93230,"El_admit_CHD",
  ifelse(x == 93231,"Em_admit_stroke",
  ifelse(x == 93232,"Em_admit_MI",
  ifelse(x == 93233,"Em_admit_COPD",       
  ifelse(x == 93239,"Hosp_stay_selfharm",       
  ifelse(x == 93240,"Hosp_stay_alcohol",  
  ifelse(x == 93241,"Em_admit_hipfrac_65plus",       
  ifelse(x == 93242,"El_admit_hiprep",       
  ifelse(x == 93243,"El_admit_kneerep",
  ifelse(x == 93234,"Inc_all_cancer",
  ifelse(x == 93235,"Inc_breast_cancer",
  ifelse(x == 93236,"Inc_bowel_cancer",
  ifelse(x == 93237,"Inc_lung_cancer",
  ifelse(x == 93238,"Inc_prostate_cancer",""       
  ))))))))))))))))))))))))))))
}

# Just selecting mortality measures expressed as SMRs 
PennineMortality <- PennineLH %>% filter(`Indicator ID` %in% c(93250,93251,93252,93253,93254,93255,93256,93257,93258,93259,93260))
PennineMortality <- PennineMortality %>% transmute(IndID = fingertips.switch(`Indicator ID`),
                                      polycode = `Area Code`,
                                      value = Value,
                                      label = paste0("MSOA: ",`Area Code`,"<br/>",
                                                     ifelse(is.na(value),"","Standardised Mortality Ratio (SMR): "),
                                                     ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                     ifelse(`Compared to England value or percentiles` ==                                                                                         "Same","Similar to England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==                                                                                         "Higher", "Higher than England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==       
                                                             "Lower", "Lower than England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==
                                                            "Better", "Better than England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==
                                                            "Worse", "Worse than England (= 100)","")))))))

# Just selecting admissions expressed as SARs
PennineAdmissions <- PennineLH %>% filter(`Indicator ID` %in% c(93227,93228,93229,93230,93231,93232,93233,93239,93240,93241,93242,93243))
PennineAdmissions <- PennineAdmissions %>% transmute(IndID = fingertips.switch(`Indicator ID`),
                                      polycode = `Area Code`,
                                      value = Value,
                                      label = paste0("MSOA: ",`Area Code`,"<br/>",
                                                     ifelse(is.na(value),"","Standardised Admission Ratio (SAR): "),
                                                     ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                     ifelse(`Compared to England value or percentiles` ==                                                                                         "Same","Similar to England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==                                                                                         "Higher", "Higher than England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==       
                                                             "Lower", "Lower than England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==
                                                            "Better", "Better than England (= 100)",
                                                     ifelse(`Compared to England value or percentiles` ==
                                                            "Worse", "Worse than England (= 100)","")))))))

# Just selecting incidences expressed as SIRs
PennineIncidences <- PennineLH %>% filter(`Indicator ID` %in% c(93234,93235,93236,93237,93238))
PennineIncidences <- PennineIncidences %>% transmute(IndID = fingertips.switch(`Indicator ID`),
                                                     polycode = `Area Code`,
                                                     value = Value,
                                                     label = paste0("MSOA: ",`Area Code`,"<br/>",
                                                                    ifelse(is.na(value),"","Standardised Incidence Ratio (SIR): "),
                                                                    ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                                    ifelse(`Compared to England value or percentiles` ==                                                                                         "Same","Similar to England (= 100)",
                                                                    ifelse(`Compared to England value or percentiles` ==                                                                                         "Higher", "Higher than England (= 100)",
                                                                    ifelse(`Compared to England value or percentiles` ==       
                                                                    "Lower", "Lower than England (= 100)",
                                                                    ifelse(`Compared to England value or percentiles` ==
                                                                    "Better", "Better than England (= 100)",
                                                                    ifelse(`Compared to England value or percentiles` ==
                                                                    "Worse", "Worse than England (= 100)","")))))))

PennineLH <- bind_rows(PennineMortality,PennineAdmissions,PennineIncidences)
write_csv(PennineLH,"PennineLH_MSOA.csv")

