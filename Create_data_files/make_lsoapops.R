library(tidyverse)

setwd("C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata")
lsoapops <- read.csv("gp-reg-pat-prac-lsoa-all.csv")
PCNdefs <- read.csv("PCNdefs.csv", stringsAsFactors = FALSE)
lsoa <- st_read("https://github.com/bwd-ph/boundaries/blob/master/lsoa.geojson?raw=true")
colnames(lsoa)[2] = "polycode"
colnames(lsoa)[3] = "polyname"
colnames(lsoa)[4] = "polynamew"

PCNdefs$PCN <- trimws(PCNdefs$PCN) # found trailing right spaces
PCNdefs$Practice.Name <- trimws(PCNdefs$Practice.Name) # found trailing right spaces

lsoapops <- lsoapops %>% filter(LSOA_CODE %in% lsoa$polycode | PRACTICE_CODE %in% PCNdefs$PCode)
write.csv(lsoapops,"lsoapops.csv")
