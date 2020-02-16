library(tidyverse)

setwd("C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata")
males <- read.csv("gp-reg-pat-prac-sing-age-male.csv")
females <- read.csv("gp-reg-pat-prac-sing-age-female.csv")

males <- males %>% filter(ONS_CCG_CODE %in% c("E38000014","E38000050") & AGE != "ALL") %>% select(ORG_CODE, SEX, AGE, POP = NUMBER_OF_PATIENTS) %>% mutate(POP = POP * -1)
females <- females %>% filter(ONS_CCG_CODE %in% c("E38000014","E38000050") & AGE != "ALL") %>% select(ORG_CODE, SEX, AGE, POP = NUMBER_OF_PATIENTS)
both <- rbind(males, females) %>%
  mutate(abs_pop = abs(POP))
write.csv(both,"practicepops.csv")

