library(tidyverse)
library(sqldf)
library(stringr)

setwd('C:/Users/user/Documents/BwD work/Interactive Mapping/mapdata')

PennineDep <- read.csv("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv",header=TRUE)

PennineDep <- PennineDep %>% 
  filter(Local.Authority.District.name..2013. %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
  select(lsoa11cd = LSOA.code..2011.,
         IMDScore = Index.of.Multiple.Deprivation..IMD..Score,
         IMDRank = Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,
         IMDDecile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
         IncomeScore = Income.Score..rate.,
         IncomeRank = Income.Rank..where.1.is.most.deprived.,
         IncomeDecile = Income.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         EmpScore = Employment.Score..rate.,
         EmpRank = Employment.Rank..where.1.is.most.deprived.,
         EmpDecile = Employment.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         EducScore = Education..Skills.and.Training.Score,
         EducRank = Education..Skills.and.Training.Rank..where.1.is.most.deprived.,
         EducDecile = Education..Skills.and.Training.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         HealthScore = Health.Deprivation.and.Disability.Score,
         HealthRank = Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.,
         HealthDecile = Health.Deprivation.and.Disability.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         CrimeScore = Crime.Score,
         CrimeRank =  Crime.Rank..where.1.is.most.deprived.,
         CrimeDecile = Crime.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         BarriersScore = Barriers.to.Housing.and.Services.Score,
         BarriersRank = Barriers.to.Housing.and.Services.Rank..where.1.is.most.deprived.,
         BarriersDecile = Barriers.to.Housing.and.Services.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         LivEnvScore = Living.Environment.Score,
         LivEnvRank = Living.Environment.Rank..where.1.is.most.deprived.,
         LivEnvDecile = Living.Environment.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         IDACIScore = Income.Deprivation.Affecting.Children.Index..IDACI..Score..rate.,
         IDACIRank = Income.Deprivation.Affecting.Children.Index..IDACI..Rank..where.1.is.most.deprived.,
         IDACIDecile  = Income.Deprivation.Affecting.Children.Index..IDACI..Decile..where.1.is.most.deprived.10..of.LSOAs.,
         IDAOPIScore = Income.Deprivation.Affecting.Older.People..IDAOPI..Score..rate.,
         IDAOPIRank = Income.Deprivation.Affecting.Older.People..IDAOPI..Rank..where.1.is.most.deprived.,
         IDAOPIDecile = Income.Deprivation.Affecting.Older.People..IDAOPI..Decile..where.1.is.most.deprived.10..of.LSOAs.) %>%
  gather(key=type,value=value,-lsoa11cd) %>% 
  mutate(measure = str_extract(type,"Score$|Rank$|Decile$")) %>%
  mutate(index_domain = str_replace(type,"Score|Rank|Decile",""))

  PennineDep$index_domain <- recode(PennineDep$index_domain,"IMD" = "IMD",
         "Income" = "Income_dep",
         "Emp" = "Emp_dep",
         "Educ" = "Educ_dep",
         "Health" = "Health_dep",
         "Crime" = "Crime_dep",
         "Barriers" = "Barriers_dep",
         "LivEnv" = "Env_dep",
         "IDACI" = "IDACI",
         "IDAOPI" = "IDAOPI")
  PennineDep <- select(PennineDep,IndID = index_domain, polycode = lsoa11cd,measure,value) %>%
    spread(measure, value) %>% 
    rename(score = Score, rank = Rank, decile = Decile) %>%
    mutate(label = paste0("LSOA: ", polycode,"<br/>",
                          "Rank: ",rank," (out of 32844)","<br>",
                          "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = most deprived)")) %>%
    select(IndID,polycode,value = decile,label)
  
  # Mood and anxiety composite indicator comes from a different file, and has been manipulated in Excel to assign deciles using the PHE tool at https://fingertips.phe.org.uk/profile/guidance
  
  MoodDep <- read.csv("MoodAnxietyDep.csv") %>%
    filter(Local.Authority.District.name..2013. %in% c('Blackburn with Darwen','Burnley','Hyndburn','Pendle','Ribble Valley','Rossendale')) %>%
    add_column(IndID = "Mood_dep") %>%
    rename(polycode = `LSOA.code..2011.`,rank=MoodRank,decile=MoodDecile) %>%
    mutate(label = paste0("LSOA: ", polycode,"<br/>",
                          "Rank: ",rank," (out of 32844)","<br>",
                          "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = most deprived)")) %>%
    select(IndID,polycode,value = decile,label)
  
  write_csv(bind_rows(PennineDep,MoodDep),"IMD_2015_LSOA.csv")
