#first cut at IPUMS for Greg for city that works
#Greg wants a large crosstabs table based on the following groups:
#IND, SOC major group, SEX, RACE, ETHNICITY, RACE/ETHNICITY

if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(pander, readr, stringr, ggplot2, dplyr)

chi <- read_csv("data/usa_00055.csv", col_types = 
                  cols(.default = col_integer(),
                       OCCSOC = col_character(),
                       IND = col_character(),
                       INDNAICS = col_character(),
                       RACE = col_character(),
                       HISPAN = col_character()))

#drop the replicate weights for these rough estimates
chi <- chi[,1:38]

#recode to major SOC groups for easier summing up for Greg and grab employed
#get other groups created and listed up

chi <- chi %>% mutate(SOC_MAJOR = str_sub(OCCSOC, 1, 2))

chi <- chi %>% filter(LABFORCE > 0)

#race recodes
chi <- chi %>% mutate(RACE2 = recode(RACE, "1" = "White",
                                     "2" = "Black",
                                     "3" = "AIAN",
                                     "4" = "Asian",
                                     "5" = "Asian",
                                     "6" = "Asian",
                                     "7" = "Other Race",
                                     "8" = "Multiracial",
                                     "9" = "Multiracial"),
                      HISP2 = recode(HISPAN, "0" = "Not Hispanic",
                        "9" = "Unknown",
                        .default = "Hispanic"))


chi <- chi %>% mutate(SOC_LABEL = recode(SOC_MAJOR, "11" = "Management Occupations",
                                         "13" = "Business and Financial Operations Occupations",
                                         "15" = "Computer and Mathematical Occupations",
                                         "17" = "Architecture and Engineering Occupations",
                                         "19" = "Life, Physical, and Social Science Occupations",
                                         "21" = "Community and Social Services Occupations",
                                         "23" = "Legal Occupations",
                                         "25" = "Educations, Library, and Training Occupations",
                                         "27" = "Arts, Design, Entertainment, Sports, and Media Occupations",
                                         "29" = "Healthcare Practitioners and Technical Occupations",
                                         "31" = "Healthcare Support Occupations",
                                         "33" = "Protective Service Occupations",
                                         "35" = "Food Preparation and Service Occupations",
                                         "37" = "Building and Grounds Cleaning and Maintenance Occupations",
                                         "39" = "Personal Care and Service Occupations",
                                         "41" = "Sales and Related Occupations",
                                         "43" = "Office and Administrative Support Occupations",
                                         "45" = "Farming, Fishing, and Forestry Occupations",
                                         "47" = "Construction and Extraction Occupations",
                                         "49" = "Installation, Maintenance, and Repair Occupations",
                                         "51" = "Production Occupations",
                                         "53" = "Tarnsportation and Material Moving Occupations",
                                         "55" = "Military Occupations"))

chi$SOC_LABEL[chi$SOC_LABEL == "0"] <- NA
chi$SOC_LABEL[chi$SOC_LABEL == "99"] <- "Unemployed"

chi$RACE_HISP <- NA
chi$RACE_HISP[chi$RACE2 == "White" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknwon"] <- "Non-Hispanic White"
chi$RACE_HISP[chi$RACE2 == "White" & chi$HISP2 == "Hispanic"] <-  "Hispanic White"
chi$RACE_HISP[chi$RACE2 == "Black" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknwon"] <- "Non-Hispanic Black"
chi$RACE_HISP[chi$RACE2 == "Black" & chi$HISP2 == "Hispanic"] <- "Hispanic Black"
chi$RACE_HISP[chi$RACE2 == "AIAN" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknwon"] <- "Non-Hispanic AIAN"
chi$RACE_HISP[chi$RACE2 == "AIAN" & chi$HISP2 == "Hispanic"] <- "Hispanic AIAN"
chi$RACE_HISP[chi$RACE2 == "Asian" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknwon"] <- "Non-Hispanic Asian"
chi$RACE_HISP[chi$RACE2 == "Asian" & chi$HISP2 == "Hispanic"] <- "Hispanic Asian"
chi$RACE_HISP[chi$RACE2 == "Other Race" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknwon"] <- "Non-Hispanic Other"
chi$RACE_HISP[chi$RACE2 == "Other Race" & chi$HISP2 == "Hispanic"] <- "Hispanic Other"
chi$RACE_HISP[chi$RACE2 == "Multiracial" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknwon"] <- "Non-Hispanic Multiracial"
chi$RACE_HISP[chi$RACE2 == "Multiracial" & chi$HISP2 == "Hispanic"] <- "Hispanic Multiracial"





ind_totals <- chi %>% group_by(IND) %>% summarise(TOTALS = sum(PERWT, na.rm = TRUE))

ind_soc <- chi %>% group_by(IND, SOC_LABEL) %>% summarise(TOTALS = sum(PERWT, na.rm = TRUE))

ind_sex <- chi %>% group_by(IND, SEX) %>% summarise(TOTALS = sum(PERWT, na.rm = TRUE))

soc_sex <- chi %>% group_by(SOC_LABEL, SEX) %>% summarise(TOTALS= sum(PERWT, na.rm = TRUE))

ind_race <- chi %>% group_by(IND, RACE2, HISP2) %>% summarise(TOTALS = sum(PERWT, na.rm = TRUE))

soc_race <- chi %>% group_by(SOC_LABEL, RACE2, HISP2) %>% summarise(TOTALS = sum(PERWT, na.rm = TRUE))

#write_csv(ind_totals, "data/ind_totals.csv")
#write_csv(ind_sex, "data/ind_sex.csv")
#write_csv(ind_race, "data/ind_race.csv")
#write_csv(ind_soc, "data/ind_soc.csv")
#write_csv(soc_race, "data/soc_race.csv")
#write_csv(soc_sex, "data/soc_sex.csv")


big_tab <- chi %>% group_by(IND, SOC_LABEL,SEX, RACE_HISP) %>%
  summarise(TOTALS = sum(PERWT, na.rm = TRUE))

write_csv(big_tab, "data/big_crosstab.csv")



#percent of workforce above Living Wage: http://livingwage.mit.edu/counties/17031
chi$ADULT_CHILD_LW <- (24.89 * 2080)
chi$ADULT_LW <-  (12.56 * 2080)

