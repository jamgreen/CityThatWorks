#first cut at IPUMS for Greg for city that works
#Greg wants a large crosstabs table based on the following groups:
#IND, SOC major group, SEX, RACE, ETHNICITY, RACE/ETHNICITY

if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(readr, stringr, dplyr)

chi <- read_csv("data/usa_00056.csv.gz", col_types = 
                  cols(.default = col_integer(),
                       EMPSTAT = col_integer(),
                       OCCSOC = col_character(),
                       IND = col_character(),
                       INDNAICS = col_character(),
                       RACE = col_character(),
                       HISPAN = col_character()))

#recode to major SOC groups for easier summing up for Greg and grab employed
#get other groups created and listed up ----

chi <- chi %>% mutate(SOC_MAJOR = str_sub(OCCSOC, 1, 2))

chi <- chi %>% filter(EMPSTAT == 1)

#race and race/hispanic recodes-----
chi <- chi %>% mutate(RACE2 = recode(RACE, "1" = "White",
                                     "2" = "Black",
                                     "3" = "AIAN",
                                     "4" = "Asian",
                                     "5" = "Asian",
                                     "6" = "Asian",
                                     "7" = "Other Race",
                                     "8" = "Other Race",
                                     "9" = "Other Race"),
                      HISP2 = recode(HISPAN, "0" = "Not Hispanic",
                                     "9" = "Unknown",
                                     .default = "Hispanic"))

chi$RACE2 <- as.factor(chi$RACE2)

chi$RACE_HISP <- NA
chi$RACE_HISP[chi$RACE2 == "White" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknown"] <- "Non-Hispanic White"
chi$RACE_HISP[chi$RACE2 == "White" & chi$HISP2 == "Hispanic"] <-  "Hispanic White"
chi$RACE_HISP[chi$RACE2 == "Black" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknown"] <- "Non-Hispanic Black"
chi$RACE_HISP[chi$RACE2 == "Black" & chi$HISP2 == "Hispanic"] <- "Hispanic Black"
chi$RACE_HISP[chi$RACE2 == "AIAN" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknown"] <- "Non-Hispanic AIAN"
chi$RACE_HISP[chi$RACE2 == "AIAN" & chi$HISP2 == "Hispanic"] <- "Hispanic AIAN"
chi$RACE_HISP[chi$RACE2 == "Asian" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknown"] <- "Non-Hispanic Asian"
chi$RACE_HISP[chi$RACE2 == "Asian" & chi$HISP2 == "Hispanic"] <- "Hispanic Asian"
chi$RACE_HISP[chi$RACE2 == "Other Race" & chi$HISP2 == "Not Hispanic" | chi$HISP2 == "Unknown"] <- "Non-Hispanic Other"
chi$RACE_HISP[chi$RACE2 == "Other Race" & chi$HISP2 == "Hispanic"] <- "Hispanic Other"

chi$RACE_HISP <- as.factor(chi$RACE_HISP)

chi <- chi %>% mutate(RACE_HISP2 = ifelse(RACE_HISP %in% c("Hispanic White", "Hispanic Black",
                                          "Hispanic AIAN", "Hispanic Asian","Hispanic Other"), 
                                          "Hispanic", as.character(RACE2)))


# occupation recodes -----
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
                                         "53" = "Transportation and Material Moving Occupations",
                                         "55" = "Military Occupations",
                                         "0" = "Not Applicable"))

chi$SOC_LABEL[chi$SOC_LABEL == "0"] <- NA
chi$SOC_LABEL[chi$SOC_LABEL == "99"] <- "Unemployed"




#percent of workforce above Living Wage: http://livingwage.mit.edu/counties/17031 ----
chi$ADULT_CHILD_LW <- (24.89 * 2080)
chi$ADULT_LW <-  (12.56 * 2080)

chi <- chi %>% mutate(ADULT_CHILD_DUMMY = ifelse(INCWAGE > ADULT_CHILD_LW, 1, 0),
                      ADULT_DUMMY = ifelse(INCWAGE > ADULT_LW, 1, 0))

#find manufacturing industries----

chi <- chi %>% mutate(MFG = ifelse(grepl("^3", chi$INDNAICS),1,0))


#NAICS supersector

chi <- chi %>% mutate(NAICS_SUPER = str_sub(INDNAICS, 1, 2))

naics_labels <- tribble(~NAICS_SUPER, ~NAICS_LABEL,
                        "11" , "Agriculture, Forestry, Fishing and Hunting",
                        "21" , "Mining",
                        "22" , "Utilities",
                        "23" , "Construction",
                        "31" , "Manufacturing",
                        "32" , "Manufacturing",
                        "3M" , "Manufacturing",
                        "33" , "Manufacturing",
                        "42" , "Wholesale Trade",
                        "44" , "Retail Trade",
                        "4M" , "Retail Trade",
                        "45" , "Retail Trade",
                        "48" , "Transportation and Warehousing",
                        "49" , "Transportation and Warehousing",
                        "51" , "Information and Communications",
                        "52" , "Fire, Insurance, Real Estate,\n and Rental and Leasing",
                        "53" , "Fire, Insurance, Real Estate,\n and Rental and Leasing",
                        "54" , "Professional, Scientific, Management,  Administrative,\n
                        and Waste Management Services",
                        "55" , "Professional, Scientific, Management,  Administrative,\n
                        and Waste Management Services",
                        "56" , "Professional, Scientific, Management,  Administrative,\n
                        and Waste Management Services",
                        "61" , "Education, Health, and Social Services",
                        "62" , "Education, Health, and Social Services",
                        "71", "Arts, Entertainment, Recreation, Accommodations,\n
                        and Food Services",
                        "72", "Arts, Entertainment, Recreation, Accommodations,\n
                        and Food Services",
                        "81" , "Other Services",
                        "92" , "Public Administration and Military",
                        "99" , "Unemployed",
                        "0", "Not Applicable")

chi <- chi %>% left_join(naics_labels)

chi <- chi %>% 
  mutate(NAICS_ADJUSTED = ifelse(NAICS_SUPER == "31" | NAICS_SUPER == "32"|
                                NAICS_SUPER == "33" | NAICS_SUPER == "3M", "31-33M",
                                ifelse(NAICS_SUPER == "44"| NAICS_SUPER == "45" | 
                                NAICS_SUPER == "4M", "44-45", NAICS_SUPER)))

chi <- chi %>% mutate(NAICS_LABEL_ADJUSTED = recode(NAICS_ADJUSTED, "31-33M" = "Manufacturing",
                                                         "44-45" ="Retail Trade",
                                                         .default = NAICS_LABEL))

# chi$NAICS_LABEL_ADJUSTED <- factor(chi$NAICS_LABEL_ADJUSTED, 
#                                           levels = c("Agriculture, Forestry, Fishing and Hunting",
#                                           "Mining", "Utilities", "Construction", "Manufacturing",
#                                           "Wholesale Trade","Retail Trade", "Transportation and Warehousing",
#                                           "Information and Communications",
#                                           "Fire, Insurance, Real Estate,\n and Rental and Leasing",
#                                           "Professional, Scientific, Management,  Administrative,\n
#                                            and Waste Management Services",
#                                           "Education, Health, and Social Services",
#                                           "Arts, Entertainment, Recreation, Accommodations,\n
#                                           and Food Services",
#                                           "Other Services", "Public Administration and Military"))

rm(naics_labels)
