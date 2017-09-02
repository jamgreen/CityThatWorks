#reproduce the first cuts but with survey getting the SEs

source("./scripts/import_pums.R")

if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(stringr, survey, srvyr, dplyr)


chi_svy <-  svrepdesign(weight = ~PERWT,
                        repweights = 'REPWTP[0-9]+',
                        scale = 4 / 80,
                        rscales = rep(1, 80),
                        mse = TRUE,
                        type = 'JK1',
                        data = chi)


chi_svy <- as_survey_rep(chi_svy)


###share of manufacturing jobs above single adult living wage by NAICS----

lw_mfg <- chi_svy %>% filter(MFG == 1) %>% group_by(INDNAICS) %>%
  summarise(ABOVE_LW = survey_total(ADULT_DUMMY == 1, vartype = "cv"),
            BELOW_LW = survey_total(ADULT_DUMMY == 0, vartype = "cv"))

lw_mfg <- lw_mfg %>% mutate(LW_SHARE = ABOVE_LW/(ABOVE_LW + BELOW_LW))

###race/ethnicity of mfg by NAICS and SOC-----

race_mfg <- chi_svy %>% filter(MFG == 1) %>% group_by(INDNAICS, SOC_LABEL,RACE_HISP) %>%
  summarise(RACE_TOT = survey_total(vartype = "cv"))

race_mfg <- race_mfg %>% ungroup() %>% group_by(INDNAICS) %>%
  mutate(TOT_MFG_WORKERS = sum(RACE_TOT), RACE_MFG_SHARE = RACE_TOT/TOT_MFG_WORKERS) %>%
  ungroup() %>% group_by(INDNAICS, SOC_LABEL) %>%
  mutate(TOT_OCC_WORKERS = sum(RACE_TOT), OCC_MFG_SHARE = RACE_TOT/TOT_OCC_WORKERS)

###compare industry wages to median household income for the region-----


med_tot_inc <- chi_svy %>% summarise(MED_TOT_INC = survey_median(INCTOT))
med_wage_inc <- chi_svy %>% summarise(MED_WAGE_INC = survey_median(INCWAGE))

med_tot_inc_NAICS <- chi_svy %>% group_by(INDNAICS) %>%
  summarise(TOT_EMP = survey_total(),
            MED_TOT_INC_NAICS = survey_median(INCTOT))

med_wage_inc_NAICS <- chi_svy %>% group_by(INDNAICS) %>%
  summarise(MED_WAGE_INC_NAICS = survey_median(INCWAGE))

med_tot_inc_NAICS <- med_tot_inc_NAICS %>% inner_join(med_wage_inc_NAICS)
med_tot_inc_NAICS <- cbind(med_tot_inc_NAICS, med_tot_inc, med_wage_inc)

med_tot_inc_NAICS <- med_tot_inc_NAICS %>% 
  mutate(MFG = ifelse(grepl("^3", med_tot_inc_NAICS$INDNAICS), 1, 0))

med_tot_inc_NAICS <- med_tot_inc_NAICS %>% 
  mutate(MED_INC_DUMMY = ifelse(MED_TOT_INC_NAICS_q50 > MED_TOT_INC_q50, 1, 0),
    MED_WAGE_DUMMY = ifelse(MED_WAGE_INC_NAICS_q50 > MED_WAGE_INC_q50, 1, 0))

write_csv(med_tot_inc_NAICS, "./data/2017-08-19_industry-region-wage-comparison.csv")


###stacked bar chart NAICS super sectors (2digit) by race------
#chi_svy <- chi_svy %>% mutate(NAICS_SUPER = str_sub(INDNAICS, 1, 2))

naics_total <- chi_svy %>% filter(NAICS_SUPER != "0") %>% 
  group_by(NAICS_SUPER) %>% summarise(NAICS_TOT = survey_total())
  
naics_racehisp <- chi_svy %>%   filter(NAICS_SUPER != "0") %>%
  group_by(NAICS_SUPER, RACE_HISP) %>% summarise(RACEHISP_TOTAL = survey_total())

naics_racehisp <- inner_join(naics_racehisp, naics_total)
naics_racehisp <- naics_racehisp %>% mutate(RACEHISP_SHARE = RACEHISP_TOTAL/NAICS_TOT)

naics_race <- chi_svy %>%   filter(NAICS_SUPER != "0") %>%
  group_by(NAICS_SUPER, RACE2) %>% summarise(RACE_TOTAL = survey_total())
naics_race <- inner_join(naics_race, naics_total)
naics_race <- naics_race %>% mutate(RACE_SHARE = RACE_TOTAL/NAICS_TOT)

naics_hisp <- chi_svy %>% filter(NAICS_SUPER != "0") %>% 
  group_by(NAICS_SUPER, HISP2) %>%  summarise(HISP_TOTAL = survey_total())

naics_hisp <- inner_join(naics_hisp, naics_total)
naics_hisp <- naics_hisp %>% mutate(HISP_SHARE = HISP_TOTAL/NAICS_TOT)

write_csv(naics_race, "./data/2017-08-28_naics_super_by_race.csv" )
write_csv(naics_hisp,"./data/2017-08-28_naics_super_by_hisp.csv")
write_csv(naics_racehisp, "./data/2017-08-28_naics_super_by_racehisp.csv")

#have to use chi because of some matrix error with the new RACEHISP2 variable
naics_hisp2 <- chi %>% filter(NAICS_SUPER != "0") %>% 
  group_by(NAICS_SUPER, NAICS_LABEL ,RACE_HISP2) %>% summarise(RACE_HISP2_NAICS_TOTAL = sum(PERWT))

naics_hisp2 <- naics_hisp2 %>% group_by(NAICS_SUPER, NAICS_LABEL) %>% 
  mutate(NAICS_TOT_EMP = sum(RACE_HISP2_NAICS_TOTAL),
        RACE_HISP_SHARE = RACE_HISP2_NAICS_TOTAL/NAICS_TOT_EMP) %>% ungroup()

soc_hisp2 <- chi %>% filter(SOC_LABEL != "Not Applicable") %>% 
  group_by(SOC_LABEL, RACE_HISP2) %>% summarise(RACE_HISP2_SOC_TOTAL = sum(PERWT)) 

soc_hisp2 <- soc_hisp2 %>% group_by(SOC_LABEL) %>% 
  mutate(SOC_TOT_EMP = sum(RACE_HISP2_SOC_TOTAL),
         RACE_HISP_SHARE = RACE_HISP2_SOC_TOTAL/SOC_TOT_EMP) %>% ungroup()

write_csv(naics_hisp2, "./data/2017-08-30_naics_super_racehisp2.csv")
write_csv(soc_hisp2, "./data/2017-08-30_soc_racehisp2.csv")

rm(list = ls())
