#comparing industry to median MSA income

source("./scripts/import_pums.R")

if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(pander, readr, survey, srvyr, ggplot2, ggthemes, dplyr)


chi_svy <-  svrepdesign(weight = ~PERWT,
                        repweights = 'REPWTP[0-9]+',
                        scale = 4 / 80,
                        rscales = rep(1, 80),
                        mse = TRUE,
                        type = 'JK1',
                        data = chi)


chi_svy <- as_survey_rep(chi_svy)


ind_wage <- read_csv("./data/2017-08-19_industry-region-wage-comparison.csv")

ind_wage <- ind_wage %>% filter(INDNAICS != "0")

poor_wage <- ind_wage %>% filter(MED_WAGE_DUMMY == 0)


p1 <- ggplot(ind_wage, aes(x = INDNAICS, y = TOT_EMP, fill = as.factor(MED_WAGE_DUMMY)))
p1 + geom_bar(stat = "identity") + theme_minimal() + coord_flip()
