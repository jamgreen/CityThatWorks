#some sample graphs for greg and marc
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(readr, stringr, ggplot2, ggthemes, scales, dplyr)


#living wage dotplot------
lw_df <- read_csv("./data/2017-08-18_mfg_living_wage.csv")
lw_df <- lw_df %>% mutate(TOT_EMP = ABOVE_LW + BELOW_LW)

p1 <- ggplot(lw_df, aes(x = LW_SHARE, y = reorder(INDNAICS, LW_SHARE))) 
p1 + geom_point() + theme_minimal() + labs(x = "Share of Workers Making Above Living Wage",
                                           y = "Industry (NAICS)",
                                           title = "Percentage of Mfg. Workers Earning a Living Wage,\n 
                                           Chicago MSA")

ggsave("./reports/plots/01-mfg_lw_dotplot.pdf", width = 6.5, height = 8, dpi = 300)


# scatterplot of % non-white and % above living wage-------

mfg_race <- read_csv("./data/2017-08-19_ind-occ-race-mfg.csv")

#calculate industry race/ethnic share------
mfg_race <- mfg_race %>% group_by(INDNAICS, RACE_HISP) %>%
  summarise(RACE_TOT = sum(RACE_TOT))

mfg_race <- mfg_race %>% group_by(INDNAICS) %>% 
  mutate(TOT_EMP = sum(RACE_TOT), RACE_SHARE = RACE_TOT/TOT_EMP) %>% ungroup()

lw_df <- lw_df %>% inner_join(mfg_race, by = c("INDNAICS" = "INDNAICS"))

non_white <- lw_df %>% filter(RACE_HISP != "Non-Hispanic White") %>% group_by(INDNAICS) %>%
  summarise(NON_WHITE = sum(RACE_TOT))

lw_df <- lw_df %>% inner_join(non_white)

lw_df <- lw_df %>% ungroup() %>% mutate(NON_WHITE_SHARE = NON_WHITE/TOT_EMP.x)

p2 <- ggplot(lw_df, aes(x = LW_SHARE, y = NON_WHITE_SHARE))
p2 + geom_point() + theme_minimal() +
  geom_smooth(method = "lm") +
  labs(x = "Share of Workers Earning Living Wage",
       y = "Share of Non-White Workers",
       title = "Manufacturing Industries with more\n People of Color Earn Less",
       caption = "Source: ACS (2011-2015), IPUMS-USA, University of Minnesota")

ggsave("./reports/plots/02-mfg_raceshare_lw_scatterplot.pdf", width = 6.5)

#scatterplot variation with proportional symbol------

p3 <- ggplot(lw_df, aes(x = LW_SHARE, y = NON_WHITE_SHARE, size = TOT_EMP.x))
p3 + geom_point() + theme_minimal() +
  geom_smooth(method = "lm") +
  labs(x = "Share of Workers Earning Living Wage",
       y = "Share of Non-White Workers",
       title = "Manufacturing Industries with more\n People of Color Earn Less",
       caption = "Source: ACS (2011-2015), IPUMS-USA, University of Minnesota")


#stacked bar chart race/hispanic by super sector naics ------

naics_race <- read_csv("./data/2017-08-28_naics_super_by_race.csv")


p_race <- ggplot(naics_race, aes(NAICS_SUPER, RACE_SHARE, fill = RACE2))
p_race + geom_bar(stat = "identity", position = "stack") + theme_minimal() 

naics_hisp <- read_csv("./data/2017-08-28_naics_super_by_hisp.csv")

p_hisp <- ggplot(naics_hisp, aes(NAICS_SUPER, HISP_SHARE, fill = HISP2))
p_hisp + geom_bar(stat = "identity", position = "stack") + theme_minimal()


#Stacked bar chart race/hispanic2 by naics and then SOC-----
naics_race <- read_csv("./data/2017-08-30_naics_super_racehisp2.csv")
soc_race <- read_csv("./data/2017-08-30_soc_racehisp2.csv")




naics_race <- naics_race %>% group_by(NAICS_LABEL_ADJUSTED,RACE_HISP2) %>% 
  summarise(RACE_TOTAL = sum(RACE_HISP2_NAICS_TOTAL))
naics_race <- naics_race %>% group_by(NAICS_LABEL_ADJUSTED) %>% 
  mutate(NAICS_TOT = sum(RACE_TOTAL),RACE_HISP_SHARE = RACE_TOTAL/NAICS_TOT)


naics_race$NAICS_LABEL_ADJUSTED <- factor(naics_race$NAICS_LABEL_ADJUSTED, 
                                          levels = naics_race$NAICS_LABEL_ADJUSTED)

naics_plot <- naics_race %>% filter(!is.na(NAICS_LABEL_ADJUSTED)) %>% 
  ggplot(aes(x = factor(NAICS_LABEL_ADJUSTED, levels =rev(levels(NAICS_LABEL_ADJUSTED))) , 
             y = RACE_HISP_SHARE, fill = RACE_HISP2))

naics_plot + geom_bar(stat = "identity", position = "stack") + theme_minimal() +
  labs(x = "NAICS Super Sectors", y = "Race/Ethnicity Share of Employment", 
      title = "Share of Workers by Race/Ethnicity in Industry Super Sectors\n for the Chicago MSA, 2011-2015",
       caption = "Source: IPUMS-USA, Univeristy of Minnesota, American Community Survey 2011-2015") +
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.text.x = element_text(size = 8.5)) + 
  scale_fill_ptol(name = "Race/Hispanic Ethnicity") + coord_flip()

ggsave(filename = "04-naics_by_racehisp_stack.pdf",path = "./reports/plots/", width = 8.5, height = 8)


#create a shorter SOC label for easier display on axes
#greg wants to arrange by SOC code. so, i have to get the SOC codes
#back into this table. 

soc_codes <- tribble(~SOC_CODE, ~SOC_LABEL,
                    "11" , "Management Occupations",
                    "13" , "Business and Financial Operations Occupations",
                    "15" , "Computer and Mathematical Occupations",
                    "17" , "Architecture and Engineering Occupations",
                    "19" , "Life, Physical, and Social Science Occupations",
                    "21" , "Community and Social Services Occupations",
                    "23" , "Legal Occupations",
                    "25" , "Educations, Library, and Training Occupations",
                    "27" , "Arts, Design, Entertainment, Sports, and Media Occupations",
                    "29" , "Healthcare Practitioners and Technical Occupations",
                    "31" , "Healthcare Support Occupations",
                    "33" , "Protective Service Occupations",
                    "35" , "Food Preparation and Service Occupations",
                    "37" , "Building and Grounds Cleaning and Maintenance Occupations",
                    "39" , "Personal Care and Service Occupations",
                    "41" , "Sales and Related Occupations",
                    "43" , "Office and Administrative Support Occupations",
                    "45" , "Farming, Fishing, and Forestry Occupations",
                    "47" , "Construction and Extraction Occupations",
                    "49" , "Installation, Maintenance, and Repair Occupations",
                    "51" , "Production Occupations",
                    "53" , "Transportation and Material Moving Occupations",
                    "55" , "Military Occupations",
                    "0" , "Not Applicable")

soc_race <- soc_race %>% left_join(soc_codes)

soc_race$SOC_LABEL <-   sub("Occupations", "", soc_race$SOC_LABEL)
soc_race <- soc_race %>% filter(SOC_LABEL != "Unemployed", SOC_LABEL != "Not Applicable")

soc_plot <- ggplot(soc_race, aes(x= reorder(SOC_LABEL, -as.integer(SOC_CODE)), 
                                 y = RACE_HISP_SHARE, fill = RACE_HISP2))
soc_plot + geom_bar(stat = "identity", position = "stack") +theme_minimal() +
  labs(x = "Major Occupations", y = "Race/Ethnicity Share of Employment", 
       title = "Share of Workers by Race/Ethnicity in Major Occupations\n for the Chicago MSA, 2011-2015",
       caption = "Source: IPUMS-USA, Univeristy of Minnesota, American Community Survey 2011-2015") +
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black")) + coord_flip() +
scale_fill_ptol(name = "Race/Hispanic Ethnicity")

ggsave(filename = "05-soc_racehisp_stack.pdf", path = "./reports/plots", width = 8.5, height = 8)
