#attempting to make some pretty maps using tmap with the LEHD and ACS info

if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(readr, sf, tigris, tmap, tmaptools, viridis, tidycensus, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

source("./scripts/process_lehd_acs.R")
#load census API key and variable key
#acs_key <- census_api_key("c67f1b3134300374d51a55c543649f843fb7d2b3",install = TRUE)
acs_key <- Sys.getenv("CENSUS_API_KEY")
vartabacs <- load_variables(year = 2015, "acs5",  cache = TRUE)



chi_tracts <-  chi_tracts %>% mutate(WhitePer = B01001A_001E/B01003_001E,
                                     BlackPer = B01001B_001E/B01003_001E,
                                     POCPer = (B01003_001E - B01001A_001E)/B01003_001E,
                                     PovPer = B17001_002E/B17001_001E)



