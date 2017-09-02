if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(readr, stringr, purrr, sf, tidycensus, tigris, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

source("./scripts/lehd_functions.R")

#load census API key and variable key
#acs_key <- census_api_key("c67f1b3134300374d51a55c543649f843fb7d2b3",install = TRUE)
acs_key <- Sys.getenv("CENSUS_API_KEY")
vartabacs <- load_variables(year = 2015, "acs5",  cache = TRUE)

#downloading 2014 IL, IN, WI workplace and residential association file for primary jobs making over-----
#$3300/month

led_states <- c("il", "in", "wi")

chi_wac <- map_df(led_states, grab_wac, segment = "SE03", jobtype = "JT01", tract = TRUE)
chi_rac <- map_df(led_states, grab_rac, segment = "SE03", jobtype = "JT01", tract = TRUE)


#grab total jobs, jobs over $3300/month and jobs held by whites, blacks, hispanics

chi_rac <- chi_rac %>% select(1, 2, 8,29, 30, 36)

chi_rac <- chi_rac %>% rename(TotJobs_RC = C000, Jobs_greater_3300_RC = CE03, WhiteWorkers_RC = CR01,
                            BlackWorkers_RC = CR02, HispanicWorkers_RC = CT02)

chi_wac <- chi_wac %>% select(1, 2, 8, 29, 30, 36) 

chi_wac <- chi_wac %>% rename(TotJobs_WC = C000, Jobs_greater_3300_WC = CE03, WhiteWorkers_WC = CR01,
                            BlackWorkers_WC = CR02, HispanicsWorkers_WC = CT02)

#grab Greg's ACS data race/ethnicty, % af-am, % poc, and poverty------
il_tracts <- get_acs(geography = "tract", variables = c("B01003_001", "B01001A_001","B01001B_001",
                                                        "B17001_001", "B17001_002"), 
                     state = c("IL", "IN", "WI"), geometry = TRUE, key = acs_key, output = "wide")
#this is for 2015 cbsa boundary
chi_msa <- core_based_statistical_areas() %>% 
  filter(NAME == "Chicago-Naperville-Elgin, IL-IN-WI") %>% select(NAME)

#make chi tracts
chi_tracts <- st_join(il_tracts, chi_msa, join = st_within, left = FALSE,
                      suffix = c("_tracts", "_MSA"))

chi_tracts <- left_join(chi_tracts, chi_wac, by = c("GEOID" = "tract_id"))
chi_tracts <- left_join(chi_tracts, chi_rac, by = c("GEOID" = "tract_id"))

chi_tracts <- st_as_sf(chi_tracts)

#st_write(chi_tracts, "./data/09-01-2017_chi_led_acs_tracts.geojson")

rm(list = c("chi_msa", "il_tracts",  "vartabacs"))