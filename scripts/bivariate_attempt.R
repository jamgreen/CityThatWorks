#bivariate choropleth following kiefer:http://lenkiefer.com/2017/04/24/bivariate-map
devtools::install_github("tidyverse/ggplot2")
library(ggplot2)

library(tidyverse)
library(stringr)
library(sf)
library(viridis)


chi_tracts <- st_read("./data/09-09-2017_chi_led_acs_tracts.geojson")

d <- expand.grid(x = 1:100, y = 1:100)

ggplot(d, aes(x,y,fill= atan(y/x), alpha = x+y)) +
  geom_tile() +
  scale_fill_viridis() +
  theme(legend.position = "none", 
        panel.background = element_blank()) +
  labs(title = "A Bivariate color scheme (Virdis)")

#Add somelabels

d <- expand.grid(x = 1:3, y = 1:3)
d <- merge(d, data.frame(x = 1:3, xlabel = c("X Low", "X Middle", "X High")), by = "x")
d <- merge(d, data.frame(y = 1:3, ylabel = c("Y Low", "Y Middle", "Y High")), by = "y")

g.legend<-
  ggplot(d, aes(x,y,fill=atan(y/x),alpha=x+y,label=paste0(xlabel,"\n",ylabel)))+
  geom_tile()+
  geom_text(alpha=1)+
  scale_fill_viridis()+
  theme_void()+
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10))+
  labs(title="A bivariate color scheme (Viridis)",x="X",y="Y")+
  theme(axis.title=element_text(color="black"))+
  # Draw some arrows:
  geom_segment(aes(x=1, xend = 3 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=1, yend = 3), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) 
g.legend

#compute black and poc percentage and terciles for them
chi_tracts$BlackPer <- chi_tracts$BlackAlone/chi_tracts$TotPop
chi_tracts$POCPer <-  (chi_tracts$TotPop- chi_tracts$WhiteAlone)/chi_tracts$TotPop
chi_tracts$PovPer <- chi_tracts$InPoverty/chi_tracts$PovStatus

#get terciles
chi_tracts$black.v <- quantile(chi_tracts$BlackPer, c(.33, .66, 1), na.rm = TRUE)
chi_tracts$poc.v <- quantile(chi_tracts$POCPer, c(.33, .66, 1), na.rm = TRUE)
chi_tracts$pov.v <- quantile(chi_tracts$PovPer, c(.33, .66, 1), na.rm = TRUE)


black.v <- quantile(chi_tracts$BlackPer, c(0.33, 0.66, 1), na.rm = TRUE)
poc.v <- quantile(chi_tracts$POCPer, c(0.33, 0.66, 1), na.rm = TRUE)
pov.v <- quantile(chi_tracts$PovPer, c(0.33, 0.66, 1), na.rm = TRUE)
highearning.v <- quantile(chi_tracts$hI_JobsWC, c(0.33, 0.66, 1), na.rm = TRUE)

chi_tracts <- chi_tracts %>% mutate(Y_Black = ifelse(BlackPer < black.v[1], 1,
                                                     ifelse(BlackPer < black.v[2], 2, 3)),
                                    Y_POC = ifelse(POCPer < poc.v[1], 1,
                                                   ifelse(POCPer < poc.v[2], 2, 3)),
                                    Y_Pov = ifelse(PovPer < pov.v[1], 1,
                                                   ifelse(PovPer < pov.v[2], 2, 3)),
                                    Y_HigherEarning = ifelse(hI_JobsWC < highearning.v[1], 1,
                                                             ifelse(hI_JobsWC < highearning.v[2], 2, 3)))

#filter for a close in look at Cook and Dupage Counties
county_tracts <- chi_tracts %>% mutate(CountyFIPs = str_sub(GEOID, 1, 5)) %>% 
  filter(CountyFIPs %in% c("17031", "17043"))

#create new legend and map

g.legendBlack <- ggplot(d, aes(x, y, fill = atan(y/x),
                  alpha = x + y, label = paste0(xlabel, "\n", ylabel))) +
  geom_tile() + scale_fill_viridis() + theme_void() +
  theme(legend.position = "none", axis.title = element_text(size = 5),
        panel.background = element_blank(), plot.margin = margin(t = 10, b = 10, l = 10)) +
  theme(axis.title = element_text(color = "black", size = 14)) +
  labs(x = "Number of Jobs Earning $3,330/month or more",
       y = "Share of Population that is Black (%)") +
  # Draw some arrows:
  geom_segment(aes(x=1, xend = 3 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=1, yend = 3), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) 

#big msa map
gmap <- ggplot(chi_tracts, aes(fill = atan(Y_Black/Y_HigherEarning),alpha = Y_HigherEarning + Y_Black)) + 
  geom_sf() + coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14, margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis() + guides(alpha = FALSE, fill = FALSE) +
  labs(caption = "@surlyurbanist Source: U.S. Census Bureau ACS and LEHD",
       title = "High Earning Jobs and Black Population for 2014",
       subtitle = "Bivariate Choropleth") + theme_minimal()

#Dupage And Cook County Map
#load the county outlines
require(tigris)
options(tigris_class = "sf", tigris_use_cache = TRUE)
cook_dupage <- counties(state = "il") %>% filter(GEOID %in% c("17031", "17043")) %>% 
  select(GEOID, NAME)

cmap <- ggplot(county_tracts, aes(fill = atan(Y_Black/Y_HigherEarning),alpha = Y_HigherEarning + Y_Black)) + 
  geom_sf() + coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14, margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis() + guides(alpha = FALSE, fill = FALSE) +
  labs(caption = "@surlyurbanist Source: U.S. Census Bureau ACS and LEHD",
       title = "High Earning Jobs and Black Population for 2014",
       subtitle = "Bivariate Choropleth") + theme_minimal()


print(gmap)
print(g.legendBlack + labs(title = ""))
