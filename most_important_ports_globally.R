# The purpose of this project is to look at 2014 data on ports in terms of
# the best way to display the busiest ports in terms of graphs and maps

# IMPORT PACKAGES

library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(tidygeocoder)
library(maps)

#=============
# GET THE DATA
#=============

url.world_ports <- url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/datasets/world_ports.RData")

load(url.world_ports)

glimpse(df.world_ports)

#=========================================
# CREATE THEMES
# We'll create two themes:
#
# 1. theme.porttheme
#    - this will be a general theme that
#      we'll apply to most of our charts
#      to format the text, titles, etc
#
# 2. theme.smallmult
#    - we'll apply this exclusively to 
#      "small multiple" charts 
#      (AKA, trellis charts).  We need this
#      because the axis text needs to be 
#      smaller for the small multiples
#=========================================

#----------------------------------------
# GENERAL THEME
# - we'll use this for most of our charts
#   and build on it when we need to
#----------------------------------------

theme.porttheme <-  
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank()) 

#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
# - there are several bar charts that 
#   are very wide, and need some
#   special formatting
#------------------------------------

theme.widebar <-
  theme.porttheme +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(legend.title = element_blank(), legend.background = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = c(.9,.55)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
#  - we'll use this for small multiple 
#    charts.  these also have some
#    special formatting requirements
#------------------------------------

theme.smallmult <- 
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))


#----------------------------------------------------
# BAR CHART: Port vs Volume (2014)
# - this is the "long" form of the bar chart.
# - it's harder to read, but we can fit more data
# - it also shows the uneven distribution of shipping
#   volume
#----------------------------------------------------

df.world_ports %>%
  filter(year == 2014) %>%
  ggplot(aes(x = reorder(port_label, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", fill = "dark red") +
  labs(title = "Busiest container ports in the world") +
  labs(subtitle = '2014, in order of shipping volume') +
  labs(x = "Port", y = "Shipping\nVolume") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme.widebar 

#----------------------------------------
# FLIPPED BAR CHART: Top 25 busiest ports
# - this is useful because it makes the 
#   chart more readable when we flip
#   the axes
# - use top 25 so you can read names
#----------------------------------------

df.world_ports %>%
  filter(year == 2014, rank <= 25) %>%
  ggplot(aes(x = reorder(port, volume), y = volume)) +
  geom_bar(stat = "identity", fill = "dark red") +
  geom_text(aes(label = volume), hjust = 1.1, color = "#FFFFFF") +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip() +
  labs(title = "Shanghai, Singapore had much higher volume\nthan other high-volume ports in 2014") +
  labs(x = "Port", y = "Shipping Volume\n(1000 TEUs)") +
  theme.porttheme

#==========================
# BAR CHART: Ports in China
# = use mutate and ifelse() to divide data into China and not China
#========================== 

df.world_ports %>%
  mutate(china_flag = ifelse(economy == "China","China","Not China")) %>%
  filter(year == 2014) %>%
  ggplot(aes(x = reorder(port_label, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", aes(fill = china_flag)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("dark red","#999999")) +
  labs(title = "Roughly 20% of busiest ports were\nin China in 2014") +
  labs(x = "Port", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme.widebar 

#==========================
# BAR CHART: Ports in Asia
# = use mutate and ifelse() to divide into Aisa and non-Asia
#========================== 

df.world_ports %>% 
  mutate(asia_flag = ifelse(continent == "Asia","Asia","Other")) %>%
  filter(year == 2014) %>%
  ggplot(aes(x = reorder(port_label, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", aes(fill = asia_flag)) +
  scale_fill_manual(values = c("dark red","#999999")) +
  labs(title = "More than half of the busiest ports were in Asia in 2014") +
  labs(x = "Port", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme.widebar

#========================================================
# SMALL MULTIPLE, LINE: All ports, shipping vol over time
#  - This is useful for getting a new overview of the
#    data
#========================================================

df.world_ports %>%
  ggplot(aes(x = year, y = volume, group = port_label)) +
  geom_line(color = "dark red", size = 1, na.rm = T) +
  facet_wrap(~ port_label) +
  labs(title = "Strong growth in Shanghai, Singapore,\nShenzhen, Guangzhou") +
  labs(subtitle = "2004 to 2014") +
  labs(x = "Port", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme.smallmult

#================================================
# LINE CHART: Shanghai, Volume change over time
#  - Shanghai volume has increased substantially
#    so we want to show it visually
#================================================

df.world_ports %>%
  mutate(port_highlight = ifelse(port == "Shanghai","Shanghai","Other")) %>%
  ggplot(aes(x = year, y = volume, group = port)) +
  geom_line(aes(color = port_highlight, alpha = port_highlight), size = 1.5, na.rm = T) +
  scale_color_manual(values = c("#999999","dark red")) + 
  scale_alpha_manual(values = c(.3,1)) +
  labs(title = "Shanghai's shipping volume increased\nsubstantially from 2004 to 2014") +
  labs(x = "Year", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme.porttheme

#===============
# PLOT SINGAPORE
#===============

df.world_ports %>%
  filter(port == "Singapore") %>%
  ggplot(aes(x = year, y = volume, group = 1)) +
  geom_line(color = "dark red", size = 2) +
  labs(title = "Singapore volume also increased\nsubstantially from 2004 to 2014") +
  labs(x = "Year", y = "Shipping\nVolume\n(1000 TEUs)") +
  scale_y_continuous(limits = c(0,NA)) +
  theme.porttheme

#===================================
# SMALL MULTIPLE: Rank over time
# - We'll use this to show
#   the rank changes of all of the
#   ports
# - Given the large number of ports
#   the data will be much easier to
#   read in a small multiple
#===================================

df.world_ports %>%
  ggplot(aes(x = year, y = rank, group = port_label)) +
  geom_line(size = 1, color = "dark red", na.rm = T) +
  scale_y_reverse() +
  facet_wrap(~ port_label) +
  labs(title = "Ranking over time of world's busiest ports") +
  labs(subtitle = "2004 to 2014") +
  labs(x = "Year", y = "Rank") +
  theme.smallmult

#============================
# BUMP CHART: CHINA
# here, we'll highlight China
# creating a variable called china_labels. china_labels 
# will enable us to individually color each line for the different Chinese ports 
# (we do this in conjunction with scale_color_manual()).
# We're also going to modify the transparency of the lines. 
# We'll set the Chinese lines to almost fully opaque, 
# and set the non-Chinese lines to be highly transparent. T
#============================

param.rank_n = 15

df.world_ports %>%
  filter(rank <= param.rank_n) %>%
  mutate(china_flag = ifelse(economy == "China", T,F)) %>%
  mutate(china_labels = ifelse(china_flag == T, port,"other")) %>%
  ggplot(aes(x = year, y = rank, group = port_label)) +
  geom_line(aes(color = china_labels, alpha = china_flag), size = 2) +
  geom_point(aes(color = china_labels, alpha = china_flag), size = 2.3) +
  geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
  geom_text(data = df.world_ports %>% filter(year == "2014", rank <= param.rank_n), aes(label = port_label, x = '2014') , hjust = -.05, color = "#888888", size = 4) +
  geom_text(data = df.world_ports %>% filter(year == "2004", rank <= param.rank_n), aes(label = port_label, x = '2004') , hjust = 1.05, color = "#888888", size = 4) +
  scale_x_discrete(expand = c(.3, .3)) +
  scale_y_reverse(breaks = c(1,5,10,15)) +
  scale_alpha_discrete(range = c(.4,.9)) +
  labs(title = "Top Chinese ports increased rank\nsubstantially from 2004 to 2014") +
  labs(subtitle = "(Port ranks, by volume)") +
  labs(x = "Year", y = "Rank") +
  theme.porttheme +
  theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +  
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#4e79a5","#f18f3b","#af7a0a","#e0585b","#5aa155","#edc958","#77b7b2","#BBBBBB"))

#=============
# GET MAP DATA
#=============

map.world_polygon <- map_data("world")
head(map.world_polygon)

#=====================================
# SIMPLE DOT DISTRIBUTION MAP
#  - This will be useful just to see
#    the data
#  - It also serves as a good test
#    for the more complex chart we're
#    going to make next
#=====================================

df.world_ports %>%
  filter(year == "2014") %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
  geom_point(color = "red")

#=========================
# BUBBLE DISTRIBUTION MAP
#=========================

# CREATE THEME

theme.maptheeme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(panel.background = element_rect(fill = "#FCFCFF")) +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = c(.17,.35)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 10)) 

#==============================================
# GEOSPATIAL BUBBLE
# - This will give us a sense
#   of the density of shipping in a particular
#   geographic region
#  
#==============================================

df.world_ports %>% 
  filter(year == "2014") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group),fill = "#AAAAAA",colour = "#818181", size = .15) +
  geom_point(aes(size = volume), color = "#DD0000", alpha = .15) +                    
  geom_point(aes(size = volume), color = "#DD0000", alpha = .7, shape = 1) +
  scale_size_continuous(range = c(.2,10), breaks = c(5000, 10000, 30000), name = "Shipping Volume\n(1000 TEUs)") +
  #coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") + # use robinson projection
  labs(title = "High volume ports were highly clustered in\nChina and Asia in 2014") +
  theme.maptheeme


