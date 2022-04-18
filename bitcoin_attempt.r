# Nie chcia³am robiæ ba³aganu w onlyplots...
# Note: dodawajmy mo¿e ju¿ od razu biblioteki tak ¿eby do raportu dodaæ

# to make it more understandable (full description in metadata file)
# DIFF - Bitcoin Difficulty
# HRATE - Bitcoin Hash Rate
# MKPRU - Bitcoin Market Price USD
# TRVOU - Bitcoin USD Exchange Trade Volume

# LIBRARIES
library(dplyr)
library(lubridate) #to extract year
library(tidyverse) #to extract month-year
library(plotly)

# DATA PREPROCESSING
DIFF <- data.frame(read.csv("resources/Data pack/Bitcoin/BCHAIN-DIFF.csv")) %>% 
  dplyr::rename(Difficulty = Value)

HRATE <- data.frame(read.csv("resources/Data pack/Bitcoin/BCHAIN-HRATE.csv")) %>% 
  dplyr::rename(Hash_rate = Value)

MKPRU <- data.frame(read.csv("resources/Data pack/Bitcoin/BCHAIN-MKPRU.csv")) %>% 
  dplyr::rename(Market_price = Value) %>%
  mutate(Market_price = round(Market_price ,digit=2))

TRVOU <- read.csv("resources/Data pack/Bitcoin/BCHAIN-TRVOU.csv") %>% 
  dplyr::rename(Trading_volume_value = Value)

BITCOIN <- inner_join(DIFF, HRATE, by = 'Date') %>%
  inner_join(MKPRU, by = 'Date') %>%
  inner_join(TRVOU, by = 'Date') %>%
  mutate(Year = year(Date),
         Date = ymd(Date),
         Month_year = format_ISO8601(Date, precision = "ym"))

# BITCOIN aggregated by Year/Month_year - you choose hihi (NOT USED YET!)
BITCOIN_AGG <- group_by(BITCOIN, Year) %>%
  dplyr::summarise(Difficulty = mean(Difficulty), 
                   Hash_rate = mean(Hash_rate),
                   Market_price = mean(Market_price),
                   Trading_volume_value = mean(Trading_volume_value))

# Note: nie zaczyna³abym pomiarów od samego pocz¹tku bo nic interesuj¹cego siê tam nie dzieje tbh
BITCOIN <- dplyr::filter(BITCOIN, Date >= "2015-01-01")

# SOME PLOTS
#1
gp <- ggplot(BITCOIN, aes(Date, Market_price)) +
  geom_line(color = "maroon") +
  labs(title = "Bitcoin Market Price (USD) changes over time", 
       x = "", y = "Price [$]")
gp <- ggplotly(gp)
gp
# gp <- blabla...
# TODO - Market_price ma byæ "Market price" po najechaniu

#2
gp <- ggplot(BITCOIN, aes(Date, Difficulty)) +
  geom_line(color = "darkgreen") +
  labs(title = "Difficulty of finding a new block of Bitcoin", 
       x = "", y = "Difficulty level")
gp <- ggplotly(gp)
gp

#3 and 4 - Trading_volume_value and Hash_rate respactively...

# TODO - combine several columns, possibly normalize/scale the data, add sliders -> myœlê ¿e wtedy jd albo np jakby jakieœ tickery mo¿na by³o daæ 

