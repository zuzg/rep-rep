# libs
library(reshape2)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggforce)
library(countrycode)
library(tidyr)
library(tidyverse)
library(gifski)
library(gganimate)

wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)
wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA)))

wdi <- filter(wdi, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)" |
                Series.Name == "Suicide mortality rate (per 100,000 population)" |
                Series.Name == "Urban population")
wdi <- subset(wdi, select=c(Country.Name, Series.Name, X2000..YR2000.:X2018..YR2018.))

wdi <- wdi %>% gather(year, val, X2000..YR2000.:X2018..YR2018.)

wdi <- spread(wdi, Series.Name, val)

wdi <- rename(wdi, alcohol="Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)", 
              suicide="Suicide mortality rate (per 100,000 population)",
              population="Urban population")
wdi$continent <- countrycode(sourcevar = wdi[, "Country.Name"],
                             origin = "country.name",
                             destination = "continent")
wdi <- wdi %>% na.omit()
wdi <- mutate(wdi, year=as.integer(str_sub(year, start = 2, end = 5)),
              alcohol=as.numeric(alcohol),
              suicide=as.numeric(suicide),
              population=as.numeric(population))

p <- ggplot(wdi, aes(alcohol, suicide, size=population, color=continent)) +
  geom_point() +
  theme_bw() +
  labs(title = "Correlation between alcohol consumption and suicides in time",
       subtitle = 'Year: {frame_time}',
       x = 'Alcohol consumption',
       y = 'Suicide mortality rate',
       caption = "in liters of pure alcohol per capita") +
  transition_time(as.integer(year)) +
  ease_aes('linear') +
  scale_size_continuous(range = c(2, 12)) +
  theme_tufte(base_size = 18) +
  theme(panel.grid.major = element_line(colour = "grey", size=0.4))

animate(plot=p, height=500, width=700, renderer = gifski_renderer(), end_pause=2)
anim_save("bubble.gif")
