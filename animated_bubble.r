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
wdi <- mutate(wdi, year=as.numeric(str_sub(year, start = 2, end = 5)), alcohol=as.numeric(alcohol), suicide=as.numeric(suicide), population=as.numeric(population))

print(as.numeric(year))

p <- ggplot(wdi, aes(alcohol, suicide, size=population, color = continent)) +
  geom_point() +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'Alcohol consumption', y = 'Suicide mortality rate') +
  transition_time(year) +
  ease_aes('linear') +
  labs(caption = "in liters of pure alcohol per capita")

animate(plot=p, width = 600, height = 400, renderer = gifski_renderer())
anim_save("output.gif")
