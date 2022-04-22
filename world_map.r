library(readxl)
library(countrycode)
library(dplyr)
library(tidyverse)
library(highcharter)


wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)
wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA)))

wdi <- filter(wdi, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- wdi %>% na.omit()
wdi <- subset(wdi, select=-c(Country.Code, Series.Name, Series.Code))
#wdi <- head(wdi, 5)

wdi$continent <- countrycode(sourcevar = wdi[, "Country.Name"],
                            origin = "country.name",
                            destination = "continent")

wdi <- mutate_at(wdi, names(select(wdi, starts_with("X"))), as.numeric)
wdi <- mutate(wdi, MeanAlcoConsumption = rowMeans(select(wdi, starts_with("X")), na.rm = TRUE))
wdi <- select(wdi, -c(2:6)) %>%
  rename(country = Country.Name) %>%
  as_tibble()

# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
data(worldgeojson, package = "highcharter")

hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, wdi, value = 'MeanAlcoConsumption', joinBy = c('name','country'),
    name = "Alco consumption per capita"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "World Map") %>% 
  hc_subtitle(text = "Mean Alco Consumption")

hc





