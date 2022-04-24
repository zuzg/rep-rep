library(readxl) #map chart
library(dplyr)  #map chart
library(countrycode) #map chart
library(highcharter) #map chart

wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)

wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA))) %>%
  filter(Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- wdi %>% na.omit() %>%
  subset(select=-c(Country.Code, Series.Name, Series.Code)) %>%
  mutate

wdi$continent <- countrycode(sourcevar = wdi[, "Country.Name"],
                            origin = "country.name",
                            destination = "continent")

wdi <- mutate_at(wdi, names(select(wdi, starts_with("X"))), as.numeric)
wdi <- mutate(wdi, MeanAlcoConsumption = rowMeans(select(wdi, starts_with("X")), na.rm = TRUE))
meandf <- select(wdi, -c(2:6)) %>%
  rename(country = Country.Name) %>%
  as_tibble()

options(highcharter.theme = hc_theme_google(tooltip = list(valueDecimals = 2)))
data(worldgeojson, package = "highcharter")

hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, meandf, value = 'MeanAlcoConsumption', joinBy = c('name','country'),
    name = "Liters of pure alcohol"
  )  %>% 
  hc_colorAxis(minColor = "#ffff4d",
               maxColor = "red") %>% 
  hc_title(
    text = "World alcohol consumption",
    style = list(fontWeight = "bold", fontSize = "25px")
  ) %>% 
  hc_subtitle(text = "average annual alcohol consumption per capita") %>%
  hc_caption(
    enabled = TRUE, 
    text = "The results are averaged based on the measurements from the years: 2000, 2005, 2010, 2018.",
    style = list(fontSize = "10px")
  )
hc