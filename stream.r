library(ggradar)
library(tidyverse)
library(scales)
library(showtext)
library(readxl)
library(countrycode)

wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)

wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA))) %>%
  filter(Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)" | 
           Series.Name == "GDP per capita (current US$)"|
           Series.Name == "Unemployment, total (% of total labor force) (national estimate)" |
           Series.Name == "Life expectancy at birth, total (years)"|
           Series.Name == "CO2 emissions (kt)")
wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- subset(wdi, select=-c(Series.Code, Country.Code))
  
wdi$continent <- countrycode(sourcevar = wdi[, "Country.Name"],
                               origin = "country.name",
                               destination = "continent")

wdi <- mutate_at(wdi, names(select(wdi, starts_with("X"))), as.numeric)
wdi <- mutate(wdi, Mean.Val = rowMeans(select(wdi, starts_with("X")), na.rm = TRUE)) %>%   subset(select=c(continent, Country.Name, Series.Name, Mean.Val)) %>%
  spread(key=Series.Name, value=Mean.Val) %>%
  subset(select=-Country.Name)

wdi <- rename(wdi, c(
                  Alcohol = "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)",
                  GDP ="GDP per capita (current US$)", 
                  Unemployment = "Unemployment, total (% of total labor force) (national estimate)",
                  Life.Exp = "Life expectancy at birth, total (years)",
                  CO2 = "CO2 emissions (kt)"
                  ))

wdi_grouped <- wdi %>%
  drop_na() %>%
  group_by(continent) %>%
  summarise(
    GDP = mean(GDP, na.rm=TRUE),
    'Life expectancy' = mean(Life.Exp, na.rm=TRUE),
    'Unemployment rate' = mean(Unemployment, na.rm=TRUE),
    'Alcohol consumption' = mean(Alcohol, na.rm=TRUE),
    'CO2\nconsumption' = mean(CO2, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate_at(vars(-continent), rescale)


# https://r-graph-gallery.com/web-radar-chart-with-R.html
plt <- wdi_grouped %>%
  ggradar(
    plot.title = "Comparison of the socio-economic factors of continents",
    legend.title = "Continents:",
    grid.label.size = 5,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 5, # Afftects the names of the variables
    group.point.size = 3,
    background.circle.transparency = 0
  )
plt
