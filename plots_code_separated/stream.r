
library(tidyverse)
library(scales)
library(readxl)
library(countrycode)
library(dplyr)

wdi_base <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi_base <- data.frame(wdi_base)

wdi_base <- wdi_base %>% mutate_all(funs(replace(., .== "..", NA)))
radar_df <- filter(wdi_base, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)" | 
           Series.Name == "GDP per capita (current US$)"|
           Series.Name == "Unemployment, total (% of total labor force) (national estimate)" |
           Series.Name == "Life expectancy at birth, total (years)"|
           Series.Name == "CO2 emissions (kt)")
radar_df <- radar_df[colSums(!is.na(radar_df)) > 0]
radar_df <- subset(radar_df, select=-c(Series.Code, Country.Code))
  
radar_df$continent <- countrycode(sourcevar = radar_df[, "Country.Name"],
                               origin = "country.name",
                               destination = "continent")

radar_df <- mutate_at(radar_df, names(select(radar_df, starts_with("X"))), as.numeric)
radar_df <- mutate(radar_df, Mean.Val = rowMeans(select(radar_df, starts_with("X")), na.rm = TRUE)) %>%   subset(select=c(continent, Country.Name, Series.Name, Mean.Val)) 
radar_df <- spread(radar_df, key=Series.Name, value=Mean.Val) %>%
  subset(select=-Country.Name)

radar_df <- rename(radar_df, c(
                  Alcohol = "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)",
                  GDP ="GDP per capita (current US$)", 
                  Unemployment = "Unemployment, total (% of total labor force) (national estimate)",
                  Life.Exp = "Life expectancy at birth, total (years)",
                  CO2 = "CO2 emissions (kt)"
                  ))

rescale100 <- function(x) (x-min(x))/(max(x) - min(x)) * 100

wdi_grouped <- radar_df %>%
  drop_na() %>%
  group_by(continent) %>%
  summarise(
    GDP = mean(GDP, na.rm=TRUE),
    'Lifespan' = mean(Life.Exp, na.rm=TRUE),
    'Unemployment' = mean(Unemployment, na.rm=TRUE),
    'Alcohol' = mean(Alcohol, na.rm=TRUE),
    'CO2' = mean(CO2, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate_at(vars(-continent), rescale100) %>%
  gather(key=Factor, value=Value, 2:6)


continent_colors <- c('#e10404', '#fe5000', '#ffbf00', '#028900', '#0392cf')


g<-wdi_grouped %>%
  ggplot(aes(
    x = Factor,
    y = Value,
    color = continent,
    group = continent
  )) +
  geom_polygon(size = 1, alpha = .0) +
  coord_radar() +
  scale_color_manual(values=continent_colors) +
  theme_radar(base_family = "serif") +
  labs(x = "", y="scale [%]", title="Socio-economic factors of continents") +
  theme(plot.title = element_text(hjust = 0.5))
g
