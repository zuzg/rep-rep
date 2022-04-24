# libraries
library(readxl)
library(dplyr)
library(countrycode)
library(highcharter)
library(ggthemes)

# 1 - data preprocessing
wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)

wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA))) %>%
  filter(Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- subset(wdi, select=-c(Series.Name, Series.Code)) %>%

wdi$continent <- countrycode(sourcevar = wdi[, "Country.Name"],
                            origin = "country.name",
                            destination = "continent")

wdi <- mutate_at(wdi, names(select(wdi, starts_with("X"))), as.numeric)
wdi <- mutate(wdi, MeanAlcoConsumption = rowMeans(select(wdi, starts_with("X")), na.rm = TRUE))


wdi <- mutate(wdi, Country.Name = recode(str_trim(Country.Name), 
                      "United States" = "United States of America",
                      "Russian Federation" = "Russia", 
                      "Venezuela, RB" = "Venezuela"))

# 1 MAP CHART
meandf <- select(wdi, -c(3:7)) %>%
  rename(country = Country.Name) %>%
  rename("iso-a3" = Country.Code) %>%
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
    text = "The results are averaged based on the measurements from the years: 2000, 2005, 2010, 2015, 2018.",
    style = list(fontSize = "10px")
  )
hc

# https://code.highcharts.com/mapdata/custom/world.js
# może da się jakoś po "iso-a3"? ale nie wychodziło mi...


# 2 SUICIDE VARIANCE (ALCOHOLIZED)
suicide <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
suicide <- data.frame(suicide) %>%
  mutate_all(funs(replace(., .== "..", NA))) %>%
  filter(Series.Name == "Suicide mortality rate (per 100,000 population)") %>%
  select(-c(Series.Name, Series.Code))
suicide <- suicide[colSums(!is.na(suicide)) > 0]
suicide <- na.omit(suicide)
suicide <- mutate_at(suicide, names(select(suicide, starts_with("X"))), as.numeric)
suicide <- mutate(suicide, Mean.Suicide.Rate = rowMeans(select(suicide, starts_with("X")), na.rm = TRUE))

suicide <- mutate(suicide, Country.Name = recode(str_trim(Country.Name), 
                                         "United States" = "United States of America",
                                         "Russian Federation" = "Russia", 
                                         "Venezuela, RB" = "Venezuela"))

df <- inner_join(wdi, suicide, by = "Country.Name") %>%
  select(c(Country.Name, Mean.Suicide.Rate, MeanAlcoConsumption)) %>%
  arrange(desc(MeanAlcoConsumption))
str(df)

df$suicide_rate_z <- round((df$Mean.Suicide.Rate - mean(df$Mean.Suicide.Rate))/sd(df$Mean.Suicide.Rate), 2)

df$suicide_rate_type <- ifelse(df$suicide_rate_z < 0, "below", "above")

df <- arrange(df, desc(MeanAlcoConsumption)) %>%
  head(30)
  #tail(30)

df <- df[order(df$suicide_rate_z), ]
df$Country.Name <- factor(df$Country.Name, levels = df$Country.Name)

ggplot(df, aes(x=Country.Name, y=suicide_rate_z, label=suicide_rate_z)) + 
  geom_bar(stat='identity', aes(fill=suicide_rate_type), width=.5)  +
  scale_fill_manual(name="Suicide rate:", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#e10404", "below"="#618c23")) + 
  labs(x = "", y = "Standardised suicide mortality rate", 
       title= "Suicide rate variance in TOP 30 alcoholized countries",
       caption="Based on the measurements from the years 2000-2019") + 
  coord_flip() +
  theme_tufte() +
  theme(
    panel.grid.major = element_line(colour = "grey", size=0.4),
    plot.caption = element_text(hjust = 0.5))

# 3 - correlation/streamgraph
