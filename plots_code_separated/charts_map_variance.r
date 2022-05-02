# libraries
library(readxl)
library(dplyr)
library(countrycode)
library(highcharter)
library(ggthemes)

# 1 - data preprocessing
wdi_base <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi_base <- data.frame(wdi_base)

wdi_base <- wdi_base %>% mutate_all(funs(replace(., .== "..", NA)))

alco_df<-filter(wdi_base, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
alco_df <- alco_df[colSums(!is.na(alco_df)) > 0]
alco_df <- subset(alco_df, select=-c(Series.Name, Series.Code)) %>%

alco_df$continent <- countrycode(sourcevar = alco_df[, "Country.Name"],
                            origin = "country.name",
                            destination = "continent")

alco_df <- mutate_at(alco_df, names(select(alco_df, starts_with("X"))), as.numeric)
alco_df <- mutate(alco_df, MeanAlcoConsumption = rowMeans(select(alco_df, starts_with("X")), na.rm = TRUE))

# 1 MAP CHART
meandf <- select(alco_df, -c(3:9)) %>%
  rename(country = Country.Name) %>%
  rename("iso3" = Country.Code)


options(highcharter.theme = hc_theme_tufte(tooltip = list(valueDecimals = 2)))
data(worldgeojson, package = "highcharter")

hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, meandf, value = 'MeanAlcoConsumption', 
    joinBy = 'iso3',
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


# 2 SUICIDE VARIANCE (ALCOHOLIZED)
suicide <- wdi_base %>%
  filter(Series.Name == "Suicide mortality rate (per 100,000 population)") %>%
  select(-c(Series.Name, Series.Code))
suicide <- suicide[colSums(!is.na(suicide)) > 0]
suicide <- na.omit(suicide)
suicide <- mutate_at(suicide, names(select(suicide, starts_with("X"))), as.numeric)
suicide <- mutate(suicide, Mean.Suicide.Rate = rowMeans(select(suicide, starts_with("X")), na.rm = TRUE))


df <- inner_join(alco_df, suicide, by = "Country.Name") %>%
  select(c(Country.Name, Mean.Suicide.Rate, MeanAlcoConsumption)) %>%
  arrange(desc(MeanAlcoConsumption))

df$suicide_rate_z <- round((df$Mean.Suicide.Rate - mean(df$Mean.Suicide.Rate))/sd(df$Mean.Suicide.Rate), 2)

df$suicide_rate_type <- ifelse(df$suicide_rate_z < 0, "below", "above")

alcoholized <- arrange(df, desc(MeanAlcoConsumption)) %>%
  head(30)
  #tail(30)

alcoholized <- alcoholized[order(alcoholized$suicide_rate_z), ]
alcoholized$Country.Name <- factor(alcoholized$Country.Name, levels = alcoholized$Country.Name)

ggplot(alcoholized, aes(x=Country.Name, y=suicide_rate_z, label=suicide_rate_z)) + 
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

#----------------------------------------------

nonalcoholized <- arrange(df, desc(MeanAlcoConsumption)) %>%
  tail(30)

nonalcoholized <- nonalcoholized[order(nonalcoholized$suicide_rate_z), ]
nonalcoholized$Country.Name <- factor(nonalcoholized$Country.Name, levels = nonalcoholized$Country.Name)

ggplot(nonalcoholized, aes(x=Country.Name, y=suicide_rate_z, label=suicide_rate_z)) + 
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
