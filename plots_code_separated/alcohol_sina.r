# libs
library(reshape2)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggforce)
library(countrycode)
library(plotly)
library(ggthemes)

# general data prep
wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)
wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA)))

wdi <- filter(wdi, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
wdi <- filter(wdi, !Country.Code %in% c("HIC", "LIC", "LMC", "LMY", "MIC", "UMC", "WLD"))
wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- wdi %>% na.omit()
wdi <- subset(wdi, select=-c(Country.Code, Series.Name, Series.Code))
wdi <- melt(wdi, id.vars='Country.Name', variable.name = 'year')

wdi$continent <- countrycode(sourcevar = wdi[, "Country.Name"],
                             origin = "country.name",
                             destination = "continent")

wdi <- mutate(wdi, value=as.numeric(value))

p <- ggplot(wdi, aes(x=year, y=value, group=year, label=Country.Name)) +
  geom_violin() +
  geom_sina(aes(colour=continent)) +
  labs(title="Distribution of alcohol consumption across countries", x="year", y="litres of alcohol per capita") +
  scale_x_discrete(labels=c("2000", "2005", "2010", "2015", "2018")) +
  theme_tufte(base_size = 16)
p
ggplotly(p)
