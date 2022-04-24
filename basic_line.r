# libs
library(reshape2)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggforce)

# general data prep
wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)
wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA)))

wdi <- filter(wdi, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
wdi <- filter(wdi, Country.Code %in% c("HIC", "LIC", "LMC", "LMY", "MIC", "WLD"))


wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- wdi %>% na.omit()
wdi <- subset(wdi, select=-c(Country.Code, Series.Name, Series.Code))
wdi <- melt(wdi, id.vars='Country.Name', variable.name = 'year')

p <- ggplot(wdi, aes(x=year, y=as.numeric(value), color=Country.Name)) +
  geom_point() +
  geom_line(aes(group=Country.Name)) +
  labs(title="Alcohol consumption in countries", x="year", y="litres of alcohol per capita") +
  scale_x_discrete(labels=c("2000", "2005", "2010", "2015", "2018"))
p
