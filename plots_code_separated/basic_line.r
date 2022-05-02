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
wdi <- filter(wdi, Country.Code %in% c("HIC", "UMC","LIC", "LMC", "LMY", "MIC",  "WLD"))



wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- wdi %>% na.omit()
wdi <- subset(wdi, select=-c(Country.Code, Series.Name, Series.Code))
wdi <- melt(wdi, id.vars='Country.Name', variable.name = 'year')

p <- ggplot(wdi, aes(x=year, y=as.numeric(value), color=Country.Name)) +
  geom_point() +
  geom_line(aes(group=Country.Name), size=1.5) +
  labs(title="Alcohol consumption by income", x="year", y="litres of alcohol per capita") +
  scale_x_discrete(labels=c("2000", "2005", "2010", "2015", "2018")) +
  theme_tufte(16) +
  scale_color_manual(values=c('#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','Black'),
                     breaks = c("High income", "Upper middle income", "Middle income",
                                "Low & middle income","Lower middle income", "Low income","World"))

p
