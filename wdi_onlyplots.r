# general data prep
wdi <- read_excel("resources/Data pack/World_Development_Indicators.xlsx")
wdi <- data.frame(wdi)
wdi <- wdi %>% mutate_all(funs(replace(., .== "..", NA)))


#1 alcohol :P
wdi <- filter(wdi, Series.Name == "Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
wdi <- wdi[colSums(!is.na(wdi)) > 0]
wdi <- wdi %>% na.omit()
wdi <- subset(wdi, select=-c(Country.Code, Series.Name, Series.Code))
wdi <- head(wdi, 5)

wdi <- melt(wdi, id.vars='Country.Name', variable.name = 'year')

ggplot(wdi, aes(x=year, y=value)) + geom_point(aes(colour = Country.Name)) #+ geom_line()
