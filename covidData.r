covidData <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", stringsAsFactors = TRUE,fileEncoding = "UTF-8-BOM")
covidData <- na.omit(covidData)
deaths <- covidData$deaths
month <- covidData$month
population <- covidData$popData2019
countries <- covidData$countriesAndTerritories
cases <- covidData$cases

#1 Descriptive Statistics- Details of COVID and the increase/spread every month for every country
summary(covidData)
str(covidData)

deathsPerMonth<-aggregate(deaths~month, FUN = sum)$deaths
casesPerMonth<-aggregate(cases~ month, FUN = sum)$cases
deathsPerMonthPerCountry<-aggregate(deaths~month + countries, FUN = sum)
casesPerMonthPerCountry<-aggregate(cases~ month + countries, FUN = sum)


plot(aggregate(deaths,by=list(month), FUN = sum)$Group.1,cumsum(deathsPerMonth), main = 'Total Deaths Jan 1, 2020- August 21, 2020',xlab = 'Month', ylab='Total Deaths', type ='l', col ='red')
plot(aggregate(cases,by=list(month), FUN = sum)$Group.1,cumsum(casesPerMonth),  main = 'Total Cases Jan 1, 2020- August 21, 2020',xlab = 'Month', ylab='Total Cases', type = 'l',col='red')
qqnorm(deaths)

sd(deaths)
range(deaths)
var(deaths)
skewness(deaths)
kurtosis(deaths)

sd(cases)
range(cases)
var(cases)
skewness(cases)
kurtosis(cases)

quantile(deaths, probs = seq(0.10,1, by = 0.10))
quantile(cases, probs = seq(0.10,1, by = 0.10))


#2- Inferential Statistics- Italy and Croatia. Ho = Croatia and Italy have the same average incidence (cases) and average fatality rates. Ha= Do not have the same
Italy <- covidData[covidData$countriesAndTerritories=='Italy',]
Croatia <- covidData[covidData$countriesAndTerritories=='Croatia',]

populationMeanDeathsItaly <- mean(Italy$deaths)
populationMeanCasesItaly <- mean(Italy$cases)
varDeathsItaly <- var(Italy$deaths)
varCasesItaly <- var(Italy$cases)

populationMeanDeathsCroatia <- mean(Croatia$deaths)
populationMeanCasesCroatia <- mean(Croatia$cases)
varDeathsCroatia <- var(Croatia$deaths)
varCasesCroatia <- var(Croatia$cases)


zscore <-round(qnorm(0.975),2)

zscoreDeaths <- round(((populationMeanDeathsItaly-populationMeanDeathsCroatia)) / sqrt((varDeathsItaly/length(Italy)) + (varDeathsCroatia/length(Croatia))),2)
zscoreCases <- round(((populationMeanCasesItaly-populationMeanCasesCroatia)) / sqrt((varCasesItaly/length(Italy)) + (varCasesCroatia/length(Croatia))),2)

confidenceIntervalLowerBoundDeaths <- round((populationMeanDeathsItaly-populationMeanDeathsCroatia) - zscore * sqrt((varDeathsItaly/length(Italy)) 
                                                                                                + (varDeathsCroatia/length(Croatia))),2)

confidenceIntervalUpperBoundDeaths <- round((populationMeanDeathsItaly-populationMeanDeathsCroatia) + zscore * sqrt((varDeathsItaly/length(Italy)) 
                                                                                                  + (varDeathsCroatia/length(Croatia))),2)

confidenceIntervalLowerBoundCases <- round((populationMeanCasesItaly-populationMeanCasesCroatia) - zscore * sqrt((varCasesItaly/length(Italy)) 
                                                                                                        + (varCasesCroatia/length(Croatia))),2)

confidenceIntervalUpperBoundCases <- round((populationMeanCasesItaly-populationMeanCasesCroatia) + zscore * sqrt((varCasesItaly/length(Italy)) 
                                                                                                        + (varCasesCroatia/length(Croatia))),2)

# With a Z-Score of 2.35 for Deaths and a Z-Score of 2.39 for Cases, these values are greater than our Z-Score of 1.96, therefore we reject the null hypothesis

#3 Correlation between incident rates and fatalities for countries 
cor.test(covidData$deaths,covidData$cases,method = c("pearson", "kendall", "spearman"))
plot(covidData$cases,covidData$deaths)

#4 Regression Model for the next 5 days for United States
USA <- covidData[covidData$countryterritoryCode=='USA',][order(nrow(covidData[covidData$countryterritoryCode=='USA',]):1),]
usaDeathTS<- ts(USA$deaths)
plot(usaDeathTS, main = 'Time Series of Deaths per day')

for(i in 1:length(USA)){
  USA$date <- as.Date(paste0(year,'-',month,'-',day))
}

usaCasesTS<- ts(USA$cases)
plot(usaCasesTS, main = 'Time Series of Cases per day')

usaDeathsModel <- lm(deaths ~ date, data = USA)
summary(usaDeathsModel)

usaCasesModel <- lm(cases ~ date, data = USA)
summary(usaCasesModel)

new.df <- data.frame(date = c(as.Date('2020-08-22'),as.Date('2020-08-23'),as.Date('2020-08-24'),as.Date('2020-08-25'),as.Date('2020-08-26')))

next5DaysDeaths<- predict(usaDeathsModel,new.df)
next5DaysCases <-predict(usaCasesModel,new.df)

data.frame(next5DaysCases,next5DaysDeaths, row.names = c(as.Date('2020-08-22'),as.Date('2020-08-23'),as.Date('2020-08-24'),as.Date('2020-08-25'),as.Date('2020-08-26')))

