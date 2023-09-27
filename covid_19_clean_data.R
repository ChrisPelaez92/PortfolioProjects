install.packages('tidyverse')
library('tidyverse')
install.packages('dplyr')
library('dplyr')


##Set File path
setwd('Desktop/data_proj/corona_virus/data')

##Import Covid Data set
cv <- read_csv('owid-covid-data.csv')

##Select columns that will be used
cv_data <- cv %>% select(c( continent, location, date, total_cases, new_cases,
                            new_deaths, total_deaths, population, total_vaccinations))

##-------------------Clean cv_data

sum(is.na(cv_data$location)) ## 0 NA values
n_distinct(cv_data$location)
unique(cv_data$location)

##Removing "High income","Low income", "Lower middle income", "Upper middle income"
cv_data <- cv_data[!(cv_data$location == 'High income'), ]
cv_data <- cv_data[!(cv_data$location == 'Low income'), ]
cv_data <- cv_data[!(cv_data$location == 'Lower middle income'), ]
cv_data <- cv_data[!(cv_data$location == 'Upper middle income'), ]

sum(is.na(cv_data$continent)) ##10493 NA values
n_distinct(cv_data$continent)
unique(cv_data$continent)
##combine content NA with location
cv_data$continent <- ifelse(is.na(cv_data$continent), cv_data$location, cv_data$continent)
sum(is.na(cv_data$continent)) ##0 NA values

sum(is.na(cv_data$date)) ## 0 NA values

sum(is.na(cv_data$new_cases)) ## 9184 NA Values
cv_data$new_cases <- replace(cv_data$new_cases, is.na(cv_data$new_cases), 0)
sum(is.na(cv_data$new_cases)) ## 0 NA Values

sum(is.na(cv_data$total_cases)) ## 37441 NA Values
##Replacing NA values in total_cases with new_cases column
cv_data$total_cases[is.na(cv_data$total_cases)] <- cv_data$new_cases[is.na(cv_data$total_cases)]
sum(is.na(cv_data$total_cases)) ## 0 NA Values

sum(is.na(cv_data$new_deaths)) ## 9115 NA Values
cv_data$new_deaths <- replace(cv_data$new_deaths, is.na(cv_data$new_deaths), 0)
sum(is.na(cv_data$new_deaths)) ## 0 NA Values

sum(is.na(cv_data$total_deaths)) ## 58530 NA Values
##Replacing NA values in total_deaths with new_death column
cv_data$total_deaths[is.na(cv_data$total_deaths)] <- cv_data$new_deaths[is.na(cv_data$total_deaths)]
sum(is.na(cv_data$total_deaths)) ## 0 NA Values


sum(is.na(cv_data$population)) ## 0 NA

sum(is.na(cv_data$total_vaccinations)) ## 251892 NA


##-------------------Adding Death Infection 

##Total Case Vs Deaths creating a infected death percentage column
cv_data$death_infection_perc <- round((cv_data$total_deaths/cv_data$total_cases)*100, 2)
##replacing NaN values
cv_data$death_infection_perc <- replace_na(cv_data$death_infection_perc, 0)


##Percentage of population that has gotten infected Column
cv_data$population_infected_perc <- round((cv_data$total_cases/cv_data$population)*100, 2)


##--------------------Tableau


##Total World Deaths %
death_infecttion_perc <- cv_data %>% filter( continent == 'World') %>% arrange(desc(total_deaths)) %>% 
  slice(1) %>%
  select(total_cases,total_deaths,death_infecttion_perc)

write.csv(death_infecttion_perc, file = 'death_infecttion_perc.csv')



##Show Highest Death Rate per Continent
highest_deaths <- cv_data %>% filter(continent != 'World') %>% 
  arrange(desc(total_deaths)) %>% 
  group_by(continent) %>% 
  slice(1) %>% select(continent, total_deaths) 

write_csv(highest_deaths,
          file = 'highest_deaths.csv')

##Location highest count
location_deaths <- cv_data %>% arrange(desc(total_cases)) %>% group_by(location) %>% 
  slice(1) %>% select(location,population, total_cases ,population_infected_perc)

write_csv(location_deaths,
          file = 'location_deaths.csv')

##Location highest count by date
location_dates <- cv_data %>% group_by(location) %>% 
  select(location,population, total_deaths ,population_infected_perc, date)

write_csv(location_dates, file = 'location_dates.csv')



