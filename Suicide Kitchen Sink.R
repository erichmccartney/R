# setwd("~/USF/6 -Statistical Data Mining/Grouo Project/Datasets")

# install.packages("reshape")
# install.packages("janitor")
# install.packages("data.table")
# install.packages("naniar")
# install.packages("readr")
# install.packages("readxl")
# install.packages('dplyr')

library(reshape)
library(janitor)
library(data.table)
library(naniar)
library(readr)
library(readxl)
library(dplyr)


# Import master file
ssq <- read_csv("WHO Suicides v3.csv")
ssq$country.name <- ssq$country_name

# Removing irrelevant columns
ssq <- ssq[, -c(1:2)]

# Un-pivot age columns
ssq <- as.data.table(ssq, keep.rownames=TRUE)
ssq <- melt(ssq, 
              id.vars<- c("code", "country.name", "year", "description", "sex", "rn"), 
              variable.name='age', 
              value.name="suicides.no")

# Scrub new features
ssq$age <- gsub("x", "", ssq$age)
ssq <- ssq %>% replace_with_na(list(suicides.no = "NULL"))
ssq$suicides.no <- as.numeric(ssq$suicides.no)
ssq <- ssq[, -c(6)]



# Import population file
who.population <- read_csv("WHO Population.csv")
who.population$country.name <- who.population$country_name

# Removing irrelevant columns
who.population <- who.population[, -c(1:2)]

# Un-pivot age columns
who.population <- as.data.table(who.population, keep.rownames=TRUE)
who.population <- melt(who.population, 
                             id.vars<- c("country.name", "year", "sex", "rn"), 
                             variable.name='age', 
                             value.name="population")

# Scrub new features
who.population$age <- gsub("x", "", who.population$age)
who.population$population <- as.numeric(who.population$population)
who.population <- who.population[, -c(4)]


# Merge data
ssq <- merge(ssq, who.population, by=c("country.name", "sex", "year", "age"))

# New Feature: Suicide rate per 100k population
ssq$suicide.rate <- (ssq$suicides.no / ssq$population) * 100000

# Import UNITED NATIONS DEVELOPMENT PROGRAMME Human Development Reports (http://hdr.undp.org/en/indicators/137506#)
Human_Development_Index_HDI <- read_csv("Human Development Index (HDI).csv")

# Cleanup data
colnames(Human_Development_Index_HDI) <- tolower(make.names(colnames(Human_Development_Index_HDI)))
Human_Development_Index_HDI <- remove_empty(Human_Development_Index_HDI, quiet = TRUE)
Human_Development_Index_HDI$country.name <- Human_Development_Index_HDI$country
Human_Development_Index_HDI <- Human_Development_Index_HDI[, -c(2)]


# Un-pivot year columns
Human_Development_Index_HDI <- as.data.table(Human_Development_Index_HDI, keep.rownames=TRUE)
Human_Development_Index_HDI <- melt(Human_Development_Index_HDI, id.vars<- c("hdi.rank", "country.name", "rn"), variable.name='year', value.name="hdi")

# Scrub new features
Human_Development_Index_HDI$year <- gsub("x", "", Human_Development_Index_HDI$year)
Human_Development_Index_HDI <- Human_Development_Index_HDI %>% replace_with_na(list(hdi = ".."))
Human_Development_Index_HDI$year <- as.numeric(Human_Development_Index_HDI$year)
Human_Development_Index_HDI <- Human_Development_Index_HDI[, -c(3)]


# Merge data
ssq <- merge(ssq, Human_Development_Index_HDI, by=c("country.name","year"))


# Import THE WORLD BANK World Development Indicators GDP US$ (https://databank.worldbank.org/source/world-development-indicators/Type/TABLE/preview/on#)
ind_gdp <- read_excel("Data_Extract_From_World_Development_IndicatorsUS$.xlsx", sheet='Data')

# Cleanup data
colnames(ind_gdp) <- tolower(make.names(colnames(ind_gdp)))
ind_gdp <- remove_empty(ind_gdp, quiet = FALSE)

# Removing irrelevant columns
ind_gdp <- ind_gdp[, -c(2:4)]

# Un-pivot year columns
ind_gdp <- as.data.table(ind_gdp)
ind_gdp <- melt(ind_gdp, id.vars<- c("country.name"), variable.name='year', value.name="GDP US$")


# Scrub new features
ind_gdp$year <- gsub("x", "", ind_gdp$year)
ind_gdp$year <- substring(ind_gdp$year, 1, 4)
ind_gdp <- ind_gdp %>% replace_with_na(list(`GDP US$` = ".."))
ind_gdp$year <- as.numeric(ind_gdp$year)

# Merge data
ssq <- merge(ssq, ind_gdp, by=c("country.name","year"))

# Import THE WORLD BANK World Development Indicators GDP per Capita (https://databank.worldbank.org/source/world-development-indicators/Type/TABLE/preview/on#)
ind_gdppercapita <- read_excel("Data_Extract_From_World_Development_IndicatorsPerCapita.xlsx", sheet='Data')

# Cleanup data
colnames(ind_gdppercapita) <- tolower(make.names(colnames(ind_gdppercapita)))
ind_gdppercapita <- remove_empty(ind_gdppercapita, quiet = FALSE)

# Removing irrelevant columns
ind_gdppercapita <- ind_gdppercapita[, -c(2:4)]

# Un-pivot year columns
ind_gdppercapita <- as.data.table(ind_gdppercapita)
ind_gdppercapita <- melt(ind_gdppercapita, id.vars<- c("country.name"), variable.name='year', value.name="GDP Per Capita")

# Scrub new features
ind_gdppercapita$year <- gsub("x", "", ind_gdppercapita$year)
ind_gdppercapita$year <- substring(ind_gdppercapita$year, 1, 4)
ind_gdppercapita <- ind_gdppercapita %>% replace_with_na(list(`GDP Per Capita` = ".."))
ind_gdppercapita$year <- as.numeric(ind_gdppercapita$year)

# Merge data
ssq <- merge(ssq, ind_gdppercapita, by=c("country.name","year"))


# Import OUR WORLD IN DATA mental and substance use disorders (https://ourworldindata.org/mental-health#)
mental_substance_use_disorder <- read_excel("suicide-rates-vs-prevalence-of-mental-and-substance-use-disorders 2.xlsx")

# Cleanup data
colnames(mental_substance_use_disorder) <- tolower(make.names(colnames(mental_substance_use_disorder)))
mental_substance_use_disorder <- remove_empty(mental_substance_use_disorder, quiet = FALSE)

# Removing irrelevant columns
mental_substance_use_disorder <- mental_substance_use_disorder[-c(1), -c(2, 4, 6:7)]

# Scrub new features
colnames(mental_substance_use_disorder) <- c("country.name", "year", "mental substance use disorder")
mental_substance_use_disorder$year <- as.numeric(mental_substance_use_disorder$year)


# Merge data
ssq <- merge(ssq, mental_substance_use_disorder, by=c("country.name","year"))
View(ssq)


#########################
# Create final dataframes
#########################


# subset data
ssq.high.level <- subset(ssq, year <= 2019 & year >= 2009)

# Flattened dataset
ssq.flat <- subset(ssq.high.level[, -c(8:14)], suicides.no >= 1)


datalist = list()

index = 1
for (i in 1:nrow(ssq.flat)) {
    for (n in ssq.flat[i]$suicides.no){
      row <- ssq.flat[i, -c(7)]
      datalist[[index]] <- row 
      index <- (index + 1)
    }
}

ssq.flat = do.call(rbind, datalist)


# Aggregated dataset
country.agg <- ssq.high.level[, c(1:2, 7)]

country.agg$country.name <- as.factor(country.agg$country.name)

country.agg[is.na(country.agg)] <- 0

country.agg <- country.agg %>% 
  group_by(country.name, year) %>%
  summarise_at(vars(suicides.no),              
               list(name = sum))

colnames(country.agg) <- c("country.name","year", "suicides")

ssq.so <- ssq.high.level[, c(1:4, 8)] 

ssq.so[is.na(ssq.so)] <- 0

ssq.so <- ssq.so %>% 
  group_by(country.name, year, sex, age, population) %>% 
  distinct(country.name, year, sex, age)

ssq.so <- ssq.so %>% 
  group_by(country.name, year) %>% 
  summarise_at(vars(population),              
               list(name = sum))

colnames(ssq.so) <- c("country.name","year", "population")

country.agg <- merge(ssq.so, country.agg, by=c("country.name","year"))

country.agg$suicide.rate <- (country.agg$suicides / country.agg$population) * 100000

ssq.so <- ssq.high.level[, c(1:2, 11:14)] 

ssq.so <- ssq.so %>% 
  group_by(country.name, year, `GDP US$`, `GDP Per Capita`, `mental substance use disorder`, hdi) %>% 
  distinct(country.name, year)


country.agg <- merge(ssq.so, country.agg, by=c("country.name","year"))
























