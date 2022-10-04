library(tidyverse)
library(readxl)
library(glmnet)

#-----------------
#READ DATA FROM FILE
#-----------------
dataset <- read_excel("D_FINAL.xlsx")

# ----------------
#DATA TIDYING
#-----------------
# This line removes all unnecessary columns. This is a very large dataset, 
# so we have many columns to remove. We are removing columns by index to 
# avoid having an impractically large command
correct_dataset_columns <- dataset[-c(1, 3:11, 15:16, 18, 20, 22, 25, 35, 38, 
                                      41:49, 52, 58:62, 65, 67:70,73:87, 
                                      90, 94,96:98, 101, 102, 104:106, 107, 113, 
                                      118, 119, 124, 142:160, 167:180)]



# There are a lot of nasty column names such as 
# 'Congestion rank (INRIX) [dimensionless]' that should be renamed to things like 
# 'congestion rank' instead. This renames each of those column names to something
# easier to work with
colnames(correct_dataset_columns) <- c('city name', 'study year', 'reporting year',
                                       'scope 1 emissions', 'year of emission',
                                       'gases included', 'increase/decrease',
                                       'scope 2 emissions','total emissions',
                                       'emissions quality flag', 's1 lower bound',
                                       's1 upper bound', 's1 mean', 'tot lower bound',
                                       'tot upper bound', 'tot mean', 'scope fraction',
                                       'co2 emissions per capita', 'latitude',
                                       'longitude', 'country', 'region', 'population',
                                       'population year', 'population 1950', 
                                       'population 1990', 'population 2010',
                                       'population growth rate 1950-2010',
                                       'population growth rate 1990-2010', 
                                       'city area km', 'city area year', 
                                       'population density', 'average altitude',
                                       'average annual temperature celcius',
                                       'gdp-ppp', 'gdp-ppp per capita', 
                                       'gdp-ppp year', 'ngdp', 'ngdp per capita',
                                       'ngdp-year', 'total consumption per capita',
                                       'energy per person co2', 'center of commerce index',
                                       'urbanization ratio', 'water bounded',
                                       'other bounded', 'mean one-way travel time',
                                       'mean one-way travel time year','average diesel price',
                                       'average gasoline price', 'household size',
                                       'household size year', 'congestion rank',
                                       'peak hours spent in congestion', 'congestion index',
                                       'average congestion rate', 'congestion level', 
                                       'congestion change', 'morning peak', 'evening peak', 
                                       'economy rank', 'environment rank', 'governance rank',
                                       'human capital rank', 'international impact rank', 
                                       'mobility and transportation rank', 'public management rank', 
                                       'social cohesion rank', 'technology rank', 
                                       'urban planning rank', 'CIMI', 'CIMI Ranking', 
                                       'CIMI Performance', 'exports', 'production', 
                                       'natgas exports', 'C4H waste per capita',
                                       'C4H waste plus natgas per capita', 
                                       'corrected C4H waste plus natgas per capita', 
                                       '')

#-----------------
#DATA VISUALIZATION
#-----------------

# Graph population vs. emissions 
ggplot(data = correct_dataset_columns) +
  geom_point(mapping = aes(x = `congestion rank`, y = `tot mean`))

# Graph congestion rank vs. total pollution
ggplot(data = correct_dataset_columns) +
  geom_point(mapping = aes(x = population, y = `tot mean`, color=region))

# Graph average gas price vs. emissions
 ggplot(data = correct_dataset_columns) +
   geom_point(mapping = aes(x = `average gasoline price`, y = `tot mean`, color=region)) +
   coord_cartesian(ylim=c(0, 30000000))

# Graph median income vs. emissions
 ggplot(data = correct_dataset_columns) +
   geom_smooth(mapping = aes(x = `ngdp per capita`, y = `tot mean`, color=region))

# Graph annual mean temperature vs emissions
 ggplot(data = correct_dataset_columns) +
   geom_point(mapping = aes(x = `average annual temperature celcius`, y = `tot mean`, color=region))
 
# Example multinomial logistic regression model
 xH <- model.matrix(`total emissions` ~ population , data=correct_dataset_columns)
 yH <- correct_dataset_columns$`total emissions`
 netfit <- glmnet(xH, yH, family='multinomial')
 lmin <- min(netfit$lambda)
 pnet <- drop(predict(netfit, xH, s=lmin, type="response"))