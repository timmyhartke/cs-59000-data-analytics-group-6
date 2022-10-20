library(tidyverse)
library(readxl)
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
correct_dataset_columns <- dataset[c(14, 23, 24, 27:32, 34, 40, 50, 63, 
                                     92, 100,103, 111,120, 136, 137)]



# There are a lot of nasty column names such as 
# 'Congestion rank (INRIX) [dimensionless]' that should be renamed to things like   
# 'congestion rank' instead. This renames each of those column names to something
# easier to work with
colnames(correct_dataset_columns) <- c(
  'scope 1 emissions', 
  'scope 2 emissions','total emissions',
  's1 lower bound',
  's1 upper bound', 's1 mean', 'tot lower bound',
  'tot upper bound', 'tot mean',
  'co2 emissions per capita', 'region', 'population',
  'city area km',
  'ngdp',
  'energy per person co2', 'center of commerce index',
  'mean one-way travel time',
  'congestion rank',
  'social cohesion rank', 'technology rank', 
  '')


# Prints the correlation between 'total emissions' and all other columns
# between 0.3 and 0.5 is considered 'moderately correlated. Between 0.5 and 0.7
# is highly correlated, and higher than 0.7 is considered highly correlated.
correlations <- cor(correct_dataset_columns$`total emissions`, "storage.mode<-"(as.matrix(correct_dataset_columns[c(1:ncol(correct_dataset_columns))]), "numeric"), use = "pairwise.complete.obs")
format(correlations)  

# This functions as our logit function that will be used in our logistic 
# regressions for research questions 2 and 3. 
# We create a new column called 'emissions level' to store this new data in
# We take the median of all 'tot mean' values, and we classify all
# values higher than the median as 'high emissions' and all the values lower or 
# equal to the median as 'low emissions'.
# For 'tot mean', we have more NA values than actual values, so we first use
# complete.cases to remove all rows where 'tot mean' is 'NA'
correct_dataset_columns <- correct_dataset_columns[complete.cases(correct_dataset_columns[ , c('tot mean')]), ]
tot_mean_median <- median(correct_dataset_columns$`tot mean`)

high_low_emissions <- c(nrow(correct_dataset_columns))
for (index in 1:nrow(correct_dataset_columns)){
  tot_mean = correct_dataset_columns[index, ]$`tot mean`
  print(tot_mean)
  if (tot_mean > tot_mean_median){
    high_low_emissions[index] <- 1
  }
  else {
    high_low_emissions[index] <- 0
  }
}
correct_dataset_columns$`emissions level` <- high_low_emissions


#-----------------
#DATA VISUALIZATION
#-----------------

# Graph population vs. emissions 
ggplot(data = correct_dataset_columns) +
  geom_point(mapping = aes(x = `congestion rank`, y = log(`tot mean`)))

# Graph congestion rank vs. total pollution
ggplot(data = correct_dataset_columns) +
  geom_point(mapping = aes(x = population, y = log(`tot mean`), color=region))


# Graph median income vs. emissions
ggplot(data = correct_dataset_columns) +
  geom_smooth(mapping = aes(x = `ngdp`, y = log(`tot mean`), color=region))


#-----------------
#RESEARCH QUESTION 1
#-----------------
# First research question: What effect does a city's population have on its total emissions?
# For this, we set up a linear regression model that uses a city's population
# to predict what the city's total pollution level is
population_model <- glm(log(`tot mean`) ~ population, data=correct_dataset_columns)
summary(population_emissions_model)


#-----------------
#RESEARCH QUESTION 2
#-----------------
# Second research question: What effect does congestion have on a city's total emissions?
congestion_model <- glm(`emissions level` ~ `mean one-way travel time` + 
                          `congestion rank`, data=correct_dataset_columns, 
                        family="binomial")
summary(congestion_model)

#-----------------
#RESEARCH QUESTION 3
#-----------------
# Third research question: What effect does a city's area have on the city's overall total emissions?
area_model <- glm(`emissions level` ~ `city area km`, data=correct_dataset_columns, 
                        family="binomial")
summary(area_model)

