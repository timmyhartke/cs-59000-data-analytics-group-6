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
  geom_point(mapping = aes(x = `average gasoline price`, y = `tot mean`, color=region))

# Graph median income vs. emissions
ggplot(data = correct_dataset_columns) +
  geom_smooth(mapping = aes(x = `ngdp per capita`, y = `tot mean`, color=region))

# Graph annual mean temperature vs emissions
ggplot(data = correct_dataset_columns) +
  geom_point(mapping = aes(x = `average annual temperature celcius`, y = `tot mean`, color=region))

