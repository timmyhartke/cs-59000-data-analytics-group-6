library(tidyverse)
library(readxl)
library(class)
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
nrow(correct_dataset_columns)

high_low_emissions <- c(nrow(correct_dataset_columns))
for (index in 1:nrow(correct_dataset_columns)){
  tot_mean = correct_dataset_columns[index, ]$`tot mean`
  if (tot_mean > tot_mean_median){
    high_low_emissions[index] <- 1
  }
  else {
    high_low_emissions[index] <- 0
  }
}
correct_dataset_columns$`emissions level` <- high_low_emissions
# We are using the first 40% of our dataset as training data, and the last 60%
# of our dataset as testing data
training_data_length <- floor(nrow(correct_dataset_columns)*0.4)
testing_data_length <- nrow(correct_dataset_columns) - training_data_length
training_data <- head(correct_dataset_columns, training_data_length)
testing_data <- tail(correct_dataset_columns,testing_data_length)


#-----------------
#DATA VISUALIZATION
#-----------------

# Graph congestion rank vs. total emissions 
ggplot(data = correct_dataset_columns, aes(x = `congestion rank`, y = log(`tot mean`))) +
  geom_point() +
  ggtitle("Congestion Rank vs. Log of Total Emissions") +
  xlab("Congestion Rank") + ylab("Total Emissions (log)")

# Graph population vs. total emissions
ggplot(data = correct_dataset_columns, aes(x = log(population), y = log(`tot mean`))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Log of Population vs. Log of Total Emissions") +
  xlab("Population (log)") + ylab("Total Emissions (log)")
  

# Graph mean one-way travel time vs. total emissions
ggplot(data = correct_dataset_columns, aes(x=`mean one-way travel time`, y=log(`tot mean`), color=region)) +
  geom_point() + 
  ggtitle("Mean One-Way Travel Time vs. Log of Total Emissions") +
  xlab("Population (log)") + ylab("Total Emissions (log)")

# Graph median income vs. emissions
ggplot(data = correct_dataset_columns) +
  geom_point(mapping = aes(x = log(`ngdp`), y = log(`tot mean`), color=region)) + 
  ggtitle("Log of Median Income vs. Log of Total Emissions") +
  xlab("Median Income") + ylab("Total Emissions")


#-----------------
#RESEARCH QUESTION 1
#-----------------
# First research question: What effect does a city's population have on its total emissions?
# For this, we set up a linear regression mode that use a city's population
# to predict what the city's total pollution level is and a logistic regression
# model to predict whether a city is a 'high emissions' city or a 
# 'low emissions' city 

# Linear regression model
population_linear_model <- lm(log(`tot mean`) ~ population, data=training_data)
population_linear_model
summary(population_linear_model)

# Information regarding the model
attributes(population_linear_model)
population_linear_model$residuals
# NOTE: If your plot window is too small in RStudio, you will get an error
# at this line. To fix this, you just need to drag the end of your plot window
# to make it a bit bigger. 
hist(population_linear_model$residuals)

# prediction
predict(population_linear_model, testing_data) %>% round(1)

# Logistic regression model
population_logistic_model <- glm(`emissions level` ~ `population`, data=training_data, 
                        family="binomial")
summary(population_logistic_model)

# Information regarding the model
attributes(population_logistic_model)
population_logistic_model$residuals
# NOTE: If your plot window is too small in RStudio, you will get an error
# at this line. To fix this, you just need to drag the end of your plot window
# to make it a bit bigger. 
hist(population_logistic_model$residuals)

# prediction
probabilities <- population_logistic_model %>% predict(testing_data, type = "response")
predicted_population_logistic_classes <- ifelse(probabilities > 0.5, 1, 0)
predicted_population_logistic_classes


#-----------------
#RESEARCH QUESTION 2
#-----------------
# Second research question: What effect does congestion have on a city's total emissions?
congestion_model <- glm(`emissions level` ~ `mean one-way travel time`, data=training_data, 
                        family="binomial")
summary(congestion_model)

# Information regarding the model
attributes(congestion_model)
congestion_model$residuals
# NOTE: If your plot window is too small in RStudio, you will get an error
# at this line. To fix this, you just need to drag the end of your plot window
# to make it a bit bigger. 
hist(congestion_model$residuals)

# prediction
probabilities <- congestion_model %>% predict(testing_data, type = "response")
predicted_congestion_classes <- ifelse(probabilities > 0.5, 1, 0)
print(predicted_congestion_classes)


#KNN
# Get the optimum k value
i=1
k.optm=1
for(i in 1:42){
  knn.mod<-knn(train=knn_training_data[,17], test=knn_testing_data[,17], cl=training_categories,k=i)
  k.optm[i]<-100*sum(testing_categories==knn.mod)/NROW(testing_categories)
  k=i
  cat(k,'=',k.optm[i],'\n')
}
# This graph shows that k=10 appears to be the optimum value
plot(k.optm, type="b",xlab="K-value", ylab="Accuracy Level")

# Gets all rows with non-empty mean one-way travel time
knn_training_data <- training_data[complete.cases(training_data[,17]),]
knn_testing_data <- testing_data[complete.cases(testing_data[,17]),]
# Get the emissions level column for training and testing data
training_categories <- knn_training_data[,21]$`emissions level`
testing_categories <- knn_testing_data[,21]$`emissions level`

congestion_knn_model <- knn(knn_training_data[,17], knn_testing_data[,17], training_categories, k=10)
# Generate confusion matrix
tab <- table(congestion_knn_model,testing_categories)
tab
# Precision
tab[1, 1]/sum(tab[1, 1:2])
# Recall
tab[1, 1]/sum(tab[1:2, 1])
# Specificity
tab[2, 2]/sum(tab[2, 1:2])
# Accuracy
(tab[1, 1] + tab[2, 2])/sum(tab[1:2, 1:2])



#-----------------
#RESEARCH QUESTION 3
#-----------------
# Third research question: What effect does a city's area have on the city's overall total emissions?
area_model <- glm(`emissions level` ~ `city area km`, data=training_data, 
                        family="binomial")
summary(area_model)

# Information regarding the model
attributes(area_model)
area_model$residuals
# NOTE: If your plot window is too small in RStudio, you will get an error
# at this line. To fix this, you just need to drag the end of your plot window
# to make it a bit bigger. 
hist(area_model$residuals)


# prediction
probabilities <- area_model %>% predict(testing_data, type = "response")
predicted_area_classes <- ifelse(probabilities > 0.5, 1, 0)
print(predicted_area_classes)

