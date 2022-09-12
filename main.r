library(tidyverse)

# Read dataset from file
dataset <- read.csv("climate_dataset.csv", fileEncoding="latin1")

# Graph population vs. emissions 
ggplot(data = dataset) +
  geom_point(mapping = aes(x = congestion_rank, y = tot_mean))

# Graph congestion rank vs. total pollution
ggplot(data = dataset) +
  geom_point(mapping = aes(x = population, y = scope_1_emissions, color=region))

# Graph average gas price vs. emissions
ggplot(data = dataset) +
  geom_point(mapping = aes(x = gas_price_2014_usd, y = scope_1_emissions, color=region))

# Graph median income vs. emissions
ggplot(data = dataset) +
  geom_smooth(mapping = aes(x = ngdp_capita_usd, y = total_scope_2_emissions, color=region))

# Graph annual mean temperature vs emissions
ggplot(data = dataset) +
  geom_point(mapping = aes(x = average_annual_temperature_celcius, y = scope_1_emissions, color=region))