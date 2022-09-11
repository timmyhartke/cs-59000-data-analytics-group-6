library(tidyverse)

dataset <- read.csv("climate_dataset.csv", fileEncoding="latin1")


ggplot(data = dataset) +
  geom_point(mapping = aes(x = population, y = scope_1_emissions, color=region))
