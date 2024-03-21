# Dependencies -----

require(tidyverse)

# Prepare free association data for package -----

intelligence <- read_csv("01_Data/intelligence_associations.csv")
save(intelligence, file = "01_Data/intelligence.rda")
