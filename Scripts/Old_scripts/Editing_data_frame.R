library(tidyverse)
All_data <- read.csv("Data/All_data.csv")
All_data <- All_data %>% mutate(Species_names = str_replace_all(Binomial, pattern = " ", "_"))
write.csv(All_data, "Data/All_data.csv", row.names = FALSE)