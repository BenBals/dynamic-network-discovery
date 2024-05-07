library(tidyverse)
library(readr)
library(glue)
library(stringr)
library(reshape2)

data <- read.csv("../data/experiment-results-2024-05-07T19:27:03.613904+00:00.csv") %>% as_tibble()

data$probability_fac <- as.factor(data$probability)

data %>%
  ggplot(aes(x = edge_count, y = restarts, color = probability_fac)) +
  geom_point() +
  geom_abline() +
  scale_color_discrete() + 
  geom_smooth(method=lm, color = "red") +
  labs(x = "Edge count", y = "Restarts", color = "p") +
  ggtitle("Follow algorithm Erdős-Renyi graphs with different densities")
