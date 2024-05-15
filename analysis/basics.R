library(tidyverse)
library(readr)
library(glue)
library(stringr)
library(reshape2)

timestamp_skipped   <- "2024-05-14T11:53:39.558182+00:00-skipped"
timestamp_unskipped <- "2024-05-14T13:41:59.779396+00:00-unskipped"

read_experiment_results <- function (timestamp) {
  data <- read.csv(glue("../data/experiment-results-{timestamp}.csv")) %>% as_tibble()
  data$probability_fac <- as.factor(data$probability)
  data$component_discovery_percentage <- data$restarts_for_component_discovery / data$restarts
  return(data)
}

data_skipped <- read_experiment_results(timestamp_skipped)
data_unskipped <- read_experiment_results(timestamp_unskipped)

data_skipped <- data_skipped %>% mutate(optimization = "skipped")
data_unskipped <- data_unskipped %>% mutate(optimization = "unskipped")

data <- bind_rows(data_skipped, data_unskipped)

data %>%
  ggplot(aes(x = edge_count, y = restarts, color = probability_fac)) +
  facet_grid(optimization ~ .) + 
  geom_point() +
  geom_abline(slope = 6) +
  scale_color_discrete() + 
  geom_smooth(method=lm, color = "red") +
  labs(x = "Edge count", y = "Restarts", color = "p") +
  ggtitle("Follow algorithm Erdős-Renyi graphs with different densities")

summary(data_skipped$component_discovery_percentage)
summary(data_unskipped$component_discovery_percentage)

summary(data_skipped$restarts_for_component_discovery)
summary(data_unskipped$restarts_for_component_discovery)

ggsave(glue("./export/erdos-renyi_restarts-by-edge-count-{timestamp}.pdf"))
