library(tidyverse)
library(readr)
library(glue)
library(stringr)
library(reshape2)

timestamp_skipped   <- "2024-05-17T13:09:35.801518422+00:00"
timestamp_unskipped <- "2024-05-16T16:05:15.418007171+00:00"

read_experiment_results <- function (timestamp) {
  data <- read.csv(glue("../data/experiment-results-{timestamp}.csv")) %>% as_tibble()
  data$probability_fac <- as.factor(data$probability)
  data$component_discovery_percentage <- data$restarts_for_component_discovery / data$restarts
  data$restarts_for_following <- data$restarts - data$restarts_for_component_discovery
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
  geom_segment(aes(x = 0, y=0, xend=max(edge_count), yend=6*max(edge_count)), color="black") +
  # geom_abline(slope = 6) +
  scale_color_discrete() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method=lm, color = "red") +
  labs(x = "Edge count", y = "Restarts", color = "p") +
  ggtitle("Follow algorithm Erdős-Renyi graphs with different densities")

summary(data_skipped$component_discovery_percentage)
summary(data_unskipped$component_discovery_percentage)

summary(data_skipped$restarts_for_component_discovery)
summary(data_unskipped$restarts_for_component_discovery)

ggsave(glue("./export/erdos-renyi_restarts-by-edge-count-{timestamp_skipped}.pdf"))
