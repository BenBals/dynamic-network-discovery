library(tidyverse)
library(readr)
library(glue)
library(stringr)
library(reshape2)

read_experiment_results <- function(timestamp) {
  data <- read.csv(glue("../data/experiment-results-{timestamp}.csv")) %>%
    as_tibble() %>%
    mutate(skipped_redundant_infections = (skipped_redundant_infections == "true"))
  data$probability_fac <- as.factor(data$probability)
  data$component_discovery_percentage <- data$restarts_for_component_discovery / data$restarts
  data$restarts_for_following <- data$restarts - data$restarts_for_component_discovery
  data$restarts_per_edge <- data$restarts / data$edge_count
  return(data)
}

timestamp_skipped <- "2024-05-22T18:32:26.609045625+00:00"
timestamp_unskipped <- "2024-05-22T18:32:27.721986939+00:00"
timestamp_snap_all <- "2024-05-29T14:45:12.664694+00:00"

data_skipped <- read_experiment_results(timestamp_skipped)
data_unskipped <- read_experiment_results(timestamp_unskipped)
data_snap <- read_experiment_results(timestamp_snap_all)

data_skipped <- data_skipped %>% mutate(optimization = "skipped")
data_unskipped <- data_unskipped %>% mutate(optimization = "unskipped")
data_snap <- data_snap %>% mutate(source = "snap")

data <- bind_rows(data_skipped, data_unskipped)

plot_variable_against_edges <- function(dat, var, label, logged = FALSE, facet_variable = NULL, title = "", color = TRUE) {
  this_aes <- if (color) {
    aes(x = edge_count, y = !!sym(var), color = probability_fac)
  } else {
    aes(x = edge_count, y = !!sym(var))
  }

  plot <- dat %>%
    ggplot(this_aes) +
    geom_point() +
    # geom_abline(slope = 6) +
    scale_color_discrete() +
    geom_smooth(method = lm, color = "red") +
    labs(x = "Edge count", y = label, color = "p") +
    ggtitle(title)

  if (!is.null(facet_variable)) {
    plot <- plot +
      facet_wrap(reformulate(facet_variable), nrow = ifelse(facet_variable == "optimization", 2, 3))
  }

  if (logged) {
    plot <- plot +
      scale_x_log10() +
      scale_y_log10() +
      geom_function(fun = function (edges) {return(6 * edges)}, color = "black")
  }

  if (!logged) {
    plot <- plot +
      geom_segment(aes(x = 0, y = 0, xend = max(edge_count), yend = 6 * max(edge_count)), color = "black")
  }

  return(plot)
}

plot_variable_against_edges(data, "restarts", "Restarts", logged = FALSE, facet_variable = "optimization",
                            title = "Follow algorithm Erdős-Renyi graphs with different densities")
ggsave(glue("./export/erdos-renyi_restarts-by-edge-count-{timestamp_skipped}.pdf"))
plot_variable_against_edges(data, "restarts_for_following", "Restarts (following only)", logged = FALSE,
                            title = "Follow algorithm Erdős-Renyi graphs with different densities")
ggsave(glue("./export/erdos-renyi_restarts-for-following-by-edge-count-{timestamp_skipped}.pdf"))
plot_variable_against_edges(data %>% filter(probability == 0.3), "restarts", "Restarts", logged = TRUE)
plot_variable_against_edges(
  data %>% filter(skipped_redundant_infections == TRUE),
  "restarts",
  "Restarts",
  logged = TRUE,
  facet_variable = "probability_fac",
  # title = "Follow algorithm Erdős-Renyi graphs with different densities"
  )
ggsave(glue("./export/erdos-renyi_restarts-by-edge-count-and-p-{timestamp_skipped}.pdf"))
plot_variable_against_edges(data_snap, "restarts", "Restarts",
                            # title = "SNAP Networks",
                            color = FALSE)
ggsave(glue("./export/snap_restarts-by-edge-count-and-p-{timestamp_snap_all}.pdf"))

summary(data_skipped$restarts_per_edge)

summary(data_skipped$component_discovery_percentage)
summary(data_unskipped$component_discovery_percentage)

summary(data_skipped$restarts_for_component_discovery)
summary(data_unskipped$restarts_for_component_discovery)

data_skipped %>%
  group_by(probability_fac) %>%
  summarise(mean_component_discovery_percentage = mean(component_discovery_percentage, na.rm = TRUE)) %>%
  ggplot(aes(x = probability_fac, y = mean_component_discovery_percentage)) +
  geom_col() +
  labs(x = "p", y = "Percentage of restarts \n due to component discovery")
ggsave(glue("./export/erdos-renyi_component-discovery-percentage-by-p-{timestamp_skipped}.pdf"), height = 3)


data %>% filter(restarts_for_following > 6 * edge_count)