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
  data$tmax_over_p <- data$tmax / data$probability
  data$tmax_over_n_times_p <- data$tmax / (data$node_count * data$probability)
  return(data)
}

timestamp_skipped <- "2024-05-22T18:32:26.609045625+00:00"
timestamp_unskipped <- "2024-05-22T18:32:27.721986939+00:00"
timestamp_different_tmax <- "2024-06-13T09:38:57.472962921+00:00"
timestamp_few_nodes_many_p <- "2024-06-19T13:48:42.697546+00:00"
timestamp_snap_all <- "2024-05-29T14:45:12.664694+00:00"

data_skipped <- read_experiment_results(timestamp_skipped)
data_unskipped <- read_experiment_results(timestamp_unskipped)
data_different_tmax <- read_experiment_results(timestamp_different_tmax)
data_few_nodes_many_p <- read_experiment_results(timestamp_few_nodes_many_p)
data_snap <- read_experiment_results(timestamp_snap_all)

data_skipped <- data_skipped %>% mutate(optimization = "skipped")
data_unskipped <- data_unskipped %>% mutate(optimization = "unskipped")
data_snap <- data_snap %>% mutate(source = "snap")

data <- bind_rows(data_skipped, data_unskipped)

plot_variable <- function(
  dat,
  var_y,
  label_y,
  var_x = "edge_count",
  label_x = "Edge count",
  logged = FALSE,
  facet_variable = NULL,
  title = "",
  color = TRUE,
  abline = TRUE) {
  this_aes <- if (color) {
    aes(x = !!sym(var_x), y = !!sym(var_y), color = probability_fac)
  } else {
    aes(x = !!sym(var_x), y = !!sym(var_y))
  }

  plot <- dat %>%
    ggplot(this_aes) +
    geom_point() +
    # geom_abline(slope = 6) +
    scale_color_discrete() +
    geom_smooth(method = lm, color = "red") +
    labs(x = label_x, y = label_y, color = "p") +
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

  if (!logged & abline) {
    plot <- plot +
      geom_segment(aes(x = 0, y = 0, xend = max(edge_count), yend = 6 * max(edge_count)), color = "black")
  }

  return(plot)
}

plot_variable(data, "restarts", "Restarts", logged = FALSE, facet_variable = "optimization",
                            title = "Follow algorithm Erdős-Renyi graphs with different densities")
ggsave(glue("./export/erdos-renyi_restarts-by-edge-count-{timestamp_skipped}.pdf"))
plot_variable(data, "restarts_for_following", "Restarts (following only)", logged = FALSE,
                            title = "Follow algorithm Erdős-Renyi graphs with different densities")
ggsave(glue("./export/erdos-renyi_restarts-for-following-by-edge-count-{timestamp_skipped}.pdf"))
plot_variable(data %>% filter(probability == 0.3), "restarts", "Restarts", logged = TRUE)
plot_variable(
  data %>% filter(skipped_redundant_infections == TRUE),
  "restarts",
  "Restarts",
  logged = TRUE,
  facet_variable = "probability_fac",
  # title = "Follow algorithm Erdős-Renyi graphs with different densities"
  )
ggsave(glue("./export/erdos-renyi_restarts-by-edge-count-and-p-{timestamp_skipped}.pdf"))
plot_variable(data_different_tmax, "restarts", "Restarts")

plot_variable(data_snap, "restarts", "Restarts",
                            # title = "SNAP Networks",
                            color = FALSE)
ggsave(glue("./export/snap_restarts-by-edge-count-and-p-{timestamp_snap_all}.pdf"))

plot_variable(data_skipped,
              "component_discovery_percentage",
              "Component discovery percentage",
              abline = FALSE)

plot_variable(data_different_tmax,
              "component_discovery_percentage",
              "Component discovery percentage",
              abline = FALSE)

plot_variable(data_different_tmax,
              "component_discovery_percentage",
              "Component discovery percentage",
              var_x = "tmax",
              label_x = "Tmax",
              abline = FALSE,
              logged = FALSE)
ggsave(glue("./export/erdos-renyi_component-discovery-percentage-by-tmax-{timestamp_different_tmax}.pdf"))

summary(data_skipped$restarts_per_edge)

summary(data_skipped$component_discovery_percentage)
summary(data_unskipped$component_discovery_percentage)

summary(data_skipped$restarts_for_component_discovery)
summary(data_unskipped$restarts_for_component_discovery)

View(data_few_nodes_many_p)

plot_summary <- function(data, x_var, grouping_var, mean_var) {
  data <- data %>%
    group_by(!!sym(grouping_var)) %>%
    summarise(summarised = mean(!!sym(mean_var), na.rm = TRUE))

  if (grouping_var == "probability_fac") {
    data <- data %>%
      mutate(probability = as.numeric(as.character(probability_fac)))
  }

  return(data %>%
    ggplot(aes(x = !!sym(x_var), y = summarised)) +
    geom_col() +
    scale_x_continuous()
  )
}

plot_summary(
  data_few_nodes_many_p %>% filter(node_count == 41),
  "probability",
  "probability_fac",
  "component_discovery_percentage") +
  labs(x = "p", y = "Percentage of restarts \n due to component discovery")
ggsave(glue("./export/erdos-renyi_component-discovery-percentage-by-p-{timestamp_few_nodes_many_p}.pdf"), height = 3)


plot_summary(
  data_few_nodes_many_p %>% filter(node_count == 41),
  "tmax",
  "tmax",
  "component_discovery_percentage") +
  labs(x = "Tmax", y = "Percentage of restarts \n due to component discovery")
ggsave(glue("./export/erdos-renyi_component-discovery-percentage-by-Tmax-{timestamp_few_nodes_many_p}.pdf"), height = 3)


data_few_nodes_many_p %>%
  # filter(node_count == 91) %>%
  group_by(tmax_over_p, node_count) %>%
  summarise(mean_component_discovery_percentage = mean(component_discovery_percentage, na.rm = TRUE)) %>%
  ggplot(aes(x = tmax_over_p, y = mean_component_discovery_percentage, color=node_count)) +
  geom_point() +
  scale_x_log10() +
  scale_color_continuous(type = "viridis") +
  labs(x = "Tmax / p", y = "Percentage of restarts \n due to component discovery")
ggsave(glue("./export/erdos-renyi_component-discovery-percentage-by-Tmax-over-p-{timestamp_few_nodes_many_p}.pdf"))

data_few_nodes_many_p %>%
  group_by(tmax_over_n_times_p, node_count) %>%
  summarise(mean_component_discovery_percentage = mean(component_discovery_percentage, na.rm = TRUE)) %>%
  ggplot(aes(x = tmax_over_n_times_p, y = mean_component_discovery_percentage, color=node_count)) +
  geom_point() +
  scale_x_log10() +
  scale_color_continuous(type = "viridis") +
  labs(x = "Tmax / (n * p)", y = "Percentage of restarts \n due to component discovery")
ggsave(glue("./export/erdos-renyi_component-discovery-percentage-by-Tmax-over-n-times-p-{timestamp_few_nodes_many_p}.pdf"))

data_few_nodes_many_p %>%
  group_by(tmax_over_n_times_p, node_count) %>%
  summarise(mean_component_count = mean(component_count, na.rm = TRUE)) %>%
  ggplot(aes(x = tmax_over_n_times_p, y = mean_component_count, color=node_count)) +
  geom_point() +
  scale_x_log10() +
  scale_color_continuous(type = "viridis") +
  labs(x = "Tmax / (n * p)", y = "Mean component count")


data_few_nodes_many_p %>%
  group_by(tmax_over_n_times_p, node_count) %>%
  summarise(mean_component_max_size = mean(component_max_size, na.rm = TRUE)) %>%
  ggplot(aes(x = tmax_over_n_times_p, y = mean_component_max_size, color=node_count)) +
  geom_point() +
  scale_x_log10() +
  scale_color_continuous(type = "viridis") +
  labs(x = "Tmax / (n * p)", y = "Mean max component size")


data %>% filter(restarts_for_following > 6 * edge_count)