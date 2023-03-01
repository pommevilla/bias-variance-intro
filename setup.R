############### Setup
library(tidyverse)
library(patchwork)

theme_set(
    theme_light() +
        theme(panel.grid = element_blank())
)

nice_expansion <- expansion(mult = c(0.1, 0.1), add = 0)

############### Creating color palette

training_testing_colors <- c(
    "Testing" = "#ffb750",
    "Training" = "#58508d"
)




############### Generating data

num_samples <- 12

set.seed(489)
sample_df <- data.frame(x = runif(num_samples, 1, 10)) %>%
    rowwise() %>%
    mutate(y = -x^2 + runif(1, -20, 20)) %>%
    ungroup() %>%
    mutate(Set = if_else(
        row_number() < num_samples * 0.7,
        "Training",
        "Testing"
    ))

training_set <- sample_df %>%
    filter(Set == "Training")

testing_set <- sample_df %>%
    filter(Set == "Testing")

############### Creating plots


base_plot <- sample_df %>%
    ggplot(aes(x, y)) +
    geom_point(size = 4, shape = 21, aes(fill = Set)) +
    theme(
        legend.background = element_rect(
            fill = "white",
            color = "gray90",
            size = 1
        ),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 1
    ) +
    labs(
        x = "",
        y = ""
    ) +
    scale_fill_manual(values = training_testing_colors) +
    coord_cartesian(ylim = c(-90, -10))

underfit_plot <- base_plot +
    geom_smooth(
        data = training_set,
        method = lm,
        formula = y ~ poly(x, 1, raw = TRUE),
        se = FALSE,
        color = "black",
        size = 0.5,
        linetype = "dashed"
    )

overfit_plot <- base_plot +
    geom_smooth(
        data = training_set,
        method = lm,
        formula = y ~ poly(x, 9, raw = TRUE),
        se = FALSE,
        color = "black",
        size = 0.5,
        linetype = "dashed"
    )

goodfit_plot <- base_plot +
    geom_smooth(
        data = training_set,
        method = lm,
        formula = y ~ poly(x, 3, raw = TRUE),
        se = FALSE,
        color = "black",
        size = 0.5,
        linetype = "dashed"
    )

all_fit_plots <- (underfit_plot | overfit_plot | goodfit_plot) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A", tag_suffix = ")") &
    theme(
        legend.position = "bottom"
    )
