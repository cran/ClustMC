## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----customplot---------------------------------------------------------------
library(ClustMC)

data(clover)

bss_test(
  y = clover$nitrogen, trt = clover$treatment, console = FALSE,
  main = "A customized plot", xlab = "Treatments", ylab = NULL, col = "grey50",
  cex = 0.75, axes = FALSE, frame.plot = TRUE
)

## ----customline---------------------------------------------------------------
library(ClustMC)

data(PlantGrowth)
plants_weights <- PlantGrowth$weight
plants_trt <- PlantGrowth$group

dgc_test(
  y = plants_weights, trt = plants_trt, console = FALSE,
  main = "A plot with a customized line",
  abline_options = list(col = "red", lty = 3, lwd = 1)
)

## ----ggdendro-----------------------------------------------------------------
library(ClustMC)
library(ggplot2)
library(ggdendro)

data(bread)
anova_model <- aov(volume ~ variety + as.factor(bromate), data = bread)

test_results <- jolliffe_test(
  y = anova_model, trt = "variety", console = FALSE,
  show_plot = FALSE
)

ggdendro::ggdendrogram(test_results$dendrogram_data) +
  ggplot2::geom_hline(
    yintercept = 0.95, colour = "blue", linetype = "dotted",
    linewidth = 0.75
  ) +
  ggplot2::ggtitle("SLCA dendrogram for bread baking data")

