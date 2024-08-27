#' Di Rienzo, Guzman and Casanoves test for multiple comparisons
#'
#' Di Rienzo, Guzman and Casanoves (DGC) test for multiple comparisons.
#' Implements a cluster-based method for identifying groups of nonhomogeneous
#' means. Average linkage clustering is applied to a distance matrix obtained
#' from the sample means. The distribution of \eqn{Q} (distance between the
#' source and the root node of the tree) is used to build a test with a
#' significance level of \eqn{\alpha}. Groups whose means join above
#' \eqn{c} (the \eqn{\alpha}-level cut-off criterion) are statistically
#' different.
#'
#' @param y Either a model (created with `lm()` or `aov()`) or a numerical
#'    vector with the values of the response variable for each unit.
#' @param trt If `y` is a model, a string with the name of the column containing
#'    the treatments. If `y` is a vector, a vector of the same length as `y`
#'    with the treatments for each unit.
#' @param alpha Value equivalent to 0.05 or 0.01, corresponding to the
#'    significance level of the test. The default value is 0.05.
#' @param show_plot Logical value indicating whether the constructed dendrogram
#'    should be plotted or not.
#' @param console Logical value indicating whether the results should be printed
#'    on the console or not.
#' @param abline_options `list` with optional arguments for the line in the
#'    dendrogram.
#' @param ... Optional arguments for the `plot()` function.
#'
#' @returns A list with three `data.frame` and one `hclust`:
#'    \item{stats}{`data.frame` containing summary statistics by treatment.}
#'    \item{groups}{`data.frame` indicating the group to which each treatment is
#'    assigned.}
#'    \item{parameters}{`data.frame` with the values used for the test.
#'    `treatments` is the total number of treatments, `alpha` is the
#'    significance level used, `c` is the cut-off criterion for the dendrogram
#'    (the height of the horizontal line on the dendrogram), `q` is the
#'    \eqn{1 - \alpha} quantile of the distribution of \eqn{Q} (distance from
#'    the root node) under the null hypothesis and `SEM` is an estimate of the
#'    standard error of the mean.}
#'    \item{dendrogram_data}{object of class `hclust` with data used to build
#'    the dendrogram.}
#' @export
#'
#' @examples
#' data("PlantGrowth")
#' # Using vectors -------------------------------------------------------
#' weights <- PlantGrowth$weight
#' treatments <- PlantGrowth$group
#' dgc_test(y = weights, trt = treatments, show_plot = FALSE)
#' # Using a model -------------------------------------------------------
#' model <- lm(weights ~ treatments)
#' dgc_test(y = model, trt = "treatments", show_plot = FALSE)
#' @references Di Rienzo, J. A., Guzman, A. W., & Casanoves, F. (2002). A
#' Multiple-Comparisons Method Based on the Distribution of the Root Node
#' Distance of a Binary Tree. \emph{Journal of Agricultural, Biological, and
#' Environmental Statistics, 7}(2), 129-142.
#' <jstor.org/stable/1400690>
#' @author Santiago Garcia Sanchez
dgc_test <- function(y, trt, alpha = 0.05, show_plot = TRUE, console = TRUE,
                     abline_options, ...) {
  # Allows `y` to be a vector and identifies the groups
  if (!"aov" %in% class(y) && !"lm" %in% class(y)) {
    y <- stats::aov(y ~ trt)
    group <- colnames(y$model)[2]
  } else {
    group <- trt
    # `trt` must be the name of a column
    if (length(colnames(y$model)[which(colnames(y$model) == group)]) != 1) {
      cli::cli_abort("Column `{trt}` can't be found in {.var y}.")
    }
  }

  # Identifies variables by their role
  dataset <- y$model
  colnames(dataset)[1] <- "var_y"
  colnames(dataset)[which(colnames(dataset) == group)] <- "treatment"

  # Avoid errors with R CMD check
  treatment <- var_y <- NULL

  # k` is necessary to obtain `Q`
  dataset <- dataset %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(
      r = dplyr::n(),
      mean = mean(var_y),
      sd = stats::sd(var_y),
      median = stats::median(var_y),
      min = min(var_y),
      max = max(var_y)
    )

  k <- nrow(dataset)

  # `n` is necessary to obtain `Q`
  if (length(unique(dataset$r)) == 1) {
    n <- unique(dataset$r)
  } else {
    # If groups have unequal sample sizes, harmonic mean is applied.
    n <- round(psych::harmonic.mean(dataset$r))
  }

  # `alpha` must have an appropriate value to calculate `Q`
  if (alpha != 0.05 && alpha != 0.01) {
    cli::cli_warn(c(
      "`alpha` must be either 0.05 or 0.01.",
      "x" = "You've tried to use alpha = {alpha}.",
      "i" = "alpha = 0.05 will be used by default."
    ))
    alpha <- 0.05
  }
  value_q <- find_q(n, k, alpha)

  # The ANOVA table is constructed to obtain the MSE
  mse <- stats::anova(y)["Residuals", "Mean Sq"]
  value_c <- value_q * sqrt(mse / n)

  # A matrix with the Euclidean distances is built
  matrix_d <- stats::dist(dataset$mean, method = "euclidean") %>%
    usedist::dist_setNames(dataset$treatment)

  # Average linkage clustering is applied
  dendrogram <- stats::hclust(matrix_d, method = "average")

  if (show_plot) {
    plot_dendrogram_dgc(dendrogram, value_c, abline_options, ...)
  }

  # Formats data for return values.
  stats <- as.data.frame(dataset[order(dataset$mean), ])
  groups <- procs::proc_sort(as.data.frame(
    stats::cutree(dendrogram, h = value_c)
  ))
  colnames(groups) <- "group"
  parameters <- data.frame(
    "treatments" = k, "alpha" = alpha, "c" = value_c, "q" = value_q,
    "SEM" = sqrt(mse / n)
  )

  if (console) {
    print(groups)
    cat("Treatments within the same group are not significantly different\n")
  }

  output <- list(
    "stats" = stats, "groups" = groups, "parameters" = parameters,
    "dendrogram_data" = dendrogram
  )
  invisible(output)
}


#' Find the value of Q
#'
#' @param value_n Integer greater than or equal to 2, corresponding to the
#'    number of repetition per treatment. If equal to or greater than 41,
#'    it takes the value of 40.
#'
#' @param value_k Integer greater than or equal to 3, corresponding to the
#'    number of treatments. If equal to or greater than 41, takes the value of
#'    40.
#'
#' @param value_alpha Value equivalent to 0.05 or 0.01, corresponding to the
#'    significance level of the test. The default value is 0.05.
#'
#' @return Tabulated value from the table corresponding to
#'    `value_alpha` for `value_n` repetitions and `value_k` treatments.
#'
#' @noRd
find_q <- function(value_n, value_k, value_alpha = 0.05) {
  if (value_n > 40) {
    value_n <- 40
  } else if (value_n < 2) {
    cli::cli_abort("Each treatment must have at least two observations.")
  }

  if (value_k > 40) {
    value_k <- 40
  } else if (value_k < 3) {
    cli::cli_abort(c(
      "There must be at least three treatments.",
      "x" = "{value_k} treatment{?s} found."
    ))
  }

  if (value_alpha == 0.05) {
    value_q <- dplyr::filter(t95, t95$n == value_n, t95$k == value_k)$valor
  } else if (value_alpha == 0.01) {
    value_q <- dplyr::filter(t99, t95$n == value_n, t95$k == value_k)$valor
  }

  return(value_q)
}

#' Plot the dendrogram
#'
#' @param dendrogram `hclust` object.
#' @param c Positive numerical value, corresponding to the critical value.
#' @param abline_options `list` with optional arguments for `abline`.
#' @param ... Optional arguments for `plot`
#'
#' @noRd
plot_dendrogram_dgc <- function(dendrogram, c, abline_options, ...) {
  # If user does not specify labels, sets default values.
  args <- list(...)
  plot_labels <- c("main", "sub", "xlab", "ylab")
  plot_args <- list(
    main = "Cluster dendrogram",
    sub = "Differences below the line are not significant.",
    xlab = "Groups",
    ylab = "Distance"
  )

  for (label in plot_labels) {
    if (label %in% names(args)) {
      plot_args <- plot_args[names(plot_args) != label]
    }
  }

  do.call(graphics::plot, c(list(dendrogram), plot_args, args))

  # Default style unless specified by the user
  if (missing(abline_options)) {
    graphics::abline(h = c, col = "steelblue", lwd = 3, lty = 2)
  } else {
    do.call(graphics::abline, c(list(h = c), abline_options))
  }

  # Print warnings as the line may or may not be visible in the plot
  if (c > max(dendrogram$height)) {
    cli::cli_alert_info("No differences were significant, the line may not be
                        displayed in some cases.")
  } else if (c < min(dendrogram$height)) {
    cli::cli_alert_info("All differences were significant, the line may not be
                        displayed in some cases.")
  }
}
