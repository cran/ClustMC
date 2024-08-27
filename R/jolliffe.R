#' Jolliffe test for multiple comparisons
#'
#' I.T. Jolliffe test for multiple comparisons.
#' Implements a cluster-based alternative closely linked to the
#' Student-Newman-Keuls multiple comparison method. Single-linkage cluster
#' analysis is applied, using the p-values obtained with the SNK test for
#' pairwise mean comparison as a similarity measure. Groups whose means join
#' beyond \eqn{1 - \alpha} are statistically different. Alternatively, complete
#' linkage cluster analysis can also be applied.
#'
#' @param y Either a model (created with `lm()` or `aov()`) or a numerical
#'    vector with the values of the response variable for each unit.
#' @param trt If `y` is a model, a string with the name of the column containing
#'    the treatments. If `y` is a vector, a vector of the same length as `y`
#'    with the treatments for each unit.
#' @param alpha Numeric value corresponding to the
#'    significance level of the test. The default value is 0.05.
#' @param method `string` indicating the clustering method to be used. For
#'    single linkage (the default method) either `"single"` or `"slca"`.
#'    For complete linkage, either `"complete"` or `"clca"`.
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
#'    significance level used, `n` is either the number of repetitions for all
#'    treatments or the harmonic mean of said repetitions, `MSE` is the mean
#'    standard error from the ANOVA table and `SEM` is an estimate of the
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
#' jolliffe_test(y = weights, trt = treatments, alpha = 0.1, show_plot = FALSE)
#' # Using a model -------------------------------------------------------
#' model <- lm(weights ~ treatments)
#' jolliffe_test(y = model, trt = "treatments", alpha = 0.1, show_plot = FALSE)
#' @references Jolliffe, I. T. (1975). Cluster analysis as a multiple comparison
#' method. \emph{Applied Statistics: Proceedings of Conference at Dalhousie
#' University, Halifax}, 159-168.
#' @author Santiago Garcia Sanchez
# nolint start: cyclocomp_linter.
jolliffe_test <- function(y, trt, alpha = 0.05, method = "single",
                          show_plot = TRUE, console = TRUE,
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

  # Multiple comparisons require more than two treatments
  if (nrow(dataset) < 3) {
    cli::cli_abort(c(
      "There must be at least three treatments.",
      "x" = "{nrow(dataset)} treatment{?s} found."
    ))
  }

  # Ordering means to calculate range
  dataset_ord <- dataset %>%
    dplyr::arrange(mean)
  dataset_ord$range <- seq_len(nrow(dataset))

  # If groups have unequal sample sizes, harmonic mean is applied.
  if (length(unique(dataset$r)) == 1) {
    n <- unique(dataset$r)
  } else {
    n <- psych::harmonic.mean(dataset$r)
  }

  mse <- stats::anova(y)["Residuals", "Mean Sq"]
  sample_s <- sqrt(mse / n)
  df_tukey <- stats::anova(y)["Residuals", "Df"]

  # Cannot divide by zero
  if (df_tukey == 0) {
    cli::cli_abort("Error degrees of freedom were equal to zero, model can't be
                   saturated.")
  }

  # Matrices of differences and of ranges
  matrix_dist <- abs(outer(dataset_ord$mean, dataset_ord$mean, FUN = "-"))
  matrix_range <- abs(outer(dataset_ord$range, dataset_ord$range,
    FUN = "-"
  )) + 1

  # Distances are studentized to calculate p-values
  matrix_d <- matrix_dist / sample_s
  matrix_p <- stats::ptukey(matrix_d, matrix_range, df_tukey) %>%
    usedist::dist_setNames(dataset_ord$treatment)

  # Specified clustering is applied
  if (method %in% c("single", "Single", "SLCA", "slca")) {
    dendrogram <- stats::hclust(stats::as.dist(matrix_p), method = "single")
  } else if (method %in% c("complete", "Complete", "CLCA", "clca")) {
    dendrogram <- stats::hclust(stats::as.dist(matrix_p), method = "complete")
  } else {
    cli::cli_warn(c(
      "Invalid `method` name.",
      "x" = "You've tried to use method = {method}.",
      "i" = "method = `single` will be used by default."
    ))
    dendrogram <- stats::hclust(stats::as.dist(matrix_p), method = "single")
  }

  if (!is.numeric(alpha)) {
    cli::cli_warn(c(
      "`alpha` must be numeric.",
      "x" = "You've tried to use an alpha of type {typeof(alpha)}.",
      "i" = "alpha = 0.05 will be used by default."
    ))
    alpha <- 0.05
  } else if (alpha <= 0 || alpha >= 1) {
    cli::cli_warn(c(
      "`alpha` must be between 0 and 1.",
      "x" = "You've tried to use alpha = {alpha}.",
      "i" = "alpha = 0.05 will be used by default."
    ))
    alpha <- 0.05
  }

  if (show_plot) {
    plot_dendrogram_jolliffe(dendrogram, alpha, abline_options, ...)
  }

  # Formats data for return values.
  stats <- as.data.frame(dataset[order(dataset$mean), ])
  groups <- procs::proc_sort(as.data.frame(
    stats::cutree(dendrogram, h = 1 - alpha)
  ))
  colnames(groups) <- "group"
  parameters <- data.frame(
    "treatments" = nrow(dataset), "n" = n, "alpha" = alpha, "df" = df_tukey,
    "MSE" = mse, "SEM" = sample_s
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
# nolint end

#' Plot the dendrogram
#'
#' @param dendrogram `hclust` object.
#' @param alpha Numeric value corresponding to the significance level of the
#'    test.
#' @param abline_options `list` with optional arguments for `abline`.
#' @param ... Optional arguments for `plot`
#'
#' @noRd
plot_dendrogram_jolliffe <- function(dendrogram, alpha, abline_options, ...) {
  # If user does not specify labels, sets default values.
  args <- list(...)
  plot_labels <- c("main", "sub", "xlab", "ylab")
  plot_args <- list(
    main = "Cluster dendrogram",
    sub = "Differences below the line are not significant.",
    xlab = "Groups",
    ylab = "1 - Probability"
  )

  for (label in plot_labels) {
    if (label %in% names(args)) {
      plot_args <- plot_args[names(plot_args) != label]
    }
  }

  do.call(graphics::plot, c(list(dendrogram), plot_args, args))

  # Default style unless specified by the user
  if (missing(abline_options)) {
    graphics::abline(h = 1 - alpha, col = "steelblue", lwd = 3, lty = 2)
  } else {
    do.call(graphics::abline, c(list(h = 1 - alpha), abline_options))
  }

  # Print warnings as the line may or may not be visible in the plot
  if (1 - alpha > max(dendrogram$height)) {
    cli::cli_alert_info("No differences were significant, the line may not be
                        displayed in some cases.")
  } else if (1 - alpha < min(dendrogram$height)) {
    cli::cli_alert_info("All differences were significant, the line may not be
                        displayed in some cases.")
  }
}
