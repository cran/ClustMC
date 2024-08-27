#' Bautista, Smith and Steiner test for multiple comparisons
#'
#' Bautista, Smith and Steiner (BSS) test for multiple comparisons.
#' Implements a procedure for grouping treatments following the determination of
#' differences among them. First, a cluster analysis of the treatment means is
#' performed and the two closest means are grouped. A nested analysis of
#' variance from the original ANOVA is then constructed with the treatment
#' source now partitioned into "groups" and "treatments within groups". This
#' process is repeated until there are no differences among the group means or
#' there are differences among the treatments within groups.
#'
#' @param y Either a model (created with `lm()` or `aov()`) or a numerical
#'    vector with the values of the response variable for each unit.
#' @param trt If `y` is a model, a string with the name of the column containing
#'    the treatments. If `y` is a vector, a vector of the same length as `y`
#'    with the treatments for each unit.
#' @param alpha Numeric value corresponding to the
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
#'    `treatments` is the total number of treatments and `alpha` is the
#'    significance level used.}
#'    \item{dendrogram_data}{object of class `hclust` with data used to build
#'    the dendrogram.}
#' @export
#'
#' @examples
#' data("PlantGrowth")
#' # Using vectors -------------------------------------------------------
#' weights <- PlantGrowth$weight
#' treatments <- PlantGrowth$group
#' bss_test(y = weights, trt = treatments, show_plot = FALSE)
#' # Using a model -------------------------------------------------------
#' model <- lm(weights ~ treatments)
#' bss_test(y = model, trt = "treatments", show_plot = FALSE)
#' @references Bautista, M. G., Smith, D. W., & Steiner, R. L. (1997).
#' A Cluster-Based Approach to Means Separation. \emph{Journal of Agricultural,
#' Biological, and Environmental Statistics, 2}(2), 179-197.
#' \doi{doi:10.2307/1400402}
#' @author Santiago Garcia Sanchez
# nolint start: cyclocomp_linter.
bss_test <- function(y, trt, alpha = 0.05, show_plot = TRUE, console = TRUE,
                     abline_options, ...) {
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

  # Avoid errors with R CMD check
  treatment <- var_y <- group <- NULL

  # Allows `y` to be a vector and identifies the treatments
  if (!"aov" %in% class(y) && !"lm" %in% class(y)) {
    y <- stats::aov(y ~ trt)
    trt <- colnames(y$model)[2]
    model_terms <- ""

    # At least one difference must be significant
    if (stats::anova(y)$`Pr(>F)`[1] > alpha) {
      cli::cli_abort("No treatments are significantly different
                     (Pr(>F) > {alpha}).")
    }
  } else {
    # `trt` must be the name of a column
    if (length(colnames(y$model)[which(colnames(y$model) == trt)]) != 1) {
      cli::cli_abort("Column `{trt}` can't be found in {.var y}.")
    }

    # Model terms are saved to be applied later
    trts <- trt

    model_terms <- setdiff(
      attr(stats::terms(stats::formula(y)),
        which = "term"
      ),
      trts
    )

    # Depending on the number of variables, one or more "+" are added
    if (length(model_terms) > 1) {
      model_terms <- gsub(":", "`:`", paste(paste0("`", model_terms, "`"),
        collapse = " + "
      ))
      model_terms <- paste0("+", model_terms)
    } else if (length(model_terms) == 1) {
      model_terms <- paste0("+`", model_terms, "`")
    }

    model_terms <- gsub(
      paste0("`", trts, "`"),
      "`treatment`",
      model_terms,
      fixed = TRUE
    )

    # At least one difference must be significant
    if (stats::anova(y)[trt, "Pr(>F)"] > alpha) {
      cli::cli_abort("No treatments are significantly different
                     (Pr(>F) > `alpha`).")
    }
  }

  # Identifies variables by their role
  full_data <- y$model
  colnames(full_data)[1] <- "var_y"
  colnames(full_data)[which(colnames(full_data) == trt)] <- "treatment"

  dataset <- full_data %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(
      r = dplyr::n(),
      mean = mean(var_y),
      sd = stats::sd(var_y),
      median = stats::median(var_y),
      min = min(var_y),
      max = max(var_y)
    ) %>%
    dplyr::arrange(mean)

  # Multiple comparisons require more than two treatments
  if (nrow(dataset) < 3) {
    cli::cli_abort(c(
      "There must be at least three treatments.",
      "x" = "{nrow(dataset)} treatment{?s} found."
    ))
  }

  # Initial values for the while loop
  p_val_among <- 0
  p_val_between <- 1
  iter <- 1
  while (
    p_val_among < alpha &&
      p_val_between > alpha &&
      iter < length(unique(dataset$treatment))
  ) {
    # First run
    if (iter == 1) {
      means <- dataset

      matrix_dif <- abs(outer(means$mean, means$mean, FUN = "-"))
      matrix_dist <- usedist::dist_setNames(matrix_dif, dataset$treatment)

      dendrogram <- stats::hclust(stats::as.dist(matrix_dist),
        method = "single"
      )

      groups <- which(matrix_dif == min(matrix_dist), arr.ind = TRUE)[1, ]
      equal_trt <- dataset[groups, ]$treatment

      full_data <- full_data %>%
        dplyr::mutate(
          group = dplyr::case_when(
            treatment %in% c(equal_trt) ~ "1",
            .default = treatment
          )
        )

      previous_data <- full_data %>%
        dplyr::mutate(group = treatment)
    } else {
      previous_data <- full_data

      means <- full_data %>%
        dplyr::group_by(group) %>%
        dplyr::summarise(
          mean = mean(var_y)
        )

      matrix_dif <- abs(outer(means$mean, means$mean, FUN = "-"))
      matrix_dist <- usedist::dist_setNames(matrix_dif, means$group)

      # Groups the two closest treatment means
      groups <- which(matrix_dif == min(matrix_dist), arr.ind = TRUE)[1, ]
      equal_trt <- means[groups, ]$group

      # If a group already exists, it's merged with the treatment
      if (any(equal_trt %in% as.character(c(1:iter)))) {
        full_data <- full_data %>%
          dplyr::mutate(
            group = dplyr::case_when(
              group %in% c(equal_trt) == TRUE ~ min(
                equal_trt[equal_trt %in% as.character(c(1:iter))]
              ),
              .default = group
            )
          )
      } else {
        full_data <- full_data %>%
          dplyr::mutate(
            group = dplyr::case_when(
              treatment %in% c(equal_trt) == TRUE ~ as.character(iter),
              .default = group
            )
          )
      }
    }

    # ANOVA table is only built if there are at least two groups
    if (1 < length(unique(full_data$group))) {
      anova_table <- stats::anova(stats::lm(
        data = full_data,
        paste0("var_y~factor(group)/treatment", model_terms)
      ))
      p_val_among <- anova_table["factor(group)", "Pr(>F)"]
      p_val_between <- anova_table["factor(group):treatment", "Pr(>F)"]
    }

    iter <- iter + 1
  }

  # Transforms groups into numbers in order
  groups <- dplyr::right_join(dataset,
    previous_data %>%
      dplyr::select(treatment, group) %>%
      dplyr::distinct(),
    by = dplyr::join_by(treatment)
  ) %>%
    dplyr::select(treatment, group) %>%
    dplyr::mutate(group = as.numeric(factor(group, levels = unique(group))))

  if (show_plot) {
    if (iter > 2) {
      height <- dendrogram$height[iter - 1]
    } else {
      height <- NULL
    }
    plot_dendrogram_bss(dendrogram, height, abline_options, ...)
  }

  # Formats data for return values.
  stats <- dataset
  groups <- groups
  parameters <- data.frame(
    "treatments" = nrow(dataset),
    "alpha" = alpha
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
#' @param height Numeric value corresponding to the height at which the line
#' will be plotted.
#' @param abline_options `list` with optional arguments for `abline`.
#' @param ... Optional arguments for `plot`
#'
#' @noRd
plot_dendrogram_bss <- function(dendrogram, height, abline_options, ...) {
  # If user does not specify labels, sets default values.
  args <- list(...)
  plot_labels <- c("main", "sub", "xlab", "ylab")
  plot_args <- list(
    main = "Cluster dendrogram",
    sub = "Differences on the line or above it are significant.",
    xlab = "Treatments",
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
    graphics::abline(h = height, col = "steelblue", lwd = 3, lty = 2)
  } else {
    do.call(graphics::abline, c(list(h = height), abline_options))
  }

  # Print warning if the line is not visible
  if (is.null(height)) {
    cli::cli_alert_info(
      "All differences were significant, the line will not be displayed."
    )
  }
}
