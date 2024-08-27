#' Nitrogen content of red clover plants
#'
#' Includes the nitrogen content (mg) of 30 red clover plants inoculated with
#' one of four single-strain cultures of \emph{Rhizobium trifolii} or a
#' composite of five \emph{Rhizobium meliloti} strains, resulting in six
#' treatments in total.
#'
#' Data originally from an experiment by Erdman (1946), conducted in a
#' greenhouse using a completely random design. The current dataset was
#' presented by Steel and Torrie (1980) and later used by Bautista et al. (1997)
#' to illustrate their proposed procedure.
#'
#' @format
#' A tibble with 30 rows and 2 columns:
#' \describe{
#'   \item{treatment}{a factor denoting the treatment applied to each plant.}
#'   \item{nitrogen}{a number denoting the nitrogen content of each plant
#'   (milligrams).}
#' }
#' @source Steel, R., & Torrie, J. (1980). \emph{Principles and procedures of
#' statistics: A biometrical approach (2nd ed.)}. San Francisco: McGraw-Hill.
#' Available at: <https://archive.org/details/principlesproce00stee>
#'
#' @references Bautista, M. G., Smith, D. W., & Steiner, R. L. (1997).
#' A Cluster-Based Approach to Means Separation. \emph{Journal of Agricultural,
#' Biological, and Environmental Statistics, 2}(2), 179-197.
#' \doi{doi:10.2307/1400402}
#'
#' Erdman, L. W. (1946). Studies to determine if antibiosis occurs among
#' rhizobia. \emph{Journal of the American Society of Agronomy, 38},
#' 251-258. \doi{doi:10.2134/agronj1946.00021962003800030005x}
#'
#' @examples
#' data(clover)
#' summary(clover)
"clover"
