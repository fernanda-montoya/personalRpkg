#'
#' Log Sum of Exponentiated Values
#'
#' @param x A vector of numeric values
#'
#' @return The log sum of exponentiated input vector
#'
#' @examples
#'
#' log_summed_exps(c(15,42,10,19,14))
#' log_summed_exps(1:2000)
#'
#' @export
log_summed_exps <- function(x) {
  if (!is.numeric(x)) {
    stop ("Input vector must be numeric")
  } else if (any(is.na(x))) {
    stop ("Input vector cannot have missing values")
    } else {
  # determine largest value in input vector
  largest <- max(x)

  # "trick" formula for log summed exponents
  lsum <- largest + log(sum(exp(x - largest)))

  return(lsum)
  }
}
