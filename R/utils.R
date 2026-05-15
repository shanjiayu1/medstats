#' @keywords internal
#' @noRd
wilcox_z_test <- function(data, variable, by, ...) {
  f <- as.formula(paste0("`", variable, "` ~ factor(", by, ")"))
  res <- coin::wilcox_test(f, data = data, distribution = "asymptotic")
  data.frame(
    statistic = as.numeric(coin::statistic(res)),
    p.value   = as.numeric(coin::pvalue(res))
  )
}

#' Format p-value with <0.001 notation
#'
#' @param p Numeric. A p-value.
#' @return Character string. Formatted p-value.
#' @keywords internal
#' @noRd
format_p <- function(p) {
  ifelse(is.na(p), NA, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

#' Format p-value for RCS plot annotations
#'
#' @param p Numeric. A p-value.
#' @return Character string. Formatted p-value.
#' @keywords internal
#' @noRd
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}
