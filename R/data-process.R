#' Convert longitudinal data to survival analysis format
#'
#' @description
#' Transforms long-format longitudinal data into a wide survival dataset suitable for
#' Cox regression. For each subject, it determines whether an event occurred (status = 1)
#' and calculates the time to first event or censoring.
#'
#' @param data A data frame in long format with repeated measurements per subject.
#' @param id_var Character string. Name of the column identifying individual subjects.
#'   Default is `"患者病案号"`.
#' @param event_flag_var Character string. Name of the column indicating event occurrence
#'   (1 = event, 0 = no event). Default is `"是否MSE"`.
#' @param time_var Character string. Name of the column containing follow-up time values.
#'   Default is `"re随访时间"`.
#' @param baseline_vars Character vector. Names of baseline variables to retain (takes the
#'   first value per subject). Default is `c("性别", "胸腺切除")`.
#'
#' @return A data frame with one row per subject, containing:
#'   - All `baseline_vars` columns (first value per subject)
#'   - `status`: 1 if event occurred, 0 otherwise
#'   - `time`: time to first event (if status=1) or last follow-up (if status=0)
#'
#' @details
#' The endpoint event must be coded as numeric 1. Events at time 0 are excluded.
#'
#' @examples
#' \dontrun{
#' surv_data <- long_to_surv_data(
#'   data = my_clinical_data,
#'   id_var = "PatientID",
#'   event_flag_var = "is_event",
#'   time_var = "follow_up_time",
#'   baseline_vars = c("Sex", "Age")
#' )
#' }
#'
#' @export
long_to_surv_data <- function(data,
                              id_var = "患者病案号",
                              event_flag_var = "是否MSE",
                              time_var = "re随访时间",
                              baseline_vars = c("性别", "胸腺切除")) {

  id_sym <- rlang::sym(id_var)
  flag_sym <- rlang::sym(event_flag_var)
  time_sym <- rlang::sym(time_var)

  surv_df <- data |>
    dplyr::arrange(!!id_sym, !!time_sym) |>
    dplyr::group_by(!!id_sym) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(baseline_vars), ~ .x[1]),
      status = ifelse(any((!!flag_sym) == 1 & (!!time_sym) > 0, na.rm = TRUE), 1, 0),
      time = ifelse(
        status == 1,
        min((!!time_sym)[(!!flag_sym) == 1 & (!!time_sym) > 0], na.rm = TRUE),
        max((!!time_sym), na.rm = TRUE)
      ),
      .groups = "drop"
    )

  return(surv_df)
}


#' Create a baseline characteristics table (Table 1)
#'
#' @description
#' Generates a publication-ready baseline table using `gtsummary::tbl_summary()`,
#' with automatic handling of non-normally distributed variables (Wilcoxon rank-sum test
#' with Z statistic) and optional group comparisons.
#'
#' @param data A data frame.
#' @param vars Character vector. Names of variables to include in the table.
#' @param specific_vars Character vector or NULL. Names of continuous variables that are
#'   non-normally distributed (reported as median + IQR). Default is `NULL`.
#' @param group_var Character string or NULL. Name of the grouping variable for comparisons.
#'   If `NULL`, no group comparison or p-values are shown. Default is `NULL`.
#'
#' @return A `gtsummary` table object.
#'
#' @details
#' - Normally distributed continuous variables: mean (SD)
#' - Non-normally distributed continuous variables (specified via `specific_vars`): median (P25, P75)
#' - Categorical variables: n (p%)
#' - When `group_var` is provided, p-values are computed using t-test for normal
#'   continuous variables and Wilcoxon rank-sum Z-test for `specific_vars`.
#'
#' @examples
#' \dontrun{
#' library(gtsummary)
#' make_table1(
#'   data = trial,
#'   vars = c("age", "marker", "stage", "grade"),
#'   specific_vars = c("marker"),
#'   group_var = "trt"
#' )
#' }
#'
#' @export
make_table1 <- function(data,
                        vars,
                        specific_vars = NULL,
                        group_var = NULL) {

  stat_list <- list(
    gtsummary::all_continuous() ~ "{mean}({sd})",
    gtsummary::all_categorical() ~ "{n}({p}%)"
  )

  if (!is.null(specific_vars) && length(specific_vars) > 0) {
    stat_list <- c(stat_list, list(gtsummary::all_of(specific_vars) ~ "{median}({p25},{p75})"))
  }

  t <- data |>
    gtsummary::tbl_summary(
      include = gtsummary::all_of(vars),
      by = if (!is.null(group_var)) gtsummary::all_of(group_var) else NULL,
      missing = "no",
      statistic = stat_list,
      digits = list(
        gtsummary::all_continuous() ~ 2,
        gtsummary::all_categorical() ~ c(0, 2)
      )
    ) |>
    gtsummary::add_overall()

  if (!is.null(group_var)) {
    t <- t |>
      gtsummary::add_p(
        test = list(
          gtsummary::all_continuous() ~ "t.test",
          specific_vars ~ wilcox_z_test
        ),
        pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
      ) |>
      gtsummary::modify_fmt_fun(gtsummary::statistic ~ gtsummary::label_style_number(digits = 2)) |>
      gtsummary::modify_header(gtsummary::statistic ~ "**Test Statistic**")
  }

  t
}
