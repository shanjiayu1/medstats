#' Plot mean with error bars (line chart)
#'
#' @description
#' Creates a publication-quality line chart showing group means with 95% confidence
#' interval error bars. Supports both single-group and multi-group comparisons.
#'
#' @param data A data frame.
#' @param group_var Character string or NULL. Name of the grouping variable.
#'   If `NULL`, a single-group plot is generated. Default is `NULL`.
#' @param target_var Character string. Name of the continuous outcome variable.
#' @param time_var Character string. Name of the time variable. Must contain parseable
#'   numeric values for proper x-axis ordering.
#' @param legend_title Character string. Title for the legend. Default is `"组别"`.
#' @param xlab Character string. X-axis label. Default is `"随访时间"`.
#' @param ylab Character string. Y-axis label. Default is `"值"`.
#' @param colors Character vector. Colors for multi-group lines. Must have length >= number of
#'   groups. Default is `c("#B5D1E8", "#EB938F", "#A3D9A5", "#F2C68F", "#D1B3DF")`.
#' @param single_color Character string. Color for single-group line. Default is `"#B5D1E8"`.
#'
#' @return A list with:
#'   - `summary_data`: data frame of computed means, SDs, SEs
#'   - `plot`: the ggplot object
#'
#' @details
#' Time variable must contain extractable numeric values via `readr::parse_number()`.
#' For multi-group plots, ensure `colors` has enough values for all groups.
#'
#' @examples
#' \dontrun{
#' plot_meanse(
#'   data = ChickWeight,
#'   target_var = "weight",
#'   time_var = "Time",
#'   group_var = "Diet"
#' )
#' }
#'
#' @export
plot_meanse <- function(data,
                        group_var = NULL,
                        target_var,
                        time_var,
                        legend_title = "组别",
                        xlab = "随访时间",
                        ylab = "值",
                        colors = c("#B5D1E8", "#EB938F", "#A3D9A5", "#F2C68F", "#D1B3DF"),
                        single_color = "#B5D1E8") {

  is_single <- is.null(group_var)

  if (is_single) {
    group_var <- "dummy_group_var"
    data[[group_var]] <- "全体患者"
  }

  group_sym <- rlang::sym(group_var)
  target_sym <- rlang::sym(target_var)
  time_sym <- rlang::sym(time_var)

  summary_df <- data |>
    dplyr::group_by(!!group_sym, !!time_sym) |>
    dplyr::summarise(
      n = sum(!is.na(!!target_sym)),
      target_mean = mean(!!target_sym, na.rm = TRUE),
      target_sd = sd(!!target_sym, na.rm = TRUE),
      target_se = target_sd / sqrt(n),
      .groups = "drop"
    ) |>
    dplyr::arrange(!!group_sym, !!time_sym) |>
    dplyr::mutate(
      time_num = readr::parse_number(as.character(!!time_sym))
    )

  min_val <- min(summary_df$target_mean - 1.96 * summary_df$target_se, na.rm = TRUE)
  auto_ymin <- ifelse(min_val > 0, min_val - min_val * 0.1, min_val + min_val * 0.1)

  unique_times <- sort(unique(summary_df$time_num))
  min_time <- min(unique_times, na.rm = TRUE)
  max_time <- max(unique_times, na.rm = TRUE)
  x_padding <- max(1, (max_time - min_time) * 0.05)

  p <- summary_df |>
    ggplot2::ggplot(ggplot2::aes(x = time_num, y = target_mean,
                                   group = !!group_sym, color = !!group_sym, shape = !!group_sym)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = target_mean - 1.96 * target_se, ymax = target_mean + 1.96 * target_se),
      width = (max_time - min_time) * 0.02,
      na.rm = TRUE) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = unique_times, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(min_time - x_padding / 2, max_time + x_padding),
                              ylim = c(auto_ymin, NA), clip = "off") +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(ggplot2::margin(t = 15, r = 15, b = 15, l = 15))

  if (is_single) {
    p <- p +
      ggplot2::scale_color_manual(values = single_color) +
      ggplot2::theme(legend.position = "none")
    summary_df <- summary_df |> dplyr::select(-dummy_group_var)
  } else {
    p <- p +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(color = legend_title, shape = legend_title) +
      ggplot2::guides(
        color = ggplot2::guide_legend(title.position = "left", title.vjust = 0.5),
        shape = ggplot2::guide_legend(title.position = "left", title.vjust = 0.5)
      ) +
      ggplot2::theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = ggplot2::element_text(size = 10, margin = ggplot2::margin(r = 10)),
        legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
        legend.background = ggplot2::element_rect(fill = "transparent", color = NA)
      )
  }

  print(p)
  return(list(summary_data = summary_df, plot = p))
}


#' Plot stacked percentage bar chart
#'
#' @description
#' Creates a stacked bar chart showing the percentage distribution of a continuous variable
#' categorized into intervals across time points.
#'
#' @param data A data frame.
#' @param target_var Character string. Name of the continuous variable to categorize.
#' @param time_var Character string. Name of the time variable for the x-axis.
#'   Strongly recommended to be a factor with correct level ordering.
#' @param breaks Numeric vector. Breakpoints for cutting `target_var` into categories.
#'   Must have one more element than `labels`.
#' @param labels Character vector. Labels for the categories.
#' @param colors Character vector. Fill colors for each category. Must equal length of `labels`.
#' @param right Logical. If `TRUE` (default), intervals are (a, b]; if `FALSE`, [a, b).
#' @param legend_title Character string. Legend title. Default is `"Range"`.
#' @param xlab Character string. X-axis label. Default is `"随访时间（月）"`.
#' @param ylab Character string. Y-axis label. Default is `"百分比 (%)"`.
#' @param label_threshold Numeric. Minimum proportion to display percentage labels inside bars.
#'   Default is `0.03`.
#'
#' @return A list with:
#'   - `summary_data`: data frame of computed percentages
#'   - `plot`: the ggplot object
#'
#' @examples
#' \dontrun{
#' plot_stacked(
#'   data = my_data,
#'   target_var = "weight",
#'   time_var = "Time_str",
#'   breaks = c(-Inf, 50, 100, 200, Inf),
#'   labels = c("<=50g", "51-100g", "101-200g", ">200g"),
#'   colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F")
#' )
#' }
#'
#' @export
plot_stacked <- function(data,
                         target_var,
                         time_var = "访视标签",
                         breaks,
                         labels,
                         colors,
                         right = TRUE,
                         legend_title = "Range",
                         xlab = "随访时间（月）",
                         ylab = "百分比 (%)",
                         label_threshold = 0.03) {

  target_sym <- rlang::sym(target_var)
  time_sym <- rlang::sym(time_var)
  names(colors) <- labels

  plot_data <- data |>
    dplyr::filter(!is.na(!!target_sym), !is.na(!!time_sym)) |>
    dplyr::mutate(
      category = cut(
        !!target_sym,
        breaks = breaks,
        labels = labels,
        right = right
      ),
      category = factor(category, levels = rev(labels))
    ) |>
    dplyr::group_by(!!time_sym, category) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(!!time_sym) |>
    dplyr::mutate(
      total = sum(n),
      pct = n / total,
      label_text = ifelse(pct > label_threshold, sprintf("%.1f%%", pct * 100), "")
    )

  p_bar <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!time_sym, y = pct, fill = category)) +
    ggplot2::geom_col(width = 0.75, color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_text),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      size = 4,
      family = "sans"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 1, by = 0.25),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 11, color = "black"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.5),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.8),
      plot.margin = ggplot2::margin(t = 15, r = 15, b = 15, l = 15)
    )

  print(p_bar)
  return(list(summary_data = plot_data, plot = p_bar))
}
