#' Format a flextable with publication-ready three-line table style
#'
#' @description
#' Applies a unified formatting scheme to a flextable, gtsummary, or data.frame object,
#' producing a clean three-line table (三线表) suitable for academic publications.
#'
#' @param ft_data A `flextable`, `gtsummary`, or `data.frame` object to format.
#'
#' @return A formatted `flextable` object with Times New Roman font, three-line borders,
#'   centered alignment, and auto-fitted column widths.
#'
#' @examples
#' # Format a data.frame
#' format_flextable(head(mtcars[, 1:4]))
#'
#' # Format a gtsummary object
#' \dontrun{
#' library(gtsummary)
#' tbl <- tbl_summary(trial, include = c(age, grade))
#' format_flextable(tbl)
#' }
#'
#' @export
format_flextable <- function(ft_data) {
  # 判断输入类型
  if (inherits(ft_data, "flextable")) {
    ft <- ft_data
    n_cols <- length(ft$body$col_keys)
  } else if (inherits(ft_data, "gtsummary")) {
    ft <- gtsummary::as_flex_table(ft_data)
    n_cols <- length(ft$body$col_keys)
  } else {
    ft <- flextable::flextable(ft_data)
    n_cols <- ncol(ft_data)
  }

  ft |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 10, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "left", part = "all", j = 1) |>
    flextable::align(align = "center", part = "all", j = 2:n_cols) |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 1.5)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 1.5)) |>
    flextable::autofit() |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::line_spacing(space = 1, part = "all")
}


#' Export tables and plots to a Word document
#'
#' @description
#' Takes a list of tables and plots and exports them into a single `.docx` file.
#' Tables are formatted with [format_flextable()]. Plots can be `ggplot` objects,
#' result lists containing a `ggplot` in `$plot`, base R plot instructions made
#' with [officer::plot_instr()], or paths to PNG, JPEG, BMP, GIF, or TIFF files.
#'
#' @param data_list A list of table or plot objects. Tables can be `data.frame`,
#'   `gtsummary`, or `flextable` objects. Plots can be `ggplot` objects, result
#'   lists containing a `ggplot` in `$plot`, `officer::plot_instr` objects, or
#'   supported image file paths.
#' @param table_titles A character vector of item titles. Must have the same length
#'   as `data_list`.
#' @param output_file A string specifying the output file path. Must end with `.docx`.
#'   Default is `"Tables_Output.docx"`.
#' @param figure_width Width of exported plots in inches. Default is `6`.
#' @param figure_height Height of exported plots in inches. Default is `5`.
#' @param figure_res Resolution of exported `ggplot` objects in pixels per inch.
#'   Default is `300`.
#'
#' @return Invisibly returns the `officer::rdocx` document object.
#'
#' @examples
#' \dontrun{
#' df1 <- head(mtcars[, 1:5])
#' df2 <- head(iris)
#' export_word(
#'   data_list = list(df1, df2),
#'   table_titles = c("Table 1: mtcars", "Table 2: iris"),
#'   output_file = "my_tables.docx"
#' )
#'
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#' export_word(
#'   data_list = list(head(mtcars), p),
#'   table_titles = c("Table 1: mtcars", "Figure 1: MPG and weight"),
#'   output_file = "tables_and_plots.docx"
#' )
#'
#' base_plot <- officer::plot_instr(
#'   graphics::plot(mtcars$wt, mtcars$mpg)
#' )
#' export_word(list(base_plot), "Figure 1: Base R plot", "base_plot.docx")
#' }
#'
#' @export
export_word <- function(data_list, table_titles, output_file = "Tables_Output.docx",
                        figure_width = 6, figure_height = 5, figure_res = 300) {

  # 参数安全性检查
  if (!is.list(data_list)) {
    stop("错误：'data_list' 必须是一个列表 (list)。")
  }
  if (length(data_list) != length(table_titles)) {
    stop("错误：数据集的数量 (data_list) 与表名的数量 (table_titles) 不一致！")
  }
  if (!grepl("\\.docx$", output_file)) {
    stop("错误：输出文件名 (output_file) 必须以 .docx 结尾！")
  }
  if (!is.character(table_titles) || anyNA(table_titles)) {
    stop("'table_titles' must be a character vector without missing values.")
  }
  figure_args <- list(figure_width, figure_height, figure_res)
  valid_figure_args <- vapply(
    figure_args,
    function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0,
    logical(1)
  )
  if (!all(valid_figure_args)) {
    stop("'figure_width', 'figure_height', and 'figure_res' must be positive numbers.")
  }

  # 创建空白 Word 文档
  doc <- officer::read_docx()

  image_extensions <- c("png", "jpg", "jpeg", "bmp", "gif", "tif", "tiff")

  # 循环处理每一个表格或图形
  for (i in seq_along(data_list)) {
    # 插入标题
    doc <- officer::body_add_par(doc, value = table_titles[i], style = "Normal")

    item <- data_list[[i]]
    plot_item <- if (inherits(item, "ggplot")) {
      item
    } else if (is.list(item) && !inherits(item, "data.frame") &&
               inherits(item$plot, "ggplot")) {
      item$plot
    } else {
      NULL
    }

    if (!is.null(plot_item)) {
      doc <- officer::body_add_gg(
        doc,
        value = plot_item,
        width = figure_width,
        height = figure_height,
        res = figure_res
      )
    } else if (inherits(item, "plot_instr")) {
      doc <- officer::body_add_plot(
        doc,
        value = item,
        width = figure_width,
        height = figure_height,
        res = figure_res
      )
    } else if (is.character(item) && length(item) == 1L && file.exists(item) &&
               tolower(tools::file_ext(item)) %in% image_extensions) {
      doc <- officer::body_add_img(
        doc,
        src = item,
        width = figure_width,
        height = figure_height
      )
    } else if (inherits(item, "data.frame") || inherits(item, "gtsummary") ||
               inherits(item, "flextable")) {
      formatted_ft <- format_flextable(item)
      doc <- flextable::body_add_flextable(doc, value = formatted_ft)
    } else {
      stop(sprintf(
        "Unsupported object at data_list[[%d]]. Use a table, ggplot, result list with $plot, plot_instr, or image path.",
        i
      ))
    }

    # 项目间空行
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
  }

  # 保存文档
  print(doc, target = output_file)

  message(sprintf("Successfully exported %d item(s) to: %s", length(data_list), output_file))

  invisible(doc)
}
