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
    ft <- flextable::as_flex_table(ft_data)
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
    flextable::hline_top(part = "header", border = flextable::fp_border(width = 1.5)) |>
    flextable::hline_bottom(part = "header", border = flextable::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "body", border = flextable::fp_border(width = 1.5)) |>
    flextable::autofit() |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::line_spacing(space = 1, part = "all")
}


#' Export multiple tables to a Word document
#'
#' @description
#' Takes a list of tables (data.frames, gtsummary objects, or flextable objects),
#' formats each one using \code{\link{format_flextable}}, and exports them into a single
#' `.docx` file with custom table titles.
#'
#' @param data_list A list of table objects. Each element can be a `data.frame`,
#'   `gtsummary`, or `flextable` object.
#' @param table_titles A character vector of table titles. Must have the same length
#'   as `data_list`.
#' @param output_file A string specifying the output file path. Must end with `.docx`.
#'   Default is `"Tables_Output.docx"`.
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
#' }
#'
#' @export
export_word <- function(data_list, table_titles, output_file = "Tables_Output.docx") {

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

  # 创建空白 Word 文档
  doc <- officer::read_docx()

  # 循环处理每一个表格
  for (i in seq_along(data_list)) {
    # 插入表名
    doc <- officer::body_add_par(doc, value = table_titles[i], style = "Normal")

    # 格式化并插入
    formatted_ft <- format_flextable(data_list[[i]])
    doc <- flextable::body_add_flextable(doc, value = formatted_ft)

    # 表格间空行
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
  }

  # 保存文档
  print(doc, target = output_file)

  message(sprintf("成功！已将 %d 个表格导出至: %s", length(data_list), output_file))

  invisible(doc)
}
