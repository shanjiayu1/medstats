test_that("format_flextable works with data.frame", {
  ft <- format_flextable(head(mtcars[, 1:4]))
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable works with flextable input", {
  ft_input <- flextable::flextable(head(iris))
  ft <- format_flextable(ft_input)
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable works with gtsummary input", {
  tbl <- gtsummary::tbl_summary(
    gtsummary::trial,
    include = c(age, grade, trt),
    by = trt
  )
  ft <- format_flextable(tbl)
  expect_s3_class(ft, "flextable")
})

test_that("export_word validates inputs", {
  expect_error(export_word("not_a_list", c("t1")), "list")
  expect_error(export_word(list(1, 2), c("only_one")), "不一致")
  expect_error(export_word(list(1), c("t1"), output_file = "test.txt"), ".docx")
  expect_error(export_word(list(1), c("t1")), "Unsupported object")
  expect_error(
    export_word(list(head(mtcars)), "t1", figure_width = 0),
    "positive numbers"
  )
  expect_error(
    export_word(list(head(mtcars)), "t1", figure_res = c(150, 300)),
    "positive numbers"
  )
})

test_that("export_word exports tables and ggplot objects", {
  output_file <- tempfile(fileext = ".docx")
  on.exit(unlink(output_file), add = TRUE)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_message(
    export_word(
      data_list = list(head(mtcars), p),
      table_titles = c("Table 1", "Figure 1"),
      output_file = output_file,
      figure_width = 5,
      figure_height = 4,
      figure_res = 150
    ),
    "Successfully exported 2 item"
  )
  expect_true(file.exists(output_file))

  extracted <- tempfile()
  on.exit(unlink(extracted, recursive = TRUE), add = TRUE)
  utils::unzip(output_file, exdir = extracted)
  media_files <- list.files(file.path(extracted, "word", "media"))
  expect_length(media_files, 1L)
})

test_that("export_word extracts plots from package result lists", {
  output_file <- tempfile(fileext = ".docx")
  on.exit(unlink(output_file), add = TRUE)
  result <- list(
    summary_data = head(mtcars),
    plot = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point()
  )

  expect_no_error(export_word(list(result), "Figure 1", output_file))
  expect_true(file.exists(output_file))
})

test_that("export_word exports an existing image file", {
  image_file <- tempfile(fileext = ".png")
  output_file <- tempfile(fileext = ".docx")
  on.exit(unlink(c(image_file, output_file)), add = TRUE)
  grDevices::png(image_file, width = 400, height = 300)
  graphics::plot(mtcars$wt, mtcars$mpg)
  grDevices::dev.off()

  expect_no_error(export_word(list(image_file), "Figure 1", output_file))
  expect_true(file.exists(output_file))
})

test_that("export_word exports base R plot instructions", {
  output_file <- tempfile(fileext = ".docx")
  on.exit(unlink(output_file), add = TRUE)
  base_plot <- officer::plot_instr(
    graphics::plot(mtcars$wt, mtcars$mpg)
  )

  expect_no_error(export_word(list(base_plot), "Figure 1", output_file))
  expect_true(file.exists(output_file))
})
