test_that("format_flextable works with data.frame", {
  ft <- format_flextable(head(mtcars[, 1:4]))
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable works with flextable input", {
  ft_input <- flextable::flextable(head(iris))
  ft <- format_flextable(ft_input)
  expect_s3_class(ft, "flextable")
})

test_that("export_word validates inputs", {
  expect_error(export_word("not_a_list", c("t1")), "list")
  expect_error(export_word(list(1, 2), c("only_one")), "不一致")
  expect_error(export_word(list(1), c("t1"), output_file = "test.txt"), ".docx")
})
