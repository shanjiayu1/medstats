test_that("run_glm_auto linear regression works", {
  result <- run_glm_auto(
    data = mtcars,
    vars = c("hp", "wt"),
    outcome_var = "mpg",
    family = "gaussian"
  )
  expect_s3_class(result, "data.frame")
  expect_true("Variable" %in% names(result))
  expect_true(grepl("Beta", names(result)[2]))
})

test_that("run_glm_auto logistic regression works", {
  skip_if_not_installed("gtsummary")
  result <- run_glm_auto(
    data = gtsummary::trial,
    vars = c("age"),
    outcome_var = "response",
    family = "binomial"
  )
  expect_s3_class(result, "data.frame")
  expect_true(grepl("OR", names(result)[2]))
})

test_that("run_cox_auto works", {
  skip_if_not_installed("survival")
  lung2 <- survival::lung
  lung2$status2 <- ifelse(lung2$status == 2, 1, 0)
  result <- run_cox_auto(
    data = lung2,
    vars = c("age", "sex"),
    time_var = "time",
    event_var = "status2"
  )
  expect_s3_class(result, "data.frame")
  expect_true("Variable" %in% names(result))
  expect_true(grepl("HR", names(result)[2]))
})

test_that("longdata_analysis works with 2 groups", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("geepack")
  test_data <- nlme::Orthodont |>
    as.data.frame() |>
    dplyr::mutate(time_str = paste0(age, "岁"))
  result <- longdata_analysis(
    data = test_data,
    id_col = "Subject",
    treatment_col = "Sex",
    time_col = "time_str",
    score_col = "distance"
  )
  expect_s3_class(result, "data.frame")
  expect_true("Sex" %in% names(result))
})
