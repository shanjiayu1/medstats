test_that("long_to_surv_data produces correct structure", {
  test_data <- data.frame(
    id = rep(1:3, each = 3),
    event = c(0, 0, 1, 0, 0, 0, 0, 1, 0),
    time = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    age = rep(c(50, 60, 70), each = 3)
  )
  result <- long_to_surv_data(
    data = test_data,
    id_var = "id",
    event_flag_var = "event",
    time_var = "time",
    baseline_vars = c("age")
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("status" %in% names(result))
  expect_true("time" %in% names(result))
  # Subject 1 had event at time 3
  expect_equal(result$status[result$id == 1], 1)
  expect_equal(result$time[result$id == 1], 3)
  # Subject 2 was censored, time should be max
  expect_equal(result$status[result$id == 2], 0)
  expect_equal(result$time[result$id == 2], 3)
})

test_that("make_table1 returns gtsummary object", {
  skip_if_not_installed("gtsummary")
  result <- make_table1(
    data = gtsummary::trial,
    vars = c("age", "grade"),
    group_var = "trt"
  )
  expect_s3_class(result, "gtsummary")
})

test_that("make_table1 works without group_var", {
  skip_if_not_installed("gtsummary")
  result <- make_table1(
    data = gtsummary::trial,
    vars = c("age", "grade")
  )
  expect_s3_class(result, "gtsummary")
})
