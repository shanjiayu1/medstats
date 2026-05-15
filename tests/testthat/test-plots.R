test_that("plot_meanse returns list with plot and data", {
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(time_str = paste0("第", Time, "天"))

  result <- plot_meanse(
    data = test_data,
    target_var = "weight",
    time_var = "time_str",
    group_var = NULL
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("summary_data" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("plot_stacked returns list with plot and data", {
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(Time_str = factor(paste0("第", Time, "天")))

  result <- plot_stacked(
    data = test_data,
    target_var = "weight",
    time_var = "Time_str",
    breaks = c(-Inf, 50, 100, 200, Inf),
    labels = c("<=50", "51-100", "101-200", ">200"),
    colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F")
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("plot_roc returns list with correct elements", {
  skip_if_not_installed("pROC")
  model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
  mtcars2 <- mtcars
  mtcars2$pred_prob <- predict(model, newdata = mtcars2, type = "response")

  result <- plot_roc(
    data = mtcars2,
    true_var = "am",
    pred_var = "pred_prob",
    show_print = FALSE
  )
  expect_type(result, "list")
  expect_true("auc" %in% names(result))
  expect_true("plot" %in% names(result))
  expect_true(result$auc > 0.5)
})

test_that("plot_rcs returns list with plot", {
  skip_if_not_installed("rms")
  result <- plot_rcs(
    data = mtcars,
    exposure = "wt",
    outcome = "mpg",
    model_type = "linear"
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("plot_sankey returns ggplot object", {
  skip_if_not_installed("ggalluvial")
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(
      Visit = factor(paste0("Day", Time), levels = c("Day0", "Day10", "Day20")),
      Status = dplyr::case_when(
        weight < 80 ~ "Light",
        weight >= 80 & weight < 150 ~ "Normal",
        weight >= 150 ~ "Heavy"
      )
    )

  result <- plot_sankey(
    data = test_data,
    id_var = "Chick",
    time_var = "Visit",
    state_var = "Status",
    na_strategy = "drop"
  )
  expect_s3_class(result, "ggplot")
})
