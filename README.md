# medstats

**medstats** is an R package designed to streamline common workflows in medical and epidemiological statistics. It provides convenient functions for data processing, statistical modeling, table generation, and publication-quality visualization.

Key features include:

- **Table formatting**: Create publication-ready three-line tables and export them to Word
- **Data processing**: Convert longitudinal data into survival-analysis format and generate baseline characteristic tables
- **Regression analysis**: Perform automated generalized linear models and Cox proportional hazards regression, including univariable and multivariable analyses
- **Repeated-measures analysis**: Fit generalized estimating equation models for longitudinal and correlated data
- **Publication-quality visualization**: Generate Kaplan–Meier curves, forest plots, restricted cubic spline plots, ROC curves, Sankey diagrams, and more

## Installation

Install the development version of **medstats** from GitHub:

```r
# Install the remotes package if necessary
install.packages("remotes")

# Install medstats from GitHub
remotes::install_github("shanjiayu1/medstats")

# Load the package
library(medstats)
```

## Main Features

### Table Formatting

#### Format a `flextable`

The `format_flextable()` function applies a consistent, publication-ready three-line table style.

```r
# Format a data frame
ft <- format_flextable(
  head(mtcars[, 1:5])
)

# Format a gtsummary object
library(gtsummary)

tbl <- tbl_summary(
  trial,
  include = c(age, grade, trt),
  by = trt
)

ft <- format_flextable(tbl)
```

#### Export tables to Word

The `export_word()` function can export multiple tables to a single Word document.

```r
export_word(
  data_list = list(
    head(mtcars),
    head(iris)
  ),
  table_titles = c(
    "Table 1. mtcars Dataset",
    "Table 2. iris Dataset"
  ),
  output_file = "my_tables.docx"
)
```

## Data Processing

### Convert longitudinal data to survival-analysis format

The `long_to_surv_data()` function converts longitudinal clinical records into a dataset suitable for survival analysis.

```r
surv_data <- long_to_surv_data(
  data = my_clinical_data,
  id_var = "PatientID",
  event_flag_var = "is_event",
  time_var = "follow_up_time",
  baseline_vars = c("Sex", "Age")
)
```

### Create a baseline characteristics table

The `make_table1()` function generates a baseline characteristics table commonly used as Table 1 in medical research.

```r
make_table1(
  data = trial,
  vars = c("age", "marker", "stage", "grade"),
  specific_vars = "marker",  # Variable with a non-normal distribution
  group_var = "trt"
)
```

## Regression Analysis

### Automated generalized linear models

The `run_glm_auto()` function performs automated generalized linear model analyses and summarizes both univariable and multivariable results.

#### Linear regression

```r
run_glm_auto(
  data = mtcars,
  vars = c("hp", "wt"),
  outcome_var = "mpg",
  family = "gaussian"
)
```

#### Logistic regression

```r
run_glm_auto(
  data = trial,
  vars = c("age", "stage"),
  outcome_var = "response",
  family = "binomial"
)
```

### Automated Cox regression

The `run_cox_auto()` function performs automated Cox proportional hazards regression.

```r
run_cox_auto(
  data = lung,
  vars = c("age", "sex"),
  time_var = "time",
  event_var = "status"
)
```

## Data Visualization

### Mean ± standard error line plot

The `plot_meanse()` function visualizes longitudinal changes using group-specific means and standard errors.

```r
my_data <- ChickWeight |>
  filter(Time %in% c(0, 4, 10, 14, 21)) |>   # 挑选第 0,4,10,14,21 天
  mutate(
    time_str = paste0("第", Time, "天"),      # 制造 "第10天" 这种字符串
    Diet_Name = paste0("饮食配方", Diet)      # 把组别从 1,2,3,4 改为有意义的名称
  )

plot_meanse(
  data         = my_data,
  target_var   = "weight",         # 结局指标：体重
  time_var     = "time_str",       # 时间列名："第x天"
  group_var    = "Diet_Name",      # 分组变量："饮食配方x"
  xlab         = "生长天数 (Days)",# 自定义 X 轴标签
  ylab         = "平均体重 (g)",   # 自定义 Y 轴标签
  legend_title = "不同饮食分组",
)
```
<img width="612" height="612" alt="示例图片" src="https://github.com/user-attachments/assets/b1b9737c-8300-46d8-a4da-b10d312931f8" />

### Stacked percentage bar plot

The `plot_stacked()` function categorizes a continuous variable into
intervals and visualizes the percentage distribution of those categories
at each time point.

```r
chick_weight <- ChickWeight
chick_weight$Time <- factor(
  chick_weight$Time,
  levels = sort(unique(chick_weight$Time))
)

plot_stacked(
  data = chick_weight,
  target_var = "weight",
  time_var = "Time",
  breaks = c(-Inf, 100, 200, 300, Inf),
  labels = c("≤100 g", "101–200 g", "201–300 g", ">300 g"),
  colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
  legend_title = "Weight range"
)
```

### Kaplan–Meier survival curve

The `plot_km()` function generates publication-ready Kaplan–Meier survival curves.

```r
lung$status2 <- ifelse(
  lung$status == 2,
  1,
  0
)

plot_km(
  data = lung,
  group_var = "sex",
  time_var = "time",
  status_var = "status2"
)
```

### Restricted cubic spline plot

The `plot_rcs()` function evaluates and visualizes potential nonlinear associations using restricted cubic splines.

```r
plot_rcs(
  data = mtcars,
  exposure = "wt",
  outcome = "mpg",
  model_type = "linear"
)
```

### ROC curve

The `plot_roc()` function evaluates the discrimination performance of a prediction model using a receiver operating characteristic curve.

```r
model <- glm(
  am ~ mpg + hp + wt,
  data = mtcars,
  family = binomial
)

mtcars$pred <- predict(
  model,
  type = "response"
)

plot_roc(
  data = mtcars,
  true_var = "am",
  pred_var = "pred"
)
```
