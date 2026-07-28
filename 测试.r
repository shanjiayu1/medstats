


remotes::install_github("shanjiayu1/medstats")

library(medstats)
#ceshi

ft <- format_flextable(
  head(mtcars[, 1:5])
)
ft
# Format a gtsummary object

tbl <- tbl_summary(
  trial,
  include = c(age, grade, trt),
  by = trt
)

ft <- format_flextable(tbl)
ft

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

my_clinical_data <- ChickWeight |>
  mutate(
    is_reach_150g = ifelse(weight >= 150, 1, 0)
  )
surv_data <- long_to_surv_data(
  data = my_clinical_data,
  id_var = "Chick",
  event_flag_var = "is_reach_150g",
  time_var = "Time",
  baseline_vars = c("Diet")
)

options(timeout =1200)
install.packages("TH.data")

make_table1(
  data = trial,
  vars = c("age", "marker", "stage", "grade"),
  specific_vars = "marker",  # Variable with a non-normal distribution
  group_var = "trt"
)

run_glm_auto(
  data = mtcars,
  vars = c("hp", "wt"),
  outcome_var = "mpg",
  family = "gaussian"
)

run_glm_auto(
  data = trial,
  vars = c("age", "stage"),
  outcome_var = "response",
  family = "binomial"
)

run_cox_auto(
  data = survival::lung,
  vars = c("age", "sex"),
  time_var = "time",
  event_var = "status"
)


plot_forest(
  data = import("森林图260424.xlsx", sheet = 1, trim_ws = FALSE),
  ci_column = 2, 
  x_ticks = c(0, 0.5,1,1.5,2),
  width = 6.5, 
  output_name = "TCM_Forestplot1.png"
)
f_data <- run_glm_auto(
  data = trial,
  vars = c("age", "stage"),
  outcome_var = "response",
  family = "binomial"
)

plot_forest(
  data = f_data[1:3],
  ci_column = 2, 
  x_ticks = c(0, 0.5,1,1.5,2),
  width = 6.5, 
  output_name = "TCM_Forestplot2.png"
)

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

remotes::install_github("nx10/httpgd")
library(httpgd)

install.packages("rstatix")
lung <- survival::lung

lung_fixed <- survival::lung
lung_fixed$status <- ifelse(lung_fixed$status == 2, 1, 0) 

# ==========================================
# 4. 调用函数进行测试
# ==========================================
# 注意：肺癌数据集的 time 是“天”，所以我们改一下坐标范围和标签


test_results <- plot_km(
  data = lung_fixed,
  time_var = "time",             # 时间变量名称
  status_var = "status",         # 状态变量名称
  xlab = "随访时间 (天)",
  ylab = "累积死亡率 (%)",
  
  # 因为单位是“天”，所以需要调整以下三个参数，避免图缩作一团
  xlim = c(0, 1000),             # X轴范围设为 0 到 1000天
  break_time = 200,              # 每 200 天一个刻度
  pval_coord = c(200, 0.8),      # 把 P值 放在图的左上方 (X=200天处, Y=80%高度)
  
  show_risk_table = TRUE,        # 顺便开启底部风险表格测试
  save_filename = "Lung_Test_Cumulative_KM.png"
)


library(nlme)  
my_data_2groups <- Orthodont %>%
  as.data.frame() %>% 
  mutate(
    time_str = paste0(age, "岁") # 故意构造文本格式时间："8岁", "10岁"...
  )
results_2groups <- longdata_analysis(
  data          = my_data_2groups,
  id_col        = "Subject",      # 个体ID
  treatment_col = "Sex",          # 分组：男/女（仅2组）
  time_col      = "time_str",     # 时间：8岁/10岁...
  score_col     = "distance"      # 结局指标：距离
)

# 4. 打印结果
print(results_2groups)



#logsitic模型：马力(hp)对自动变速箱(am)的影响，调整体重(wt)，4个节点，OR图
res1 <- plot_rcs(
  data = mtcars,
  exposure = "hp",
  outcome = "am",
  covars = c("wt"),
  nk = 4,
  # ylim=c(0,5),
  model_type = "logistic",
  xlab = "马力(hp)",
  ylab = "自动变速箱概率"
)

res1$plot


#线性模型：体重(wt)对每加仑英里数(mpg)的影响，调整马力(hp)和排量(disp)，4个节点，预测值图
res2 <- plot_rcs(
  data = mtcars,
  exposure = "wt",
  outcome = "mpg",
  covars = c("hp", "disp"),
  model_type = "linear",
  # ylim=c(0,20),
  ylab = "Predicted MPG"
)

res2$plot

#cox模型：年龄(age)对生存时间(time)和状态(status)的影响
res3 <- plot_rcs(
  data = lung,
  exposure = "age",
  outcome = "Surv(time, status)",
  covars = c("sex", "ph.ecog"),
  model_type = "cox",
  ylab = "Hazard Ratio"
)

res3$plot
install.packages("Hmisc")

plot_rcs(
  data = mtcars,
  exposure = "wt",
  outcome = "mpg",
  model_type = "linear"
)




model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
train_data <- mtcars
train_data$pred_prob <- predict(model, newdata = train_data, type = "response")

res_train <- plot_roc(
  data = train_data, 
  true_var = "am", 
  pred_var = "pred_prob", 
  title = "训练集 ROC 曲线 (mtcars)",
  line_color = "#2E86AB"   # 蓝色
)
res_train$plot




my_test_data <- ChickWeight %>%
  filter(Time %in% c(0, 10, 20)) %>%
  mutate(
    Visit_Time = factor(paste0("第 ", Time, " 天"), levels = c("第 0 天", "第 10 天", "第 20 天")),
    Weight_Status = case_when(
      weight < 50  ~ "偏瘦 (Light)",
      weight >= 50 & weight < 150 ~ "正常 (Normal)",
      weight >= 150 ~ "超重 (Overweight)"
    ),
    Weight_Status = factor(Weight_Status, levels = c("偏瘦 (Light)", "正常 (Normal)", "超重 (Overweight)"))
  )

# ==========================================
# 4. 调用新版函数！
# ==========================================
sankey_plot <- plot_sankey(
  data           = my_test_data,
  id_var         = "Chick",           
  time_var       = "Visit_Time",      
  state_var      = "Weight_Status",   
  na_strategy    = "show",             # <--- 改为 "drop" 即可无缝切换为完整版
  missing_label  = "Drop-out (失访)"  # 可以自定义叫什么名字          
)



mtcars
