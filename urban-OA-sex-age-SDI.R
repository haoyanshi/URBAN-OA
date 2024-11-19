# 1. 加载必要的R包
library(tidyverse)
library(ggplot2)
library(scales)
library(ggnewscale)

# 2. 加载数据
OA_data_dan <- read.csv('全球+收入地区疾病负担.csv') 
Global_2021_AgeandSex <- OA_data_dan

# 3. 数据清洗和处理

# 3.1 提取年龄段信息
Global_2021_AgeandSex$age_name <- 
  str_split(Global_2021_AgeandSex$age_name, ' ', simplify = TRUE)[,1]

# 定义年龄段顺序
age_levels <- c("1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64",
                "65-69","70-74","75-79","80-84","85-89","90-94","95+")

# 检查数据中的实际收入地区名称
unique(Global_2021_AgeandSex$location_name)
# [1] "Global"          "Middle SDI"      "High SDI"        "High-middle SDI" "Low-middle SDI"  "Low SDI"

# 定义收入地区的顺序（从高到低）
income_levels <- c("High SDI", "High-middle SDI", "Middle SDI", 
                   "Low-middle SDI", "Low SDI")
unique(Global_2021_AgeandSex$measure_name)
# 3.2 准备柱状图数据（不同收入地区）
BarChartData <- Global_2021_AgeandSex %>%
  filter(
    location_name %in% income_levels,
    year == "2021",
    age_name %in% age_levels,
    sex_name %in% c("Male", "Female"),
    measure_name == 'DALYs (Disability-Adjusted Life Years)',
    metric_name == 'Number'
  ) %>%
  mutate(
    val = val / 1e3,  # 转换为千
    # 将女性的数据取负值
    val = if_else(sex_name == 'Female', -val, val)
  )

# 3.3 准备折线图数据（全球数据）
LineChartData <- Global_2021_AgeandSex %>%
  filter(
    location_name == "Global",
    year == "2021",
    age_name %in% age_levels,
    sex_name %in% c("Male", "Female"),
    measure_name == 'DALYs (Disability-Adjusted Life Years)',
    metric_name == 'Rate'
  ) %>%
  mutate(
    val = val ,
    upper = upper ,
    lower = lower ,
    # 将女性的数据取负值
    val = if_else(sex_name == 'Female', -val, val),
    upper = if_else(sex_name == 'Female', -upper, upper),
    lower = if_else(sex_name == 'Female', -lower, lower)
  )

# 3.4 设置因子顺序并创建性别和SDI组合变量
BarChartData <- BarChartData %>%
  mutate(
    age_name = factor(age_name, levels = age_levels),
    sex_name = factor(sex_name, levels = c('Male', 'Female')),
    location_name = factor(location_name, levels = income_levels),
    Sex_SDI = paste(sex_name, location_name, sep = "_")
  )

LineChartData <- LineChartData %>%
  mutate(
    age_name = factor(age_name, levels = age_levels),
    sex_name = factor(sex_name, levels = c('Male', 'Female'))
  )

# 定义 Sex_SDI 的因子水平顺序，以控制柱状图的堆叠顺序
# 首先定义性别和SDI组合的顺序
sex_sdi_levels <- c()

# 对于男性，从高到低SDI
for (sdi in income_levels) {
  sex_sdi_levels <- c(sex_sdi_levels, paste("Male", sdi, sep = "_"))
}

# 对于女性，从高到低SDI
for (sdi in income_levels) {
  sex_sdi_levels <- c(sex_sdi_levels, paste("Female", sdi, sep = "_"))
}

# 将 Sex_SDI 转换为因子，并按照 sex_sdi_levels 定义顺序
BarChartData$Sex_SDI <- factor(BarChartData$Sex_SDI, levels = sex_sdi_levels)

# 为男性定义从蓝色到绿色的渐变颜色
male_colors <- colorRampPalette(c("#8D0128", "#FDF7DA"))(length(income_levels))
# 为女性定义从黄色到红色的渐变颜色
female_colors <- colorRampPalette(c("#30327E", "#EBF3F8"))(length(income_levels))#ffaaa5

# 合并男性和女性的颜色
sex_sdi_colors <- c(male_colors, female_colors)
names(sex_sdi_colors) <- sex_sdi_levels

# 计算缩放因子
max_bar <- max(abs(BarChartData$val), na.rm = TRUE)
max_line <- max(abs(LineChartData$val), na.rm = TRUE)
scaling_factor <- max_bar / max_line

# 缩放折线图数据
LineChartData <- LineChartData %>%
  mutate(
    val_scaled = val * scaling_factor,
    upper_scaled = upper * scaling_factor,
    lower_scaled = lower * scaling_factor
  )

# 绘制图形
plot <- ggplot() +
  geom_col(
    data = BarChartData,
    aes(x = age_name, y = val, fill = Sex_SDI),
    width = 0.6
  ) +
  scale_fill_manual(
    name = "Sex and SDI Level",
    values = sex_sdi_colors
  ) +
  new_scale_fill() +
  geom_line(
    data = LineChartData,
    aes(x = as.numeric(age_name), y = val_scaled, group = sex_name, color = sex_name),
    linetype = "solid",
    size = 1
  ) +
  geom_ribbon(
    data = LineChartData,
    aes(x = as.numeric(age_name), ymin = lower_scaled, ymax = upper_scaled, group = sex_name, fill = sex_name),
    alpha = 0.2
  ) +
  scale_fill_manual(
    name = "Sex",
    values = c("Male" = "#FB9A99", "Female" = "#A6CEE3")
  ) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = "#E31A1C", "Female" = "#1F78B4")
  ) +
  guides(fill = guide_legend(order = 1, ncol = 2), color = guide_legend(order = 2)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    legend.position = 'top',
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.5, 'cm'),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(
    name = "Total DALYs number (thousands)",
    labels = abs,
    expand = expansion(mult = c(0.02, 0.1)),
    sec.axis = sec_axis(~ . / scaling_factor, name = "DALYs per 100,000 population", labels = abs)
  ) +
  xlab('Age group (years)')

print(plot)

# 保存图形
ggsave(plot, file = 'DALYs_age_and_sex_2021_updated1.pdf', units = 'cm', height = 18, width = 30)
