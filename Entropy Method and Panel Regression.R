#加载包
library(dplyr)
library(plm)
library(tidyverse)
library(stringr)
library(sandwich)

# 正向指标列表
positive_indicators <- c("urban", "employ","largecity", "GDP", 
                         "GNI", "NPISH","elect", "internet")
# 负向指标列表
negative_indicators <- c("co2", "PM2.5")

# 自定义标准化函数
normalize <- function(x, year, type){
  max_val <- max(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  range_val <- max_val - min_val
  if (range_val > 0) {
    if (type == "positive") {
      (x - min_val) / range_val
    } else {
      (max_val - x) / range_val
    }
  } else {
    rep(0.5, length(x))
  }
}

# 对数据进行年份和指标基础上的标准化
df_panel <- df_merged %>%
  mutate(across(all_of(positive_indicators), ~normalize(., year, "positive")),
         across(all_of(negative_indicators), ~normalize(., year, "negative"))) %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, 0.001, .)))


load('合并后总数据-标准化.RData')
load('204个国家的OA数据.RData')
#筛选OA的数据
OA_ASPR  <- Countries_1990to2021 %>%
  filter(sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "Prevalence",
         metric_name == "Rate") %>%
  select("location_name","val","year")
OA_ASDR  <- Countries_1990to2021 %>%
  filter(sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate") %>%
  select("location_name","val","year")
#修改列名以便下一步和城市化数据进行匹配、合并
colnames(OA_ASPR)[1] <- "Country"
colnames(OA_ASPR)[3] <- "Year"
colnames(OA_ASDR)[1] <- "Country"
colnames(OA_ASDR)[3] <- "Year"
#确认数据匹配情况
# 假设 df_panel, OA_ASPR, 和 OA_ASDR 已经加载到 R 环境中

# 提取 df_panel 中的 Country 列
df_panel_countries <- unique(df_panel$Country)

# 提取 OA_ASPR 中的 Country 列
OA_ASPR_countries <- unique(OA_ASPR$Country)

# 提取 OA_ASDR 中的 Country 列
OA_ASDR_countries <- unique(OA_ASDR$Country)

# 检查 df_panel 中的 Country 是否在 OA_ASPR 和 OA_ASDR 的 Country 列中
missing_in_OA_ASPR <- setdiff(df_panel_countries, OA_ASPR_countries)
missing_in_OA_ASDR <- setdiff(df_panel_countries, OA_ASDR_countries)

# 输出找不到的内容
if (length(missing_in_OA_ASPR) > 0) {
  cat("在 OA_ASPR 中找不到的 Country 有:\n")
  print(missing_in_OA_ASPR)
} else {
  cat("所有 Country 在 OA_ASPR 中都能找到。\n")
}

if (length(missing_in_OA_ASDR) > 0) {
  cat("在 OA_ASDR 中找不到的 Country 有:\n")
  print(missing_in_OA_ASDR)
} else {
  cat("所有 Country 在 OA_ASDR 中都能找到。\n")
}

#协调不一致的数据
# 替换 OA_ASPR 中的 Country 列
OA_ASPR <- OA_ASPR %>%
  mutate(Country = str_replace_all(Country, "Türkiye", "Turkiye"),
         Country = str_replace_all(Country, "Côte d'Ivoire", "Cote d'Ivoire"))

# 替换 OA_ASDR 中的 Country 列
OA_ASDR <- OA_ASDR %>%
  mutate(Country = str_replace_all(Country, "Türkiye", "Turkiye"),
         Country = str_replace_all(Country, "Côte d'Ivoire", "Cote d'Ivoire"))
#标记列名
colnames(OA_ASPR)[2] <- "OA_ASPR"
colnames(OA_ASDR)[2] <- "OA_ASDR"
# 将 OA_ASPR 中的 Year 列转换为字符串格式
OA_ASPR <- OA_ASPR %>%
  mutate(Year = as.character(Year))

# 将 OA_ASDR 中的 Year 列转换为字符串格式
OA_ASDR <- OA_ASDR %>%
  mutate(Year = as.character(Year))
#数据合并
df_OA_urban <- df_panel %>%
  inner_join(OA_ASPR, by = c("Country", "Year"))
df_OA_urban <- df_OA_urban %>%
  inner_join(OA_ASDR, by = c("Country", "Year"))
summary(df_OA_urban)


# 面板数据模型分析
pdata <- pdata.frame(df_OA_urban, index = c("Country", "Year"))
save(pdata,file='面板数据.RData')
load('面板数据.RData')

#城市人口
fe_model_ASPR_urban <- plm(OA_ASPR ~ urban , data = pdata, model = "within")
summary(fe_model_ASPR_urban, vcov = vcovHC(fe_model_ASPR_urban, type = "HC1"))
confint(fe_model_ASPR_urban, level = 0.95, vcov = vcovHC(fe_model_ASPR_urban, type = "HC1"))

re_model_ASPR_urban <- plm(OA_ASPR ~ urban , data = pdata, model = "random")
summary(re_model_ASPR_urban, vcov = vcovHC(re_model_ASPR_urban, type = "HC1"))
confint(re_model_ASPR_urban, level = 0.95, vcov = vcovHC(re_model_ASPR_urban, type = "HC1"))

hausman_test_ASPR_urban <- phtest(fe_model_ASPR_urban, re_model_ASPR_urban)

fe_model_ASDR_urban <- plm(OA_ASDR ~ urban , data = pdata, model = "within")
summary(fe_model_ASDR_urban, vcov = vcovHC(fe_model_ASDR_urban, type = "HC1"))
confint(fe_model_ASDR_urban, level = 0.95, vcov = vcovHC(fe_model_ASDR_urban, type = "HC1"))

re_model_ASDR_urban <- plm(OA_ASDR ~ urban , data = pdata, model = "random")
summary(re_model_ASDR_urban, vcov = vcovHC(re_model_ASDR_urban, type = "HC1"))
confint(re_model_ASDR_urban, level = 0.95, vcov = vcovHC(re_model_ASDR_urban, type = "HC1"))

hausman_test_ASDR_urban <- phtest(fe_model_ASDR_urban, re_model_ASDR_urban)

#就业比例
fe_model_ASPR_employ <- plm(OA_ASPR ~ employ , data = pdata, model = "within")
summary(fe_model_ASPR_employ, vcov = vcovHC(fe_model_ASPR_employ, type = "HC1"))
confint(fe_model_ASPR_employ, level = 0.95, vcov = vcovHC(fe_model_ASPR_employ, type = "HC1"))

re_model_ASPR_employ <- plm(OA_ASPR ~ employ , data = pdata, model = "random")
summary(re_model_ASPR_employ, vcov = vcovHC(re_model_ASPR_employ, type = "HC1"))
confint(re_model_ASPR_employ, level = 0.95, vcov = vcovHC(re_model_ASPR_employ, type = "HC1"))

hausman_test_ASPR_employ <- phtest(fe_model_ASPR_employ, re_model_ASPR_employ)

fe_model_ASDR_employ <- plm(OA_ASDR ~ employ , data = pdata, model = "within")
summary(fe_model_ASDR_employ, vcov = vcovHC(fe_model_ASDR_employ, type = "HC1"))
confint(fe_model_ASDR_employ, level = 0.95, vcov = vcovHC(fe_model_ASDR_employ, type = "HC1"))

re_model_ASDR_employ <- plm(OA_ASDR ~ employ , data = pdata, model = "random")
summary(re_model_ASDR_employ, vcov = vcovHC(re_model_ASDR_employ, type = "HC1"))
confint(re_model_ASDR_employ, level = 0.95, vcov = vcovHC(re_model_ASDR_employ, type = "HC1"))

hausman_test_ASDR_employ <- phtest(fe_model_ASDR_employ, re_model_ASDR_employ)

#最大城市人口
fe_model_ASPR_largecity <- plm(OA_ASPR ~ largecity , data = pdata, model = "within")
summary(fe_model_ASPR_largecity,vcov = vcovHC)
confint(fe_model_ASPR_largecity, level = 0.95, vcov = vcovHC(fe_model_ASPR_largecity, type = "HC1"))

re_model_ASPR_largecity <- plm(OA_ASPR ~ largecity, data = pdata, model = "random")
summary(re_model_ASPR_largecity, vcov = vcovHC(re_model_ASPR_largecity, type = "HC1"))
confint(re_model_ASPR_largecity, level = 0.95, vcov = vcovHC(re_model_ASPR_largecity, type = "HC1"))

hausman_test_ASPR_largecity <- phtest(fe_model_ASPR_largecity, re_model_ASPR_largecity)

fe_model_ASDR_largecity <- plm(OA_ASDR ~ largecity , data = pdata, model = "within")
summary(fe_model_ASDR_largecity,vcov = vcovHC)
confint(fe_model_ASDR_largecity, level = 0.95, vcov = vcovHC(fe_model_ASDR_largecity, type = "HC1"))

re_model_ASDR_largecity <- plm(OA_ASDR ~ largecity, data = pdata, model = "random")
summary(re_model_ASDR_largecity, vcov = vcovHC(re_model_ASDR_largecity, type = "HC1"))
confint(re_model_ASDR_largecity, level = 0.95, vcov = vcovHC(re_model_ASDR_largecity, type = "HC1"))

hausman_test_ASDR_largecity <- phtest(fe_model_ASDR_largecity, re_model_ASDR_largecity)

#GDP
fe_model_ASPR_GDP <- plm(OA_ASPR ~ GDP , data = pdata, model = "within")
summary(fe_model_ASPR_GDP,vcov = vcovHC)
confint(fe_model_ASPR_GDP, level = 0.95, vcov = vcovHC(fe_model_ASPR_GDP, type = "HC1"))

re_model_ASPR_GDP <- plm(OA_ASPR ~ GDP , data = pdata, model = "random")
summary(re_model_ASPR_GDP,vcov = vcovHC)
confint(re_model_ASPR_GDP, level = 0.95, vcov = vcovHC(re_model_ASPR_GDP, type = "HC1"))

hausman_test_ASPR_GDP <- phtest(fe_model_ASPR_GDP, re_model_ASPR_GDP)

fe_model_ASDR_GDP <- plm(OA_ASDR ~ GDP , data = pdata, model = "within")
summary(fe_model_ASDR_GDP,vcov = vcovHC)
confint(fe_model_ASDR_GDP, level = 0.95, vcov = vcovHC(fe_model_ASDR_GDP, type = "HC1"))

re_model_ASDR_GDP <- plm(OA_ASDR ~ GDP , data = pdata, model = "random")
summary(re_model_ASDR_GDP,vcov = vcovHC)
confint(re_model_ASDR_GDP, level = 0.95, vcov = vcovHC(re_model_ASDR_GDP, type = "HC1"))

hausman_test_ASDR_GDP <- phtest(fe_model_ASDR_GDP, re_model_ASDR_GDP)

#GNI
fe_model_ASPR_GNI <- plm(OA_ASPR ~ GNI , data = pdata, model = "within")
summary(fe_model_ASPR_GNI,vcov = vcovHC)
confint(fe_model_ASPR_GNI, level = 0.95, vcov = vcovHC(fe_model_ASPR_GNI, type = "HC1"))

re_model_ASPR_GNI <- plm(OA_ASPR ~ GNI , data = pdata, model = "random")
summary(re_model_ASPR_GNI,vcov = vcovHC)
confint(re_model_ASPR_GNI, level = 0.95, vcov = vcovHC(re_model_ASPR_GNI, type = "HC1"))

hausman_test_ASPR_GNI <- phtest(fe_model_ASPR_GNI, re_model_ASPR_GNI)

fe_model_ASDR_GNI <- plm(OA_ASDR ~ GNI , data = pdata, model = "within")
summary(fe_model_ASDR_GNI,vcov = vcovHC)
confint(fe_model_ASDR_GNI, level = 0.95, vcov = vcovHC(fe_model_ASDR_GNI, type = "HC1"))

re_model_ASDR_GNI <- plm(OA_ASDR ~ GNI , data = pdata, model = "random")
summary(re_model_ASDR_GNI,vcov = vcovHC)
confint(re_model_ASDR_GNI, level = 0.95, vcov = vcovHC(re_model_ASDR_GNI, type = "HC1"))

hausman_test_ASDR_GNI <- phtest(fe_model_ASDR_GNI, re_model_ASDR_GNI)

#NPISH
fe_model_ASPR_NPISH <- plm(OA_ASPR ~ NPISH , data = pdata, model = "within")
summary(fe_model_ASPR_NPISH,vcov = vcovHC)
confint(fe_model_ASPR_NPISH, level = 0.95, vcov = vcovHC(fe_model_ASPR_NPISH, type = "HC1"))

re_model_ASPR_NPISH <- plm(OA_ASPR ~ NPISH , data = pdata, model = "random")
summary(re_model_ASPR_NPISH,vcov = vcovHC)
confint(re_model_ASPR_NPISH, level = 0.95, vcov = vcovHC(re_model_ASPR_NPISH, type = "HC1"))

hausman_test_ASPR_NPISH <- phtest(fe_model_ASPR_NPISH, re_model_ASPR_NPISH)

fe_model_ASDR_NPISH <- plm(OA_ASDR ~ NPISH , data = pdata, model = "within")
summary(fe_model_ASDR_NPISH,vcov = vcovHC)
confint(fe_model_ASDR_NPISH, level = 0.95, vcov = vcovHC(fe_model_ASDR_NPISH, type = "HC1"))

re_model_ASDR_NPISH <- plm(OA_ASDR ~ NPISH , data = pdata, model = "random")
summary(re_model_ASDR_NPISH,vcov = vcovHC)
confint(re_model_ASDR_NPISH, level = 0.95, vcov = vcovHC(re_model_ASDR_NPISH, type = "HC1"))

hausman_test_ASDR_NPISH <- phtest(fe_model_ASDR_NPISH, re_model_ASDR_NPISH)

#elect
fe_model_ASPR_elect <- plm(OA_ASPR ~ elect , data = pdata, model = "within")
summary(fe_model_ASPR_elect,vcov = vcovHC)
confint(fe_model_ASPR_elect, level = 0.95, vcov = vcovHC(fe_model_ASPR_elect, type = "HC1"))

re_model_ASPR_elect <- plm(OA_ASPR ~ elect , data = pdata, model = "random")
summary(re_model_ASPR_elect,vcov = vcovHC)
confint(re_model_ASPR_elect, level = 0.95, vcov = vcovHC(re_model_ASPR_elect, type = "HC1"))

hausman_test_ASPR_elect <- phtest(fe_model_ASPR_elect, re_model_ASPR_elect)

fe_model_ASDR_elect <- plm(OA_ASDR ~ elect , data = pdata, model = "within")
summary(fe_model_ASDR_elect,vcov = vcovHC)
confint(fe_model_ASDR_elect, level = 0.95, vcov = vcovHC(fe_model_ASDR_elect, type = "HC1"))

re_model_ASDR_elect <- plm(OA_ASDR ~ elect , data = pdata, model = "random")
summary(re_model_ASDR_elect,vcov = vcovHC)
confint(re_model_ASDR_elect, level = 0.95, vcov = vcovHC(re_model_ASDR_elect, type = "HC1"))

hausman_test_ASDR_elect <- phtest(fe_model_ASDR_elect, re_model_ASDR_elect)

#internet
fe_model_ASPR_internet <- plm(OA_ASPR ~ internet , data = pdata, model = "within")
summary(fe_model_ASPR_internet,vcov = vcovHC)
confint(fe_model_ASPR_internet, level = 0.95, vcov = vcovHC(fe_model_ASPR_internet, type = "HC1"))

re_model_ASPR_internet <- plm(OA_ASPR ~ internet , data = pdata, model = "random")
summary(re_model_ASPR_internet,vcov = vcovHC)
confint(re_model_ASPR_internet, level = 0.95, vcov = vcovHC(re_model_ASPR_internet, type = "HC1"))

hausman_test_ASPR_internet <- phtest(fe_model_ASPR_internet, re_model_ASPR_internet)

fe_model_ASDR_internet <- plm(OA_ASDR ~ internet , data = pdata, model = "within")
summary(fe_model_ASDR_internet,vcov = vcovHC)
confint(fe_model_ASDR_internet, level = 0.95, vcov = vcovHC(fe_model_ASDR_internet, type = "HC1"))

re_model_ASDR_internet <- plm(OA_ASDR ~ internet , data = pdata, model = "random")
summary(re_model_ASDR_internet,vcov = vcovHC)
confint(re_model_ASDR_internet, level = 0.95, vcov = vcovHC(re_model_ASDR_internet, type = "HC1"))

hausman_test_ASDR_internet <- phtest(fe_model_ASDR_internet, re_model_ASDR_internet)

#co2
fe_model_ASPR_co2 <- plm(OA_ASPR ~ co2 , data = pdata, model = "within")
summary(fe_model_ASPR_co2,vcov = vcovHC)
confint(fe_model_ASPR_co2, level = 0.95, vcov = vcovHC(fe_model_ASPR_co2, type = "HC1"))

re_model_ASPR_co2 <- plm(OA_ASPR ~ co2 , data = pdata, model = "random")
summary(re_model_ASPR_co2,vcov = vcovHC)
confint(re_model_ASPR_co2, level = 0.95, vcov = vcovHC(re_model_ASPR_co2, type = "HC1"))

hausman_test_ASPR_co2 <- phtest(fe_model_ASPR_co2, re_model_ASPR_co2)

fe_model_ASDR_co2 <- plm(OA_ASDR ~ co2 , data = pdata, model = "within")
summary(fe_model_ASDR_co2,vcov = vcovHC)
confint(fe_model_ASDR_co2, level = 0.95, vcov = vcovHC(fe_model_ASDR_co2, type = "HC1"))

re_model_ASDR_co2 <- plm(OA_ASDR ~ co2 , data = pdata, model = "random")
summary(re_model_ASDR_co2,vcov = vcovHC)
confint(re_model_ASDR_co2, level = 0.95, vcov = vcovHC(re_model_ASDR_co2, type = "HC1"))

hausman_test_ASDR_co2 <- phtest(fe_model_ASDR_co2, re_model_ASDR_co2)

#PM2.5
fe_model_ASPR_PM2.5 <- plm(OA_ASPR ~ PM2.5 , data = pdata, model = "within")
summary(fe_model_ASPR_PM2.5,vcov = vcovHC)
confint(fe_model_ASPR_PM2.5, level = 0.95, vcov = vcovHC(fe_model_ASPR_PM2.5, type = "HC1"))

re_model_ASPR_PM2.5 <- plm(OA_ASPR ~ PM2.5 , data = pdata, model = "random")
summary(re_model_ASPR_PM2.5,vcov = vcovHC)
confint(re_model_ASPR_PM2.5, level = 0.95, vcov = vcovHC(re_model_ASPR_PM2.5, type = "HC1"))

hausman_test_ASPR_PM2.5 <- phtest(fe_model_ASPR_PM2.5, re_model_ASPR_PM2.5)

fe_model_ASDR_PM2.5 <- plm(OA_ASDR ~ PM2.5 , data = pdata, model = "within")
summary(fe_model_ASDR_PM2.5,vcov = vcovHC)
confint(fe_model_ASDR_PM2.5, level = 0.95, vcov = vcovHC(fe_model_ASDR_PM2.5, type = "HC1"))

re_model_ASDR_PM2.5 <- plm(OA_ASDR ~ PM2.5 , data = pdata, model = "random")
summary(re_model_ASDR_PM2.5,vcov = vcovHC)
confint(re_model_ASDR_PM2.5, level = 0.95, vcov = vcovHC(re_model_ASDR_PM2.5, type = "HC1"))

hausman_test_ASDR_PM2.5 <- phtest(fe_model_ASDR_PM2.5, re_model_ASDR_PM2.5)

# 构建总面板回归模型，以 OA_ASPR 为因变量
#先构建综合指标变量
df_panel <- pdata

#人口
df_panel$DU_score <- df_panel$urban * 0.1234 + 
  df_panel$employ * 0.1418 + 
  df_panel$largecity * 0.0815
df_panel$DU_score <- df_panel$DU_score / 0.3467

# Economic urbanization (EU)
df_panel$EU_score <- df_panel$GDP * 0.0313 +
  df_panel$GNI * 0.0326 +
  df_panel$NPISH * 0.0348
df_panel$EU_score <- df_panel$EU_score / 0.0987

# Social Services Urbanization (SSU)
df_panel$SSU_score <- df_panel$elect * 0.1631 +
  df_panel$internet * 0.0578
df_panel$SSU_score <- df_panel$SSU_score / 0.2209

# Eco-environment urbanization (EEU)
df_panel$EEU_score <- df_panel$co2 * 0.1687 +
  df_panel$PM2.5 * 0.1648
df_panel$EEU_score <- df_panel$EEU_score / 0.3335

#人口综合指标
fe_model_aspr_DU <- plm(OA_ASPR ~ DU_score, data = df_panel, model = "within")
summary(fe_model_aspr_DU,vcov = vcovHC)
confint(fe_model_aspr_DU, level = 0.95, vcov = vcovHC(fe_model_aspr_DU, type = "HC1"))

re_model_aspr_DU <- plm(OA_ASPR ~ DU_score, data = df_panel, model = "random")
summary(re_model_aspr_DU,vcov = vcovHC)
confint(re_model_aspr_DU, level = 0.95, vcov = vcovHC(re_model_aspr_DU, type = "HC1"))

hausman_test_aspr_DU <- phtest(fe_model_aspr_DU, re_model_aspr_DU)

fe_model_asdr_DU <- plm(OA_ASDR ~ DU_score, data = df_panel, model = "within")
summary(fe_model_asdr_DU,vcov = vcovHC)
confint(fe_model_asdr_DU, level = 0.95, vcov = vcovHC(fe_model_asdr_DU, type = "HC1"))

re_model_asdr_DU <- plm(OA_ASDR ~ DU_score, data = df_panel, model = "random")
summary(re_model_asdr_DU,vcov = vcovHC)
confint(re_model_asdr_DU, level = 0.95, vcov = vcovHC(re_model_asdr_DU, type = "HC1"))

hausman_test_asdr_DU <- phtest(fe_model_asdr_DU, re_model_asdr_DU)

#经济综合指标
fe_model_aspr_EU <- plm(OA_ASPR ~ EU_score, data = df_panel, model = "within")
summary(fe_model_aspr_EU,vcov = vcovHC)
confint(fe_model_aspr_EU, level = 0.95, vcov = vcovHC(fe_model_aspr_EU, type = "HC1"))

re_model_aspr_EU <- plm(OA_ASPR ~ EU_score, data = df_panel, model = "random")
summary(re_model_aspr_EU,vcov = vcovHC)
confint(re_model_aspr_EU, level = 0.95, vcov = vcovHC(re_model_aspr_EU, type = "HC1"))

hausman_test_aspr_EU <- phtest(fe_model_aspr_EU, re_model_aspr_EU)

fe_model_asdr_EU <- plm(OA_ASDR ~ EU_score, data = df_panel, model = "within")
summary(fe_model_asdr_EU,vcov = vcovHC)
confint(fe_model_asdr_EU, level = 0.95, vcov = vcovHC(fe_model_asdr_EU, type = "HC1"))

re_model_asdr_EU <- plm(OA_ASDR ~ EU_score, data = df_panel, model = "random")
summary(re_model_asdr_EU,vcov = vcovHC)
confint(re_model_asdr_EU, level = 0.95, vcov = vcovHC(re_model_asdr_EU, type = "HC1"))

hausman_test_asdr_EU <- phtest(fe_model_asdr_EU, re_model_asdr_EU)

#社会服务综合指标
fe_model_aspr_SSU <- plm(OA_ASPR ~ SSU_score, data = df_panel, model = "within")
summary(fe_model_aspr_SSU,vcov = vcovHC)
confint(fe_model_aspr_SSU, level = 0.95, vcov = vcovHC(fe_model_aspr_SSU, type = "HC1"))

re_model_aspr_SSU <- plm(OA_ASPR ~ SSU_score, data = df_panel, model = "random")
summary(re_model_aspr_SSU,vcov = vcovHC)
confint(re_model_aspr_SSU, level = 0.95, vcov = vcovHC(re_model_aspr_SSU, type = "HC1"))

hausman_test_aspr_SSU <- phtest(fe_model_aspr_SSU, re_model_aspr_SSU)

fe_model_asdr_SSU <- plm(OA_ASDR ~ SSU_score, data = df_panel, model = "within")
summary(fe_model_asdr_SSU,vcov = vcovHC)
confint(fe_model_asdr_SSU, level = 0.95, vcov = vcovHC(fe_model_asdr_SSU, type = "HC1"))

re_model_asdr_SSU <- plm(OA_ASDR ~ SSU_score, data = df_panel, model = "random")
summary(re_model_asdr_SSU,vcov = vcovHC)
confint(re_model_asdr_SSU, level = 0.95, vcov = vcovHC(re_model_asdr_SSU, type = "HC1"))

hausman_test_asdr_SSU <- phtest(fe_model_asdr_SSU, re_model_asdr_SSU)

#生态综合指标
fe_model_aspr_EEU <- plm(OA_ASPR ~ EEU_score, data = df_panel, model = "within")
summary(fe_model_aspr_EEU,vcov = vcovHC)
confint(fe_model_aspr_EEU, level = 0.95, vcov = vcovHC(fe_model_aspr_EEU, type = "HC1"))

re_model_aspr_EEU <- plm(OA_ASPR ~ EEU_score, data = df_panel, model = "random")
summary(re_model_aspr_EEU,vcov = vcovHC)
confint(re_model_aspr_EEU, level = 0.95, vcov = vcovHC(re_model_aspr_EEU, type = "HC1"))

hausman_test_aspr_EEU <- phtest(fe_model_aspr_EEU, re_model_aspr_EEU)

fe_model_asdr_EEU <- plm(OA_ASDR ~ EEU_score, data = df_panel, model = "within")
summary(fe_model_asdr_EEU,vcov = vcovHC)
confint(fe_model_asdr_EEU, level = 0.95, vcov = vcovHC(fe_model_asdr_EEU, type = "HC1"))

re_model_asdr_EEU <- plm(OA_ASDR ~ EEU_score, data = df_panel, model = "random")
summary(re_model_asdr_EEU,vcov = vcovHC)
confint(re_model_asdr_EEU, level = 0.95, vcov = vcovHC(re_model_asdr_EEU, type = "HC1"))

hausman_test_asdr_EEU <- phtest(fe_model_asdr_EEU, re_model_asdr_EEU)



#熵值法（待确认）
# 计算每个指标的比例 Yij
Y <- df_panel[, -c(1, 2)] / rowSums(df_panel[, -c(1, 2)])

# 计算信息熵 ej
n <- ncol(df_panel) - 2  # 指标数量
ej <- numeric(n)

for (j in 1:n) {
  Yij <- Y[, j]
  ej[j] <- -sum(Yij * log(Yij)) / log(nrow(df_panel))
}

# 计算熵冗余 dj
dj <- 1 - ej

# 计算权重 wj
wj <- dj / sum(dj)

# 计算单个指标的评价 Sij
Sij <- sweep(df_panel[, -c(1, 2)], 2, wj, "*")

# 计算综合水平 Si
Si <- rowSums(Sij)

# 将综合水平添加到数据框中
df_panel$ComprehensiveLevel <- Si

# 输出权重
cat("指标权重:\n")
print(wj)

# 输出综合水平
cat("综合水平:\n")
print(df_panel$ComprehensiveLevel)

#在面板数据中加入SDI
load('面板数据.RData')
order <- readxl::read_xlsx('2.orders.xlsx', sheet = 3)
order <- order %>%
  mutate(Country = gsub("Côte d'Ivoire", "Cote d'Ivoire", Country))
order <- order %>%
  mutate(Country = gsub("Turkey", "Turkiye", Country))
pdata <- pdata %>%
  select(-SDI)
pdata <- pdata %>%
  left_join(select(order, Country, SDI), by = "Country")
pdata <- pdata %>%
  select(SDI, everything())
pdata1 <- pdata.frame(pdata, index = c("Country", "Year"))

df_panel <- pdata1
#人口
df_panel$DU_score <- df_panel$urban * 0.1234 + 
  df_panel$employ * 0.1418 + 
  df_panel$largecity * 0.0815
df_panel$DU_score <- df_panel$DU_score / 0.3467

# Economic urbanization (EU)
df_panel$EU_score <- df_panel$GDP * 0.0313 +
  df_panel$GNI * 0.0326 +
  df_panel$NPISH * 0.0348
df_panel$EU_score <- df_panel$EU_score / 0.0987

# Social Services Urbanization (SSU)
df_panel$SSU_score <- df_panel$elect * 0.1631 +
  df_panel$internet * 0.0578
df_panel$SSU_score <- df_panel$SSU_score / 0.2209

# Eco-environment urbanization (EEU)
df_panel$EEU_score <- df_panel$co2 * 0.1687 +
  df_panel$PM2.5 * 0.1648
df_panel$EEU_score <- df_panel$EEU_score / 0.3335

#针对SDI分组进行面板回归
# 根据 `region` 进行分层
sum(is.na(df_panel$SDI))
SDI <- unique(df_panel$SDI)
results <- list()
confint_results <- list()

# 对每个子组进行面板回归分析
for (r in SDI) {
  # 筛选出当前子组的数据
  subset_data <- subset(df_panel, SDI == r)
  
  # 进行面板回归分析
  model <- plm(OA_ASDR ~ DU_score + EU_score + SSU_score + EEU_score, data = subset_data, model = "within")
  
  # 将回归结果存储在列表中
  results[[r]] <- model

  # 获取系数的95%置信区间
  confint_results[[r]] <- confint(model)
}

# 查看每个子组的回归结果和置信区间
for (r in SDI) {
  cat("SDI:", r, "\n")
  cat("Regression Summary:\n")
  print(summary(results[[r]]))
  cat("Confidence Intervals:\n")
  print(confint_results[[r]])
  cat("\n")
}


save.image(file='SDI面板回归.RData')
load('SDI面板回归.RData')
