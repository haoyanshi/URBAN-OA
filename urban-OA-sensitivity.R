# 安装并加载必要的包
library(dplyr)
library(plm)

# 导入数据
load('面板数据.RData')

# 1. 简单随机抽样以国家为单位
set.seed(123)  # 设置随机种子
# 先获取不重复的国家列表
countries <- unique(pdata$Country)
random_country_sample_size <- 100  # 想要抽取的国家数量，可以根据需要调整

# 随机选择国家
random_countries <- sample(countries, random_country_sample_size)

# 从数据集中抽取选中的国家的所有记录
random_sample_data <- pdata %>%
  filter(Country %in% random_countries)


#城市人口，简单随机
random_sample_data <- pdata.frame(random_sample_data, index = c("Country", "Year"))

RA_fe_model_aspr_urban <- plm(OA_ASPR ~ urban, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_urban,vcov = vcovHC)
confint(RA_fe_model_aspr_urban, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_urban, type = "HC1"))

RA_re_model_aspr_urban <- plm(OA_ASPR ~ urban, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_urban,vcov = vcovHC)
confint(re_model_aspr_EEU, level = 0.95, vcov = vcovHC(re_model_aspr_EEU, type = "HC1"))

hausman_test_aspr_urban <- phtest(RA_fe_model_aspr_urban, RA_re_model_aspr_urban)

RA_fe_model_asdr_urban <- plm(OA_ASDR ~ urban, data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_urban,vcov = vcovHC)
confint(RA_fe_model_asdr_urban, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_urban, type = "HC1"))

RA_re_model_asdr_urban <- plm(OA_ASDR ~ urban, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_urban,vcov = vcovHC)
confint(RA_re_model_asdr_urban, level = 0.95, vcov = vcovHC(re_model_asdr_urban, type = "HC1"))

hausman_test_asdr_urban <- phtest(RA_fe_model_asdr_urban, RA_re_model_asdr_urban)

#就业，简单随机
RA_fe_model_aspr_employ <- plm(OA_ASPR ~ employ, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_employ,vcov = vcovHC)
confint(RA_fe_model_aspr_employ, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_employ, type = "HC1"))

RA_re_model_aspr_employ <- plm(OA_ASPR ~ employ, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_employ,vcov = vcovHC)
confint(RA_re_model_aspr_employ, level = 0.95, vcov = vcovHC(RA_re_model_aspr_employ, type = "HC1"))

hausman_test_aspr_employ <- phtest(RA_fe_model_aspr_employ, RA_re_model_aspr_employ)

RA_fe_model_asdr_employ <- plm(OA_ASDR ~ employ , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_employ,vcov = vcovHC)
confint(RA_fe_model_asdr_employ, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_employ, type = "HC1"))

RA_re_model_asdr_employ <- plm(OA_ASDR ~ employ, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_employ,vcov = vcovHC)
confint(RA_re_model_asdr_employ, level = 0.95, vcov = vcovHC(re_model_asdr_employ, type = "HC1"))

hausman_test_asdr_employ <- phtest(RA_fe_model_asdr_employ, RA_re_model_asdr_employ)

#最大城市，简单随机
RA_fe_model_aspr_largecity <- plm(OA_ASPR ~ largecity, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_largecity,vcov = vcovHC)
confint(RA_fe_model_aspr_largecity, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_largecity, type = "HC1"))

RA_re_model_aspr_largecity <- plm(OA_ASPR ~ largecity, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_largecity,vcov = vcovHC)
confint(RA_re_model_aspr_largecity, level = 0.95, vcov = vcovHC(RA_re_model_aspr_largecity, type = "HC1"))

hausman_test_aspr_largecity <- phtest(RA_fe_model_aspr_largecity, RA_re_model_aspr_largecity)

RA_fe_model_asdr_largecity <- plm(OA_ASDR ~ largecity , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_largecity,vcov = vcovHC)
confint(RA_fe_model_asdr_largecity, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_largecity, type = "HC1"))

RA_re_model_asdr_largecity <- plm(OA_ASDR ~ largecity, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_largecity,vcov = vcovHC)
confint(RA_re_model_asdr_largecity, level = 0.95, vcov = vcovHC(re_model_asdr_largecity, type = "HC1"))

hausman_test_asdr_largecity <- phtest(RA_fe_model_asdr_largecity, RA_re_model_asdr_largecity)

#GDP，简单随机
RA_fe_model_aspr_GDP <- plm(OA_ASPR ~ GDP, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_GDP,vcov = vcovHC)
confint(RA_fe_model_aspr_GDP, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_GDP, type = "HC1"))

RA_re_model_aspr_GDP <- plm(OA_ASPR ~ GDP, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_GDP,vcov = vcovHC)
confint(RA_re_model_aspr_GDP, level = 0.95, vcov = vcovHC(RA_re_model_aspr_GDP, type = "HC1"))

hausman_test_aspr_GDP <- phtest(RA_fe_model_aspr_GDP, RA_re_model_aspr_GDP)

RA_fe_model_asdr_GDP <- plm(OA_ASDR ~ GDP , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_GDP,vcov = vcovHC)
confint(RA_fe_model_asdr_GDP, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_GDP, type = "HC1"))

RA_re_model_asdr_GDP <- plm(OA_ASDR ~ GDP, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_GDP,vcov = vcovHC)
confint(RA_re_model_asdr_GDP, level = 0.95, vcov = vcovHC(re_model_asdr_GDP, type = "HC1"))

hausman_test_asdr_GDP <- phtest(RA_fe_model_asdr_GDP, RA_re_model_asdr_GDP)

#GNI，简单随机
RA_fe_model_aspr_GNI <- plm(OA_ASPR ~ GNI, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_GNI,vcov = vcovHC)
confint(RA_fe_model_aspr_GNI, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_GNI, type = "HC1"))

RA_re_model_aspr_GNI <- plm(OA_ASPR ~ GNI, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_GNI,vcov = vcovHC)
confint(RA_re_model_aspr_GNI, level = 0.95, vcov = vcovHC(RA_re_model_aspr_GNI, type = "HC1"))

hausman_test_aspr_GNI <- phtest(RA_fe_model_aspr_GNI, RA_re_model_aspr_GNI)

RA_fe_model_asdr_GNI <- plm(OA_ASDR ~ GNI , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_GNI,vcov = vcovHC)
confint(RA_fe_model_asdr_GNI, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_GNI, type = "HC1"))

RA_re_model_asdr_GNI <- plm(OA_ASDR ~ GNI, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_GNI,vcov = vcovHC)
confint(RA_re_model_asdr_GNI, level = 0.95, vcov = vcovHC(re_model_asdr_GNI, type = "HC1"))

hausman_test_asdr_GNI <- phtest(RA_fe_model_asdr_GNI, RA_re_model_asdr_GNI)

#NPISH，简单随机
RA_fe_model_aspr_NPISH <- plm(OA_ASPR ~ NPISH, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_NPISH,vcov = vcovHC)
confint(RA_fe_model_aspr_NPISH, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_NPISH, type = "HC1"))

RA_re_model_aspr_NPISH <- plm(OA_ASPR ~ NPISH, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_NPISH,vcov = vcovHC)
confint(RA_re_model_aspr_NPISH, level = 0.95, vcov = vcovHC(RA_re_model_aspr_NPISH, type = "HC1"))

hausman_test_aspr_NPISH <- phtest(RA_fe_model_aspr_NPISH, RA_re_model_aspr_NPISH)

RA_fe_model_asdr_NPISH <- plm(OA_ASDR ~ NPISH , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_NPISH,vcov = vcovHC)
confint(RA_fe_model_asdr_NPISH, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_NPISH, type = "HC1"))

RA_re_model_asdr_NPISH <- plm(OA_ASDR ~ NPISH, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_NPISH,vcov = vcovHC)
confint(RA_re_model_asdr_NPISH, level = 0.95, vcov = vcovHC(re_model_asdr_NPISH, type = "HC1"))

hausman_test_asdr_NPISH <- phtest(RA_fe_model_asdr_NPISH, RA_re_model_asdr_NPISH)

#elect，简单随机
RA_fe_model_aspr_elect <- plm(OA_ASPR ~ elect, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_elect,vcov = vcovHC)
confint(RA_fe_model_aspr_elect, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_elect, type = "HC1"))

RA_re_model_aspr_elect <- plm(OA_ASPR ~ elect, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_elect,vcov = vcovHC)
confint(RA_re_model_aspr_elect, level = 0.95, vcov = vcovHC(RA_re_model_aspr_elect, type = "HC1"))

hausman_test_aspr_elect <- phtest(RA_fe_model_aspr_elect, RA_re_model_aspr_elect)

RA_fe_model_asdr_elect <- plm(OA_ASDR ~ elect , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_elect,vcov = vcovHC)
confint(RA_fe_model_asdr_elect, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_elect, type = "HC1"))

RA_re_model_asdr_elect <- plm(OA_ASDR ~ elect, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_elect,vcov = vcovHC)
confint(RA_re_model_asdr_elect, level = 0.95, vcov = vcovHC(re_model_asdr_elect, type = "HC1"))

hausman_test_asdr_elect <- phtest(RA_fe_model_asdr_elect, RA_re_model_asdr_elect)

#internet，简单随机
RA_fe_model_aspr_internet <- plm(OA_ASPR ~ internet, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_internet,vcov = vcovHC)
confint(RA_fe_model_aspr_internet, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_internet, type = "HC1"))

RA_re_model_aspr_internet <- plm(OA_ASPR ~ internet, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_internet,vcov = vcovHC)
confint(RA_re_model_aspr_internet, level = 0.95, vcov = vcovHC(RA_re_model_aspr_internet, type = "HC1"))

hausman_test_aspr_internet <- phtest(RA_fe_model_aspr_internet, RA_re_model_aspr_internet)

RA_fe_model_asdr_internet <- plm(OA_ASDR ~ internet , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_internet,vcov = vcovHC)
confint(RA_fe_model_asdr_internet, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_internet, type = "HC1"))

RA_re_model_asdr_internet <- plm(OA_ASDR ~ internet, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_internet,vcov = vcovHC)
confint(RA_re_model_asdr_internet, level = 0.95, vcov = vcovHC(re_model_asdr_internet, type = "HC1"))

hausman_test_asdr_internet <- phtest(RA_fe_model_asdr_internet, RA_re_model_asdr_internet)

#co2，简单随机
RA_fe_model_aspr_co2 <- plm(OA_ASPR ~ co2, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_co2,vcov = vcovHC)
confint(RA_fe_model_aspr_co2, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_co2, type = "HC1"))

RA_re_model_aspr_co2 <- plm(OA_ASPR ~ co2, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_co2,vcov = vcovHC)
confint(RA_re_model_aspr_co2, level = 0.95, vcov = vcovHC(RA_re_model_aspr_co2, type = "HC1"))

hausman_test_aspr_co2 <- phtest(RA_fe_model_aspr_co2, RA_re_model_aspr_co2)

RA_fe_model_asdr_co2 <- plm(OA_ASDR ~ co2 , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_co2,vcov = vcovHC)
confint(RA_fe_model_asdr_co2, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_co2, type = "HC1"))

RA_re_model_asdr_co2 <- plm(OA_ASDR ~ co2, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_co2,vcov = vcovHC)
confint(RA_re_model_asdr_co2, level = 0.95, vcov = vcovHC(re_model_asdr_co2, type = "HC1"))

hausman_test_asdr_co2 <- phtest(RA_fe_model_asdr_co2, RA_re_model_asdr_co2)

#PM2.5，简单随机
RA_fe_model_aspr_PM2.5 <- plm(OA_ASPR ~ PM2.5, data = random_sample_data, model = "within")
summary(RA_fe_model_aspr_PM2.5,vcov = vcovHC)
confint(RA_fe_model_aspr_PM2.5, level = 0.95, vcov = vcovHC(RA_fe_model_aspr_PM2.5, type = "HC1"))

RA_re_model_aspr_PM2.5 <- plm(OA_ASPR ~ PM2.5, data = random_sample_data, model = "random")
summary(RA_re_model_aspr_PM2.5,vcov = vcovHC)
confint(RA_re_model_aspr_PM2.5, level = 0.95, vcov = vcovHC(RA_re_model_aspr_PM2.5, type = "HC1"))

hausman_test_aspr_PM2.5 <- phtest(RA_fe_model_aspr_PM2.5, RA_re_model_aspr_PM2.5)

RA_fe_model_asdr_PM2.5 <- plm(OA_ASDR ~ PM2.5 , data = random_sample_data, model = "within")
summary(RA_fe_model_asdr_PM2.5,vcov = vcovHC)
confint(RA_fe_model_asdr_PM2.5, level = 0.95, vcov = vcovHC(RA_fe_model_asdr_PM2.5, type = "HC1"))

RA_re_model_asdr_PM2.5 <- plm(OA_ASDR ~ PM2.5, data = random_sample_data, model = "random")
summary(RA_re_model_asdr_PM2.5,vcov = vcovHC)
confint(RA_re_model_asdr_PM2.5, level = 0.95, vcov = vcovHC(re_model_asdr_PM2.5, type = "HC1"))

hausman_test_asdr_PM2.5 <- phtest(RA_fe_model_asdr_PM2.5, RA_re_model_asdr_PM2.5)

#先构建综合指标变量
df_panel <- random_sample_data

#DU
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

#在面板数据中加入SDI
order <- readxl::read_xlsx('2.orders.xlsx', sheet = 3)
order <- order %>%
  mutate(Country = gsub("Côte d'Ivoire", "Cote d'Ivoire", Country))
order <- order %>%
  mutate(Country = gsub("Turkey", "Turkiye", Country))

pdata <- df_panel %>%
  left_join(select(order, Country, SDI), by = "Country")
pdata <- pdata %>%
  select(SDI, everything())
pdata1 <- pdata.frame(pdata, index = c("Country", "Year"))

df_panel <- pdata1

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
  model <- plm(OA_ASPR ~ DU_score + EU_score + SSU_score + EEU_score, data = subset_data, model = "within")
  
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

save.image(file = '敏感性分析.RData')
load('敏感性分析.RData')

# 2. 根据SDI水平进行分层抽样（按国家）
# 先把每个国家分配到相应的SDI组
save(pdata_SDI,file = '面板数据_SDI.RData')
load('面板数据_SDI.RData')

pdata <- pdata %>%
  left_join(select(order, Country, SDI), by = "Country")
pdata <- pdata %>%
  select(SDI, everything())
pdata <- pdata.frame(pdata, index = c("Country", "Year"))
# 以每个国家的第一个记录的SDI值为基准进行分层
country_SDI <- pdata %>%
  group_by(Country) %>%
  summarize(SDI = first(SDI))

# 按SDI进行分层抽样
set.seed(123)  # 确保随机性一致
stratified_countries <- country_SDI %>%
  group_by(SDI) %>%
  sample_n(size = 20) # 每个SDI层选取2个国家，调整此参数适应你的需求

# 获取分层抽样后选定国家的全部数据
stratified_sample_data <- pdata %>%
  filter(Country %in% stratified_countries$Country)

pdata <- stratified_sample_data
pdata <- pdata.frame(pdata, index = c("Country", "Year"))

#单变量面板回归
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
save.image(file = '敏感性分析2.RData')
load('敏感性分析2.RData')

#先构建综合指标变量
df_panel <- pdata

#DU
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
  model <- plm(OA_ASPR ~ DU_score + EU_score + SSU_score + EEU_score, data = subset_data, model = "within")
  
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
