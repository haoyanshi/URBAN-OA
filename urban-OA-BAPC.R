#加载必要的R包
library("data.table")
library("tidyverse")
library("caTools")
library("fanplot")
library("cmprsk")
library("etm")
library("BAPC")
library("INLA")
library("reshape")
library("reshape2")
library("tidyr")
library("epitools")
library("ggplot2")
library("ggfan")
library('vroom')

#导入数据文件
#1、疾病的Global数据
load('OA_data_dan.RData')
unique(OA_data_dan$measure_name)
unique(OA_data_dan$age_name)
#1.1 筛选需要的行和列(男P)
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")   ###20个年龄组
GBD_OA_Male_P<- subset(OA_data_dan,
                     (OA_data_dan$age_name %in% age1 ) &
                       OA_data_dan$sex_name=="Male"&
                       OA_data_dan$location_name=='Global'&
                       OA_data_dan$metric_name== 'Number' &
                       OA_data_dan$measure_name=="Prevalence")
GBD_OA_Male_P$age_name<-gsub(" years","", GBD_OA_Male_P$age_name)
unique(GBD_OA_Male_P$age_name)
GBD_OA_Male_P <- GBD_OA_Male_P[,c("location_name","year",
                                  "sex_name","age_name",
                                  "measure_name","metric_name","val","upper","lower")]
#排序
GBD_OA_Male_P$age_name <- factor(GBD_OA_Male_P$age_name, 
                               levels = c("<5", "5-9", "10-14", "15-19",
                                          "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                          "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                          "90-94", "95+"))
GBD_OA_Male_P_wide <- reshape2::dcast(data=GBD_OA_Male_P, year ~ age_name, value.var="val")
rownames(GBD_OA_Male_P_wide) <- GBD_OA_Male_P_wide$year
GBD_OA_Male_P_wide <- GBD_OA_Male_P_wide[,-1]
#1.2 筛选需要的行和列(女P)
GBD_OA_Female_P<- subset(OA_data_dan,
                       (OA_data_dan$age_name %in% age1 ) &
                         OA_data_dan$sex_name=="Female"&
                         OA_data_dan$location_name=='Global'&
                         OA_data_dan$metric_name== 'Number' &
                         OA_data_dan$measure_name=="Prevalence")
GBD_OA_Female_P$age_name<-gsub(" years","", GBD_OA_Female_P$age_name)
unique(GBD_OA_Female_P$age_name)
GBD_OA_Female_P <- GBD_OA_Female_P[,c("location_name","year",
                                  "sex_name","age_name",
                                  "measure_name","metric_name","val","upper","lower")]
#排序
GBD_OA_Female_P$age_name <- factor(GBD_OA_Female_P$age_name, 
                                 levels = c("<5", "5-9", "10-14", "15-19",
                                            "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                            "90-94", "95+"))
GBD_OA_Female_P_wide <- reshape2::dcast(data=GBD_OA_Female_P, year ~ age_name, value.var="val")
rownames(GBD_OA_Female_P_wide) <- GBD_OA_Female_P_wide$year
GBD_OA_Female_P_wide <- GBD_OA_Female_P_wide[,-1]
#1.3 筛选需要的行和列(全P)
GBD_OA_Both_P<- subset(OA_data_dan,
                         (OA_data_dan$age_name %in% age1 ) &
                           OA_data_dan$sex_name=="Both"&
                           OA_data_dan$location_name=='Global'&
                           OA_data_dan$metric_name== 'Number' &
                           OA_data_dan$measure_name=="Prevalence")
GBD_OA_Both_P$age_name<-gsub(" years","", GBD_OA_Both_P$age_name)
unique(GBD_OA_Both_P$age_name)
GBD_OA_Both_P <- GBD_OA_Both_P[,c("location_name","year",
                                      "sex_name","age_name",
                                      "measure_name","metric_name","val","upper","lower")]
#排序
GBD_OA_Both_P$age_name <- factor(GBD_OA_Both_P$age_name, 
                                   levels = c("<5", "5-9", "10-14", "15-19",
                                              "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                              "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                              "90-94", "95+"))
GBD_OA_Both_P_wide <- reshape2::dcast(data=GBD_OA_Both_P, year ~ age_name, value.var="val")
rownames(GBD_OA_Both_P_wide) <- GBD_OA_Both_P_wide$year
GBD_OA_Both_P_wide <- GBD_OA_Both_P_wide[,-1]
#1.4 筛选需要的行和列(男D)
unique(OA_data_dan$measure_name)
GBD_OA_Male_D<- subset(OA_data_dan,
                       (OA_data_dan$age_name %in% age1 ) &
                         OA_data_dan$sex_name=="Male"&
                         OA_data_dan$location_name=='Global'&
                         OA_data_dan$metric_name== 'Number' &
                         OA_data_dan$measure_name=="DALYs (Disability-Adjusted Life Years)")
GBD_OA_Male_D$age_name<-gsub(" years","", GBD_OA_Male_D$age_name)
unique(GBD_OA_Male_D$age_name)
GBD_OA_Male_D <- GBD_OA_Male_D[,c("location_name","year",
                                  "sex_name","age_name",
                                  "measure_name","metric_name","val","upper","lower")]
#排序
GBD_OA_Male_D$age_name <- factor(GBD_OA_Male_D$age_name, 
                                 levels = c("<5", "5-9", "10-14", "15-19",
                                            "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                            "90-94", "95+"))
GBD_OA_Male_D_wide <- reshape2::dcast(data=GBD_OA_Male_D, year ~ age_name, value.var="val")
rownames(GBD_OA_Male_D_wide) <- GBD_OA_Male_D_wide$year
GBD_OA_Male_D_wide <- GBD_OA_Male_D_wide[,-1]
#1.5 筛选需要的行和列(女D)
GBD_OA_Female_D<- subset(OA_data_dan,
                       (OA_data_dan$age_name %in% age1 ) &
                         OA_data_dan$sex_name=="Female"&
                         OA_data_dan$location_name=='Global'&
                         OA_data_dan$metric_name== 'Number' &
                         OA_data_dan$measure_name=="DALYs (Disability-Adjusted Life Years)")
GBD_OA_Female_D$age_name<-gsub(" years","", GBD_OA_Female_D$age_name)
unique(GBD_OA_Female_D$age_name)
GBD_OA_Female_D <- GBD_OA_Female_D[,c("location_name","year",
                                  "sex_name","age_name",
                                  "measure_name","metric_name","val","upper","lower")]
#排序
GBD_OA_Female_D$age_name <- factor(GBD_OA_Female_D$age_name, 
                                 levels = c("<5", "5-9", "10-14", "15-19",
                                            "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                            "90-94", "95+"))
GBD_OA_Female_D_wide <- reshape2::dcast(data=GBD_OA_Female_D, year ~ age_name, value.var="val")
rownames(GBD_OA_Female_D_wide) <- GBD_OA_Female_D_wide$year
GBD_OA_Female_D_wide <- GBD_OA_Female_D_wide[,-1]
#1.6 筛选需要的行和列(全D)
GBD_OA_Both_D<- subset(OA_data_dan,
                         (OA_data_dan$age_name %in% age1 ) &
                           OA_data_dan$sex_name=="Both"&
                           OA_data_dan$location_name=='Global'&
                           OA_data_dan$metric_name== 'Number' &
                           OA_data_dan$measure_name=="DALYs (Disability-Adjusted Life Years)")
GBD_OA_Both_D$age_name<-gsub(" years","", GBD_OA_Both_D$age_name)
unique(GBD_OA_Both_D$age_name)
GBD_OA_Both_D <- GBD_OA_Both_D[,c("location_name","year",
                                      "sex_name","age_name",
                                      "measure_name","metric_name","val","upper","lower")]
#排序
GBD_OA_Both_D$age_name <- factor(GBD_OA_Both_D$age_name, 
                                   levels = c("<5", "5-9", "10-14", "15-19",
                                              "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                              "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                              "90-94", "95+"))
GBD_OA_Both_D_wide <- reshape2::dcast(data=GBD_OA_Both_D, year ~ age_name, value.var="val")
rownames(GBD_OA_Both_D_wide) <- GBD_OA_Both_D_wide$year
GBD_OA_Both_D_wide <- GBD_OA_Both_D_wide[,-1]
#1.7 检查数据集格式
class(GBD_OA_Male_P_wide)
class(GBD_OA_Female_P_wide)
class(GBD_OA_Both_P_wide)
class(GBD_OA_Male_D_wide)
class(GBD_OA_Female_D_wide)
class(GBD_OA_Both_D_wide)
#1.8 将Number数据取整
GBD_OA_Male_P_wide <- as.data.frame(apply(GBD_OA_Male_P_wide, c(1,2), round))
GBD_OA_Female_P_wide <- as.data.frame(apply(GBD_OA_Female_P_wide, c(1,2), round))
GBD_OA_Both_P_wide <- as.data.frame(apply(GBD_OA_Both_P_wide, c(1,2), round))
GBD_OA_Male_D_wide <- as.data.frame(apply(GBD_OA_Male_D_wide, c(1,2), round))
GBD_OA_Female_D_wide <- as.data.frame(apply(GBD_OA_Female_D_wide, c(1,2), round))
GBD_OA_Both_D_wide <- as.data.frame(apply(GBD_OA_Both_D_wide, c(1,2), round))
#1.9 补充没有发病人数数据的年份
#创建2022-2050的空数据框
GBD_OA_2035NA <- matrix(data=NA, nrow=2035-2021, ncol=ncol(GBD_OA_Male_P_wide)) %>% as.data.frame()
rownames(GBD_OA_2035NA) <- seq(2022, 2035, 1)
colnames(GBD_OA_2035NA) <- names(GBD_OA_Male_P_wide)
#将创建好的空数据框与上述数据进行合并
GBD_OA_Male_P_pro <- rbind(GBD_OA_Male_P_wide, GBD_OA_2035NA)
GBD_OA_Female_P_pro <- rbind(GBD_OA_Female_P_wide, GBD_OA_2035NA)
GBD_OA_Both_P_pro <- rbind(GBD_OA_Both_P_wide, GBD_OA_2035NA)
GBD_OA_Male_D_pro <- rbind(GBD_OA_Male_D_wide, GBD_OA_2035NA)
GBD_OA_Female_D_pro <- rbind(GBD_OA_Female_D_wide, GBD_OA_2035NA)
GBD_OA_Both_D_pro <- rbind(GBD_OA_Both_D_wide, GBD_OA_2035NA)
#保存整理好的数据
save(GBD_OA_Male_P_pro,GBD_OA_Female_P_pro,GBD_OA_Both_P_pro,
     GBD_OA_Male_D_pro,GBD_OA_Female_D_pro,GBD_OA_Both_D_pro,
     file = '疾病数据各性别P和D的1990-2035.RData')
load('疾病数据各性别P和D的1990-2035.RData')


#2、当前人口数据
path = "GBD_population/"
fileName = dir(path)
fileName
GBD_population_1990_2021 <- data.frame()
for(k in 1:length(fileName)){
  data = vroom(file = paste(path,fileName[k],sep = "\\"))
  GBD_population_1990_2021=rbind(GBD_population_1990_2021,data)
}
unique(GBD_population_1990_2021$age_name)
#2.1 筛选需要的数据
age2 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years") 
var_name <- c("location_name", "sex_name", "year", "age_name", "val") 
GBD_population_1990_2021<-GBD_population_1990_2021%>% dplyr::select(var_name) %>% 
  filter(location_name %in% 'Global' & age_name %in% age2 )
#2.2 整理现有人口数据
GBD_population_1990_2021$age_name<-gsub(" years","",GBD_population_1990_2021$age_name)

#3、预测人口数据
prediction_var_name <- c("location_name", "sex", "year_id", "age_group_name", "val")
GBD_population_prediction <- fread("IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv") %>%
  dplyr::select(prediction_var_name) %>%
  dplyr::filter(location_name %in% 'Global' & year_id %in% 2022:2035)
unique(GBD_population_prediction$age_group_name)
#3.1 GBD_population_prediction中目前无Both数据，需要自行补充
Both <- GBD_population_prediction %>% 
  group_by(location_name, year_id, age_group_name) %>%
  summarise(Both = sum(val)) %>%
  mutate('sex' = 'Both') %>%
  dplyr::rename('val' = 'Both') %>%
  select(colnames(GBD_population_prediction))
GBD_population_prediction <- rbind(GBD_population_prediction, Both)
#3.2 整理年龄结构
GBD_5year <- GBD_population_prediction %>% 
  filter(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4")) %>%
  group_by(location_name,sex,year_id) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_group_name="<5")
GBD_population_prediction <- GBD_population_prediction %>% 
  filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","All Ages","1 to 4"))) %>%
  rbind(GBD_5year)
#3.3 整理列名以和现有人口数据保持一致
names(GBD_population_prediction)[names(GBD_population_prediction) == 'age_group_name'] <- 'age_name'
GBD_population_prediction$age_name<-gsub(" to ","-",GBD_population_prediction$age_name)
GBD_population_prediction$age_name<-gsub(" plus","+",GBD_population_prediction$age_name)
unique(GBD_population_prediction$age_name)
colnames(GBD_population_prediction)<-var_name
#3.4 合并所有人口学数据并按照年龄大小排序
GBD_population <- rbind(GBD_population_1990_2021, GBD_population_prediction)
GBD_population$age_name<-factor(GBD_population$age_name, levels = c("<5", "5-9", "10-14", "15-19",
                                              "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                              "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                              "90-94", "95+"))

#检查数据结构
unique(GBD_population$location_name)
unique(GBD_population$sex_name)
unique(GBD_population$year)
unique(GBD_population$age_name)
#3.5 数据分割（按照性别）
names(GBD_population)[names(GBD_population) == "sex_name"] <- "sex"
GBD_population_Male <- subset(GBD_population, sex == "Male")
GBD_population_Female <- subset(GBD_population, sex == "Female")
GBD_population_Both <- subset(GBD_population, sex == "Both")
#3.6 将3个数据框变为宽格式
#男
GBD_population_Male$age_name<-factor(GBD_population_Male$age_name, 
                                levels = c("<5", "5-9", "10-14", "15-19",
                                           "20-24", "25-29", "30-34", 
                                           "35-39", "40-44", "45-49", "50-54", 
                                          "55-59", "60-64", "65-69", "70-74", 
                                          "75-79", "80-84", "85-89", 
                                           "90-94", "95+"))
GBD_population_Male_wide <- dcast(data=GBD_population_Male, year~age_name, 
                          value.var=c("val")) %>% as.data.frame()
#女
GBD_population_Female$age_name<-factor(GBD_population_Female$age_name, 
                                     levels = c("<5", "5-9", "10-14", "15-19",
                                                "20-24", "25-29", "30-34", 
                                                "35-39", "40-44", "45-49", "50-54", 
                                                "55-59", "60-64", "65-69", "70-74", 
                                                "75-79", "80-84", "85-89", 
                                                "90-94", "95+"))
GBD_population_Female_wide <- dcast(data=GBD_population_Female, year~age_name, 
                                  value.var=c("val")) %>% as.data.frame()
#全性别
GBD_population_Both$age_name<-factor(GBD_population_Both$age_name, 
                                       levels = c("<5", "5-9", "10-14", "15-19",
                                                  "20-24", "25-29", "30-34", 
                                                  "35-39", "40-44", "45-49", "50-54", 
                                                  "55-59", "60-64", "65-69", "70-74", 
                                                  "75-79", "80-84", "85-89", 
                                                  "90-94", "95+"))
GBD_population_Both_wide <- dcast(data=GBD_population_Both, year~age_name, 
                                    value.var=c("val")) %>% as.data.frame()
#3.7 将年份改为行名
rownames(GBD_population_Male_wide) <- GBD_population_Male_wide$year
GBD_population_Male_wide <- GBD_population_Male_wide[,-1]
rownames(GBD_population_Female_wide) <- GBD_population_Female_wide$year
GBD_population_Female_wide <- GBD_population_Female_wide[,-1]
rownames(GBD_population_Both_wide) <- GBD_population_Both_wide$year
GBD_population_Both_wide <- GBD_population_Both_wide[,-1]
#3.8 数据取整
GBD_population_Male_wide <- as.data.frame(apply(GBD_population_Male_wide, c(1,2), round))
GBD_population_Female_wide <- as.data.frame(apply(GBD_population_Female_wide, c(1,2), round))
GBD_population_Both_wide <- as.data.frame(apply(GBD_population_Both_wide, c(1,2), round))
#保存整理好的人口数据
save(GBD_population_Male_wide, GBD_population_Female_wide, GBD_population_Both_wide,
     file = '1990-2035人口数据.RData')
load('1990-2035人口数据.RData')

#4、年龄占比结构数据
age_stand <- read.csv("age_stand.csv")
age_stand <- age_stand %>%
  filter(age %in% c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29","30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59","60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 plus"))

wstand <- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(), 
            age_stand$std_population[3:21] %>% as.numeric())/sum(age_stand$std_population[1:21]) 

#5、 BAPC模型拟合
#5.1 男性P
a_male_p <- GBD_OA_Male_P_pro
b_male_p <- GBD_population_Male_wide
esoph_male_p <- APCList(a_male_p, b_male_p, gf = 5)
BAPC_result_male_p <- BAPC(esoph_male_p, predict = list(npredict = 10, retro = T),
                    secondDiff = FALSE, stdweight = wstand, verbose = T) 

BAPC_list_male_p<- BAPC_result_male_p

#定义步长的百分位数
probs = c(0.002,seq(0.004, 0.996, by = 0.002), 0.998)
newcolnames <- paste(probs, "Q", sep = "")

#初始化一个空的数据框 data_plot_all，用于存储最终的绘图数据。
data_plot_all <- data.frame(matrix(data = NA, nrow = 0, ncol = 6))

#使用 qapc 函数计算 BAPC_list 中每个百分位数的值。
APCList <- qapc(BAPC_list_male_p, percentiles = probs)

#获取周期数（J）和周期标签（plab）
J <- nperiod(APCList)
plab <- periodlabels(APCList)

#创建一个标准化权重矩阵 my.wm，用于后续的标准化计算。
my.wm <- matrix(rep(stdweight(APCList), each = J),
                byrow = F, nrow = J)
#st是一个索引值，表示从1990年开始。1990 - 1989 等于 1，所以 st 的值为 1。意味着从第 1 年开始处理数据。
st <- 1990 - 1989
#计算每个年份的总人数
agg.n <- rowSums(pyrs(APCList))
#定义标准化的单位
scale <- 100000 #根据单位来修改，目前的单位是“/1000”
data_forplot <- t(agestd.proj(APCList)[st:J, newcolnames]/agg.n[st:J] * 
                    scale) %>%
  as.data.frame()%>%
  rownames_to_column('quantile') %>%
  pivot_longer(cols = colnames(.)[-1], names_to = 'year') %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(sub = 'Male')

median <- data_forplot[data_forplot$quantile == '0.5Q',c('year', 'value')] %>%
  dplyr::rename('median_data' = 'value')

y <- epi(APCList)
n <- pyrs(APCList)

data_forplot <- left_join(data_forplot, median, by = 'year')

data_forplot$median_data2 <- ifelse(data_forplot$year > 2021, NA, data_forplot$median_data)
data_plot_all_Male <- rbind(data_plot_all,data_forplot) #得到了一个关键的作图数据框：data_plot_all_Male
summary(data_plot_all_Male)

#5.2 女性P
a <- GBD_OA_Female_P_pro
b <- GBD_population_Female_wide
esoph <- APCList(a, b, gf = 5)
BAPC_result <- BAPC(esoph, predict = list(npredict = 10, retro = T),
                           secondDiff = FALSE, stdweight = wstand, verbose = T) 

BAPC_list<- BAPC_result

#定义步长的百分位数
probs = c(0.002,seq(0.004, 0.996, by = 0.002), 0.998)
newcolnames <- paste(probs, "Q", sep = "")

#初始化一个空的数据框 data_plot_all，用于存储最终的绘图数据。
data_plot_all <- data.frame(matrix(data = NA, nrow = 0, ncol = 6))

#使用 qapc 函数计算 BAPC_list 中每个百分位数的值。
APCList <- qapc(BAPC_list, percentiles = probs)

#获取周期数（J）和周期标签（plab）
J <- nperiod(APCList)
plab <- periodlabels(APCList)

#创建一个标准化权重矩阵 my.wm，用于后续的标准化计算。
my.wm <- matrix(rep(stdweight(APCList), each = J),
                byrow = F, nrow = J)
#st是一个索引值，表示从1990年开始。1990 - 1989 等于 1，所以 st 的值为 1。意味着从第 1 年开始处理数据。
st <- 1990 - 1989
#计算每个年份的总人数
agg.n <- rowSums(pyrs(APCList))
#定义标准化的单位
scale <- 100000 #根据单位来修改，目前的单位是“/1000”
data_forplot <- t(agestd.proj(APCList)[st:J, newcolnames]/agg.n[st:J] * 
                    scale) %>%
  as.data.frame()%>%
  rownames_to_column('quantile') %>%
  pivot_longer(cols = colnames(.)[-1], names_to = 'year') %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(sub = 'Female')

median <- data_forplot[data_forplot$quantile == '0.5Q',c('year', 'value')] %>%
  dplyr::rename('median_data' = 'value')

y <- epi(APCList)
n <- pyrs(APCList)

data_forplot <- left_join(data_forplot, median, by = 'year')

data_forplot$median_data2 <- ifelse(data_forplot$year > 2021, NA, data_forplot$median_data)
data_plot_all_Female <- rbind(data_plot_all,data_forplot) #得到了一个关键的作图数据框：data_plot_all_Female
summary(data_plot_all_Female)

#5.3 全性P
a <- GBD_OA_Both_P_pro
b <- GBD_population_Both_wide
esoph <- APCList(a, b, gf = 5)
BAPC_result <- BAPC(esoph, predict = list(npredict = 10, retro = T),
                    secondDiff = FALSE, stdweight = wstand, verbose = T) 

BAPC_list<- BAPC_result

#定义步长的百分位数
probs = c(0.002,seq(0.004, 0.996, by = 0.002), 0.998)
newcolnames <- paste(probs, "Q", sep = "")

#初始化一个空的数据框 data_plot_all，用于存储最终的绘图数据。
data_plot_all <- data.frame(matrix(data = NA, nrow = 0, ncol = 6))

#使用 qapc 函数计算 BAPC_list 中每个百分位数的值。
APCList <- qapc(BAPC_list, percentiles = probs)

#获取周期数（J）和周期标签（plab）
J <- nperiod(APCList)
plab <- periodlabels(APCList)

#创建一个标准化权重矩阵 my.wm，用于后续的标准化计算。
my.wm <- matrix(rep(stdweight(APCList), each = J),
                byrow = F, nrow = J)
#st是一个索引值，表示从1990年开始。1990 - 1989 等于 1，所以 st 的值为 1。意味着从第 1 年开始处理数据。
st <- 1990 - 1989
#计算每个年份的总人数
agg.n <- rowSums(pyrs(APCList))
#定义标准化的单位
scale <- 100000 #根据单位来修改，目前的单位是“/1000”
data_forplot <- t(agestd.proj(APCList)[st:J, newcolnames]/agg.n[st:J] * 
                    scale) %>%
  as.data.frame()%>%
  rownames_to_column('quantile') %>%
  pivot_longer(cols = colnames(.)[-1], names_to = 'year') %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(sub = 'Both')

median <- data_forplot[data_forplot$quantile == '0.5Q',c('year', 'value')] %>%
  dplyr::rename('median_data' = 'value')

y <- epi(APCList)
n <- pyrs(APCList)

data_forplot <- left_join(data_forplot, median, by = 'year')

data_forplot$median_data2 <- ifelse(data_forplot$year > 2021, NA, data_forplot$median_data)
data_plot_all_Both <- rbind(data_plot_all,data_forplot) #得到了一个关键的作图数据框：data_plot_all_Both
summary(data_plot_all_Both)

#6、 作图
data_plot_all <- rbind(data_plot_all_Both, data_plot_all_Female,data_plot_all_Male)

color_forpoint = c('Both' = '#1d2786',
                   'Female' = '#008891',
                   'Male' = '#c51350')
#'Low SDI' = '#e78ac3',
#'Low-middle SDI' = '#a6d854',
#'Middle SDI' = '#ffd92f')
shape_forpoint = c('Both' = 15,
                   'Female' = 16,
                   'Male' = 17)
#'Low SDI' = 18,
#'Low-middle SDI' = 25,
#'Middle SDI' = 8) #color_forpoint和shape_forpiont都是根据需要来修改

color_forfan = '#c8d9eb'
plot <- ggplot(data_plot_all, aes(x=year,y=value, group = sub))+
  geom_fan()+scale_fill_gradientn(colours = rep(color_forfan,2), guide = FALSE) +
  geom_point( aes(x = year, y = median_data2, color = sub,shape = sub))+
  geom_line(aes(x = year, y = median_data, color = sub))+
  scale_color_manual(values = color_forpoint)+
  scale_shape_manual(values = shape_forpoint)+
  scale_y_continuous(breaks = seq(4000, 12000, by = 1000), limits = c(4000, 12000))+
  geom_vline(xintercept = 2021,linetype = 2)+
  ylab('ASPR (per 100,000)') +
  xlab('Year')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.key = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank()) 
summary(data_plot_all)
ggsave(plot, file="Global_threesex_1990-2035_ASPR1.pdf",units = "cm", width = 10,height = 8)

#7、DALY
#7.1 男性D
a <- GBD_OA_Male_D_pro
b <- GBD_population_Male_wide
esoph <- APCList(a, b, gf = 5)
BAPC_result <- BAPC(esoph, predict = list(npredict = 10, retro = T),
                           secondDiff = FALSE, stdweight = wstand, verbose = T) 

BAPC_list<- BAPC_result

#定义步长的百分位数
probs = c(0.002,seq(0.004, 0.996, by = 0.002), 0.998)
newcolnames <- paste(probs, "Q", sep = "")

#初始化一个空的数据框 data_plot_all，用于存储最终的绘图数据。
data_plot_all <- data.frame(matrix(data = NA, nrow = 0, ncol = 6))

#使用 qapc 函数计算 BAPC_list 中每个百分位数的值。
APCList <- qapc(BAPC_list, percentiles = probs)

#获取周期数（J）和周期标签（plab）
J <- nperiod(APCList)
plab <- periodlabels(APCList)

#创建一个标准化权重矩阵 my.wm，用于后续的标准化计算。
my.wm <- matrix(rep(stdweight(APCList), each = J),
                byrow = F, nrow = J)
#st是一个索引值，表示从1990年开始。1990 - 1989 等于 1，所以 st 的值为 1。意味着从第 1 年开始处理数据。
st <- 1990 - 1989
#计算每个年份的总人数
agg.n <- rowSums(pyrs(APCList))
#定义标准化的单位
scale <- 100000 #根据单位来修改，目前的单位是“/1000”
data_forplot <- t(agestd.proj(APCList)[st:J, newcolnames]/agg.n[st:J] * 
                    scale) %>%
  as.data.frame()%>%
  rownames_to_column('quantile') %>%
  pivot_longer(cols = colnames(.)[-1], names_to = 'year') %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(sub = 'Male')

median <- data_forplot[data_forplot$quantile == '0.5Q',c('year', 'value')] %>%
  dplyr::rename('median_data' = 'value')

y <- epi(APCList)
n <- pyrs(APCList)

data_forplot <- left_join(data_forplot, median, by = 'year')

data_forplot$median_data2 <- ifelse(data_forplot$year > 2021, NA, data_forplot$median_data)
data_plot_all_Male <- rbind(data_plot_all,data_forplot) #得到了一个关键的作图数据框：data_plot_all_Male
summary(data_plot_all_Male)

#7.2 女性D
a <- GBD_OA_Female_D_pro
b <- GBD_population_Female_wide
esoph <- APCList(a, b, gf = 5)
BAPC_result <- BAPC(esoph, predict = list(npredict = 10, retro = T),
                    secondDiff = FALSE, stdweight = wstand, verbose = T) 

BAPC_list<- BAPC_result

#定义步长的百分位数
probs = c(0.002,seq(0.004, 0.996, by = 0.002), 0.998)
newcolnames <- paste(probs, "Q", sep = "")

#初始化一个空的数据框 data_plot_all，用于存储最终的绘图数据。
data_plot_all <- data.frame(matrix(data = NA, nrow = 0, ncol = 6))

#使用 qapc 函数计算 BAPC_list 中每个百分位数的值。
APCList <- qapc(BAPC_list, percentiles = probs)

#获取周期数（J）和周期标签（plab）
J <- nperiod(APCList)
plab <- periodlabels(APCList)

#创建一个标准化权重矩阵 my.wm，用于后续的标准化计算。
my.wm <- matrix(rep(stdweight(APCList), each = J),
                byrow = F, nrow = J)
#st是一个索引值，表示从1990年开始。1990 - 1989 等于 1，所以 st 的值为 1。意味着从第 1 年开始处理数据。
st <- 1990 - 1989
#计算每个年份的总人数
agg.n <- rowSums(pyrs(APCList))
#定义标准化的单位
scale <- 100000 #根据单位来修改，目前的单位是“/1000”
data_forplot <- t(agestd.proj(APCList)[st:J, newcolnames]/agg.n[st:J] * 
                    scale) %>%
  as.data.frame()%>%
  rownames_to_column('quantile') %>%
  pivot_longer(cols = colnames(.)[-1], names_to = 'year') %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(sub = 'Female')

median <- data_forplot[data_forplot$quantile == '0.5Q',c('year', 'value')] %>%
  dplyr::rename('median_data' = 'value')

y <- epi(APCList)
n <- pyrs(APCList)

data_forplot <- left_join(data_forplot, median, by = 'year')

data_forplot$median_data2 <- ifelse(data_forplot$year > 2021, NA, data_forplot$median_data)
data_plot_all_Female <- rbind(data_plot_all,data_forplot) #得到了一个关键的作图数据框：data_plot_all_Female
summary(data_plot_all_Female)

#7.3 全性P
a <- GBD_OA_Both_D_pro
b <- GBD_population_Both_wide
esoph <- APCList(a, b, gf = 5)
BAPC_result <- BAPC(esoph, predict = list(npredict = 10, retro = T),
                    secondDiff = FALSE, stdweight = wstand, verbose = T) 

BAPC_list<- BAPC_result

#定义步长的百分位数
probs = c(0.002,seq(0.004, 0.996, by = 0.002), 0.998)
newcolnames <- paste(probs, "Q", sep = "")

#初始化一个空的数据框 data_plot_all，用于存储最终的绘图数据。
data_plot_all <- data.frame(matrix(data = NA, nrow = 0, ncol = 6))

#使用 qapc 函数计算 BAPC_list 中每个百分位数的值。
APCList <- qapc(BAPC_list, percentiles = probs)

#获取周期数（J）和周期标签（plab）
J <- nperiod(APCList)
plab <- periodlabels(APCList)

#创建一个标准化权重矩阵 my.wm，用于后续的标准化计算。
my.wm <- matrix(rep(stdweight(APCList), each = J),
                byrow = F, nrow = J)
#st是一个索引值，表示从1990年开始。1990 - 1989 等于 1，所以 st 的值为 1。意味着从第 1 年开始处理数据。
st <- 1990 - 1989
#计算每个年份的总人数
agg.n <- rowSums(pyrs(APCList))
#定义标准化的单位
scale <- 100000 #根据单位来修改，目前的单位是“/1000”
data_forplot <- t(agestd.proj(APCList)[st:J, newcolnames]/agg.n[st:J] * 
                    scale) %>%
  as.data.frame()%>%
  rownames_to_column('quantile') %>%
  pivot_longer(cols = colnames(.)[-1], names_to = 'year') %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(sub = 'Both')

median <- data_forplot[data_forplot$quantile == '0.5Q',c('year', 'value')] %>%
  dplyr::rename('median_data' = 'value')

y <- epi(APCList)
n <- pyrs(APCList)

data_forplot <- left_join(data_forplot, median, by = 'year')

data_forplot$median_data2 <- ifelse(data_forplot$year > 2021, NA, data_forplot$median_data)
data_plot_all_Both <- rbind(data_plot_all,data_forplot) #得到了一个关键的作图数据框：data_plot_all_Both
summary(data_plot_all_Both)

#8、 作图
data_plot_all <- rbind(data_plot_all_Both, data_plot_all_Female,data_plot_all_Male)

color_forpoint = c('Both' = '#1d2786',
                   'Female' = '#008891',
                   'Male' = '#c51350')
#'Low SDI' = '#e78ac3',
#'Low-middle SDI' = '#a6d854',
#'Middle SDI' = '#ffd92f')
shape_forpoint = c('Both' = 15,
                   'Female' = 16,
                   'Male' = 17)
#'Low SDI' = 18,
#'Low-middle SDI' = 25,
#'Middle SDI' = 8) #color_forpoint和shape_forpiont都是根据需要来修改

color_forfan = '#c8d9eb'
plot <- ggplot(data_plot_all, aes(x=year,y=value, group = sub))+
  geom_fan()+scale_fill_gradientn(colours = rep(color_forfan,2), guide = FALSE) +
  geom_point( aes(x = year, y = median_data2, color = sub,shape = sub))+
  geom_line(aes(x = year, y = median_data, color = sub))+
  scale_color_manual(values = color_forpoint)+
  scale_shape_manual(values = shape_forpoint)+
  scale_y_continuous(breaks = c(160,200,240,280,320,360,400),limits = c(150,400))+
  geom_vline(xintercept = 2021,linetype = 2)+
  ylab('ASDR (per 100,000)') +
  xlab('Year')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.key = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank()) 
summary(data_plot_all)
unique(data_plot_all_Both$quantile)
ggsave(plot, file="Global_threesex_1990-2035_ASDR1.pdf",units = "cm", width = 10,height = 8)


save.image(file='BAPC-人口-性别4.RData')
load('BAPC-人口-性别4.RData')

# 提取2035年的近似95%置信区间和中位数
data_2035 <- data_plot_all_Both %>%
  filter(year == 2035) %>%
  filter(quantile %in% c("0.024Q", "0.5Q", "0.976Q"))

# 检查结果
print(data_2035)
