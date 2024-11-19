---
  title: "Table 1_prevalence"
output: html_document
date: "2023-07-25"
---
  #1.下载并加载R包
  ```{R}
library(tidyverse)
```
path1 = "原始数据/全球+21个地区/单个年份/"
path2 = "原始数据/全球+21个地区/连续年份/"
fileName1 = dir(path1)
fileName2 = dir(path2)
fileName1
fileName2
OA_data_dan<-data.frame()
OA_data_lian<-data.frame()
library(vroom)
for(k in 1:length(fileName1)){
  data = vroom(file = paste(path1,fileName1[k],sep = "\\"))
  OA_data_dan=rbind(OA_data_dan,data)
}
for(k in 1:length(fileName2)){
  data = vroom(file = paste(path2,fileName2[k],sep = "\\"))
  OA_data_lian=rbind(OA_data_lian,data)
}

#2.导入数据
```{R}
#Global_21Regions_1990to2019 <- read.csv('./1.数据/Global_21Regions_1990to2019.csv', header=T) #全球及21个地区30年的数据

#Global_21Regions_change <- read.csv('./1.数据/Global_21Regions_change.csv', header=T)
#全球及21个地区30年的变化数据

order_globalandregions <- read.csv('原始数据/order_globalandregions.csv', header=F)
#全球及21个地区的排序文件
```

#3.数据清洗
##3.1 2019_Prevalence_number_bothsex
```{R}
###确定数据清洗的目标：通过对比发现需要进行3个清洗步骤，分别是列行筛选、数值格式、行排序

###基于select和filter函数分别筛选列与行（使用时注意需要添加“，或者&”）
Prevalence2021_number_bothsex <- OA_data_dan %>%
  select("year","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year == "2021",
         sex_name == "Both",
         age_name == "All ages",
         measure_name == "Prevalence",
         metric_name == "Number")

###按照格式计算number（单位、小数点）：使用mutate函数
Prevalence2021_number_bothsex <- Prevalence2021_number_bothsex %>%
  mutate(val = round(val/1000000,1),
         upper = round(upper/1000000,1),
         lower = round(lower/1000000,1),
         Prevalence_Number_2021 = paste0(val,' (',lower,',',upper,')')) 

###对行进行排序
####第一步是新建一个因子（将向量转变为因子），用于第二步
Order_factor <- factor(Prevalence2021_number_bothsex$"location_name", 
                       levels = order_globalandregions$"V1")
####第二步是应用order函数，对数据框的行按照既定的因子进行排序
Prevalence2021_number_bothsex <- Prevalence2021_number_bothsex[order(Order_factor), ]
```

##3.2 2019_Prevalence_rate_Age-standarzied_bothsex
```{R}
###filter函数使用时注意需要添加“，或者&”
Prevalence2021_Rate_bothsex <- OA_data_dan %>%
  select("year","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "Prevalence",
         metric_name == "Rate")

###按照格式计算ASR
Prevalence2021_Rate_bothsex <- Prevalence2021_Rate_bothsex %>%
  mutate(val = round(val,1),
         upper = round(upper,1),
         lower = round(lower,1),
         Prevalence_ASR_2019 = paste0(val,' (',lower,',',upper,')')) 

###对行进行排序
####第一步是新建一个因子，用于第二步
Order_factor <- factor(Prevalence2021_Rate_bothsex$"location_name", levels = order_globalandregions$"V1")
####第二步是应用order函数，对数据框的行按照既定的因子进行排序
Prevalence2021_Rate_bothsex <- Prevalence2021_Rate_bothsex[order(Order_factor), ]

```

##3.3 1990_to_2019change_Prevalence_rate_Age-standarzied_bothsex
```{R}
###结合select和filter函数对数据框进行清洗
Global_21Regions_change_ASPR <- OA_data_lian %>%
  select("year_start","year_end","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year_start == "1990",
         year_end == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "Prevalence",
         metric_name == "Rate")

###按照格式计算ASR
Global_21Regions_change_ASPR <- Global_21Regions_change_ASPR %>%
  mutate(val = round(val*100,1),
         upper = round(upper*100,1),
         lower = round(lower*100,1),
         Prevalence_ASR_change = paste0(val,' (',lower,',',upper,')')) 

###对行进行排序
####第一步是新建一个因子，用于第二步
Order_factor <- factor(Global_21Regions_change_ASPR$"location_name", levels = order_globalandregions$"V1")
####第二步是应用order函数，对数据框的行按照既定的因子进行排序
Global_21Regions_change_ASPR <- Global_21Regions_change_ASPR[order(Order_factor), ]

```

##3.4数据整合
```{R}
### data.frame函数
Table1_Prevalence <- data.frame(Location = Prevalence2021_number_bothsex$location_name, 
                                "No_in_millions_(95%_UI)" = Prevalence2021_number_bothsex$Prevalence_Number_2021, 
                                "ASRs_per_100000_(95%_UI)" = Prevalence2021_Rate_bothsex$Prevalence_ASR_2019, 
                                Percentage_change_in_ASRs_from_1990_to_2021 = Global_21Regions_change_ASPR$Prevalence_ASR_change)

```

##3.5 整理Death和DALYs
```{R}
###Death
####Death_2019 number
Incidence2021_number_bothsex <- OA_data_dan %>%
  select("year","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year == "2021",
         !(location_name %in% c("Low-middle SDI", "Low SDI","Middle SDI","High-middle SDI", "High SDI")),#这个是根据目的来（不一定需要）
         sex_name == "Both",
         age_name == "All ages",
         measure_name == "Incidence",
         metric_name == "Number")


Incidence2021_number_bothsex <- Incidence2021_number_bothsex %>%
  mutate(val = round(val/1000,1),
         upper = round(upper/1000,1),
         lower = round(lower/1000,1),
         Deaths_Number_2021 = paste0(val,' (',lower,',',upper,')')) 

Order_factor <- factor(Incidence2021_number_bothsex$"location_name", levels = order_globalandregions$"V1")
Incidence2021_number_bothsex <- Incidence2021_number_bothsex[order(Order_factor), ]


####Death_2019_ASR
Incidence2021_Rate_bothsex <- OA_data_dan %>%
  select("year","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year == "2021",
         !(location_name %in% c("Low-middle SDI", "Low SDI","Middle SDI","High-middle SDI", "High SDI")),#这个是根据目的来（不一定需要）
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "Incidence",
         metric_name == "Rate")

Incidence2021_Rate_bothsex <- Incidence2021_Rate_bothsex %>%
  mutate(val = round(val,1),
         upper = round(upper,1),
         lower = round(lower,1),
         Incidence_ASR_2021 = paste0(val,' (',lower,',',upper,')')) 

Order_factor <- factor(Incidence2021_Rate_bothsex$"location_name", levels = order_globalandregions$"V1")
Incidence2021_Rate_bothsex <- Incidence2021_Rate_bothsex[order(Order_factor), ]


####Death_2019_ASR_change
Global_21Regions_change_ASPR_Incidence <- OA_data_lian %>%
  select("year_start","year_end","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year_start == "1990",
         year_end == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "Incidence",
         metric_name == "Rate")

Global_21Regions_change_ASPR_Incidence <- Global_21Regions_change_ASPR_Incidence %>%
  mutate(val = round(val*100,1),
         upper = round(upper*100,1),
         lower = round(lower*100,1),
         Incidence_ASR_change = paste0(val,' (',lower,',',upper,')')) 

Order_factor <- factor(Global_21Regions_change_ASPR_Incidence$"location_name", levels = order_globalandregions$"V1")
Global_21Regions_change_ASPR_Incidence <- Global_21Regions_change_ASPR_Incidence[order(Order_factor), ]

####Death 3列数据合并
Table1_Incidence <- data.frame(Location = Incidence2021_number_bothsex$location_name, 
                            "No_in_millions_(95%_UI)" = Incidence2021_number_bothsex$Deaths_Number_2021, 
                            "ASRs_per_100000_(95%_UI)" = Incidence2021_Rate_bothsex$Incidence_ASR_2021, 
                            Percentage_change_in_ASRs_from_1990_to_2021 = Global_21Regions_change_ASPR_Incidence$Incidence_ASR_change)



###DALYs
####DALYs_2019_number
DALYs2021_number_bothsex <- OA_data_dan %>%
  select("year","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year == "2021",
         !(location_name %in% c("Low-middle SDI", "Low SDI","Middle SDI","High-middle SDI", "High SDI")),#这个是根据目的来（不一定需要）
         sex_name == "Both",
         age_name == "All ages",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Number")

DALYs2021_number_bothsex <- DALYs2021_number_bothsex %>%
  mutate(val = round(val/1000,1),
         upper = round(upper/1000,1),
         lower = round(lower/1000,1),
         DALYs_Number_2021 = paste0(val,' (',lower,',',upper,')')) 

Order_factor <- factor(DALYs2021_number_bothsex$"location_name", levels = order_globalandregions$"V1")
DALYs2021_number_bothsex <- DALYs2021_number_bothsex[order(Order_factor), ]


####DALYs_2019_ASR
DALYs2021_Rate_bothsex <- OA_data_dan %>%
  select("year","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year == "2021",
         !(location_name %in% c("Low-middle SDI", "Low SDI","Middle SDI","High-middle SDI", "High SDI")),#这个是根据目的来（不一定需要）
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate")

DALYs2021_Rate_bothsex <- DALYs2021_Rate_bothsex %>%
  mutate(val = round(val,1),
         upper = round(upper,1),
         lower = round(lower,1),
         DALYs_ASR_2021 = paste0(val,' (',lower,',',upper,')')) 

Order_factor <- factor(DALYs2021_Rate_bothsex$"location_name", levels = order_globalandregions$"V1")
DALYs2021_Rate_bothsex <- DALYs2021_Rate_bothsex[order(Order_factor), ]


####DALYs_2019_ASR_change
Global_21Regions_change_ASPR_DALYs <- OA_data_lian %>%
  select("year_start","year_end","location_name","sex_name","age_name","measure_name","metric_name","val","upper", "lower" ) %>%
  filter(year_start == "1990",
         year_end == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate")

Global_21Regions_change_ASPR_DALYs <- Global_21Regions_change_ASPR_DALYs %>%
  mutate(val = round(val*100,1),
         upper = round(upper*100,1),
         lower = round(lower*100,1),
         DALYs_ASR_change = paste0(val,' (',lower,',',upper,')')) 

Order_factor <- factor(Global_21Regions_change_ASPR_DALYs$"location_name", levels = order_globalandregions$"V1")
Global_21Regions_change_ASPR_DALYs <- Global_21Regions_change_ASPR_DALYs[order(Order_factor), ]

####Death 3列数据合并
Table1_DALYs <- data.frame(Location = DALYs2021_number_bothsex$location_name, 
                           "No_in_millions_(95%_UI)" = DALYs2021_number_bothsex$DALYs_Number_2021, 
                           "ASRs_per_100000_(95%_UI)" = DALYs2021_Rate_bothsex$DALYs_ASR_2021, 
                           Percentage_change_in_ASRs_from_1990_to_2021 = Global_21Regions_change_ASPR_DALYs$DALYs_ASR_change)

```

##3.6 整理Table1
```{R}
Table1 <- cbind(Table1_Prevalence,Table1_Incidence,Table1_DALYs) 
write.csv(Table1, file = "Table1.csv")

```
save.image(file = "Table1.RData")
load("Table1.RData")
save(OA_data_dan, file = "OA_data_dan.RData")
