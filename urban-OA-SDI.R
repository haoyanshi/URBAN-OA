---
  title: "Correlation_SDI_地区层面"
output: html_document
date: "2023-08-7"
---
  
  #1.安装并加载相关R包
  ```{r}
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("ggplot2")

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(vroom)
```


#2.加载数据(主要有三个:1.全球和21个地区从1990到2019年的SDI数据；2.全球和21个地区在2019年的DALYs ASR；3.全球和21个地区的list)
```{r}
##获取SDI_value文件(这个文件是location × year的SDI矩阵)
SDI_value_2019 <- readxl::read_xlsx('SDI_value_GBD2019_1990_2019.XLSX',sheet=2,skip=1) #读入xlsx文件的第二个sheet,跳过第一行

SDI_value_2021 <- read.csv('IHME_GBD_SDI_2021_SDI_1950_2021_Y2024M05D16.csv', header=T)
colnames(SDI_value_2021)
SDI_value_2021 <- SDI_value_2021 %>%
  select("location_name", "year_id", "mean_value") 
# 创建一个正则表达式，匹配1990到2021的字符串
year_pattern <- paste0("^(", paste(1990:2021, collapse = "|"), ")$")

filtered_df <- SDI_value_2021 %>%
  filter(str_detect(year_id, year_pattern))

wide_df <- filtered_df %>%
  pivot_wider(names_from = year_id, values_from = "mean_value", values_fn = mean) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

SDI_value_2021_wide <- wide_df
SDI_value_2021_wide <- SDI_value_2021_wide %>%
  rename(
    Location = location_name
  )

##获取Global和21regions的COPD疾病负担数据
path = "单个年份/"
fileName = dir(path)
fileName
Countries_1990to2021<-data.frame()
for(k in 1:length(fileName)){
  data_temp = vroom(file = paste(path,fileName[k],sep = "\\"))
  Countries_1990to2021=rbind(Countries_1990to2021,data_temp)
}

##获取Global和21regions的list文件
order_globalandregions <- read.csv("order_globalandregions.csv", header = F)
```


#3.数据清洗（即需要整理出3列数据，分别是Global和21个region的location_year pair，SDI值，疾病负担指标如ASR）
```{r}
##3.1 制作第一列数据：Global和21个regions的"location_year"组合，一共22*30=660行，在整理第二列和第三列数据的时候，整理

##3.2 制作第二列数据：Global和21个regions在1990-2019年的SDI数据
SDI_value_Global_21Regions <- SDI_value_2021_wide %>%
  filter(Location %in% order_globalandregions$V1)

SDI_value_Global_21Regions_long <- pivot_longer(SDI_value_Global_21Regions, # 输入的数据框（wide format）
                                                cols = colnames(SDI_value_Global_21Regions)[-1],  # 指定要进行转换的列，排除第一列
                                                names_to = 'Year', # 转换后的列名将存储在名为'Year'的新列中
                                                values_to = 'SDI') %>% # 转换后的数值将存储在名为'SDI'的新列中
  mutate(ID = paste0(Location,"_", Year)) #用于后续数据框合并


##3.3 制作第三列数据：全球和21个地区，1990-2019年，年龄标准化的DALYs rate
Global_21Regions_1990to2021 <- Countries_1990to2021
Global_21regions_DALYs_ASR <- Global_21Regions_1990to2021 %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","val") %>%
  filter(location_name %in% order_globalandregions$V1,
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate") %>%
  rename("DALYs_ASR" = "val") %>% #列名重命名
  mutate(DALYs_ASR = round(DALYs_ASR/1000,5),
         ID = paste0(location_name,"_",year)) #用于后续数据框合并

##3.4 将上述数据框合并（基于location_time组合）
Global_21regions_DALYs_ASR_LocationandYear <- left_join(SDI_value_Global_21Regions_long,Global_21regions_DALYs_ASR, by = 'ID') %>%
  mutate(SDI = as.numeric(str_replace(SDI,'\\·','.'))) #因为用于相关性分析的变量需要是数值型变量，而不能是字符串型。明确变量类型用class()函数
```

#4.计算相关系数并可视化
```{R}
##4.1 计算相关系数(包括置信区间和p值)
cor_test <- cor.test(Global_21regions_DALYs_ASR_LocationandYear[,"SDI",drop = T], Global_21regions_DALYs_ASR_LocationandYear[, "DALYs_ASR", drop = TRUE])

cor_r <- cor_test$estimate
cor_int<- cor_test$conf.int
cor_p <- cor_test$p.value

##4.2 基于ggplot函数进行可视化(数据、映射【x轴 + y轴】、几何图形【点 + 线 + 文本】、坐标轴信息、主题）
Corr_Global_Regions_SDI <- ggplot(Global_21regions_DALYs_ASR_LocationandYear, # 数据框，用于绘图
                                  aes(Global_21regions_DALYs_ASR_LocationandYear[,"SDI",drop = T],  # x轴变量，从数据框中获取"SDI"列
                                      Global_21regions_DALYs_ASR_LocationandYear[,"DALYs_ASR",drop = T])) + # y轴变量，从数据框中获取"DALYs_ASR"列
  geom_point(aes(color = Location, shape= Location))+ #添加图形几何图层以及该图层中颜色/形状映射
  scale_shape_manual(values = 1:22) +  #手动设置点的形状，一共22种
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) + #添加平滑曲线的图形几何图层
  geom_text(x = min(Global_21regions_DALYs_ASR_LocationandYear$SDI + 0.2), #添加文本标签的图层，设置该文本的位置（x和y)、内容（label）、大小
            y = max(Global_21regions_DALYs_ASR_LocationandYear[,"DALYs_ASR",drop = T])*0.8,
            label = paste("R =", round(cor_r, 2),"(", round(cor_int[1],2), "to", round(cor_int[2],2), ")","\n", "p =", ifelse(cor_p<0.001,"< 0.001",round(cor_p,2))),
            hjust = 1, vjust = 0,
            size = 4)+
  ylab(paste0("DALYs_ASR",'\n (100,000 persons)'))+ # 设置x和y轴的标签
  xlab("Sociodemographic index")+
  theme_bw(base_size = 8)+  #设置主题样式和基本字体大小，theme_bw 函数用于设置图表的主题样式为黑白风格。通过 base_size = 14 参数，设置图表的基本字体大小为 14
  theme(panel.background = element_rect(fill = "transparent"), # 设置绘图区域的背景为透明
        plot.background = element_rect(fill = "transparent"), # 设置整个绘图的背景为透明
        legend.position = 'top', # 设置legend的位置和大小
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(), 
        axis.text.x = element_text(face = "bold",size = 8), # 设置坐标轴文本的外观和大小
        axis.text.y = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold",size = 10),# 设置坐标轴标题的外观和大小
        axis.title.y = element_text(face = "bold",size = 10)) 


Corr_Global_Regions_SDI
```

#5.文件储存
```{R}
ggsave(Corr_Global_Regions_SDI, file = 'DALYs_rate_22region_SDI1.pdf', width = 7, height = 4)
```
