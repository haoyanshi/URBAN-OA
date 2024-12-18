---
  title: "Map_prevalence_ASR_2019"
output: html_document
date: "2023-08-03"
---
  #1.下载并加载R包
  ```{R}
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("maps")
#install.packages("cowplot")
#install.packages("patchwork")
#install.packages("sf")
#install.packages("maptools")
#install.packages("tmaptools")
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
library(cowplot)
library(patchwork)
library(purrr)
library(stringr)
library(sf)
library(maptools)
library(tmaptools)
library(ggplot2)
library(vroom)
```

#2.导入数据(2大数据：GBD数据库 & 世界地图数据)
```{R}
#Countries_1990to2019 <- read.csv('./1.数据/204counties_1990to2019.csv', header=T) 
load('GBD_maps.RData')
```

path = "OA 204单个年份/"
fileName = dir(path)
fileName
Countries_1990to2021<-data.frame()
for(k in 1:length(fileName)){
  data_temp = vroom(file = paste(path,fileName[k],sep = "\\"))
  Countries_1990to2021=rbind(Countries_1990to2021,data_temp)
}

#3.大地图数据清洗_Prevalence_ASR_2019
```{R}
##3.1 数据筛选（目的：最终选出204个location的val)####
DALY2021_ASR  <- Countries_1990to2021 %>%
  filter(year == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图

summary(DALY2021_ASR$val)
##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
breaks <- c(165,190,215,240,265,290,315,340)
###3.2.2 根据节点形成区间break_labels####
breaks_labels <- imap_chr(breaks, function(., idx){
  return(paste0(breaks[idx], " to ", breaks[idx+1]))
})
breaks_labels <- breaks_labels[1:length(breaks)-1]  
breaks_labels[length(breaks_labels)] <- paste0('>=', str_split(breaks_labels[length(breaks_labels)],' ',simplify = T)[1])
breaks_labels
###3.2.3 给不同的区间加上不同的颜色 pal####
pal <- tmaptools::get_brewer_pal(palette = "PuOr",n = length(breaks_labels)) 
pal <- c('#7aa68e','#98b7a6','#baccc2','#e4e4e4','#ba9dd7','#a181c9','#8667ba','#6551ad')
pal <- c('#6b58a6','#8778b7','#a89ccc','#cfc9e4','#d78670','#c96651','#ba4535','#ad1f1e') 


##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
DALY2021_ASR_map <- left_join(DALY2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）

##3.4 数据可视化;（基于ggplot函数，需要了解其设计理念：基于图层来构建图形。图层由三部分组成：数据、映射、几何对象）####
DALY2021_ASR_map_plot <- ggplot(data = DALY2021_ASR_map,aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) + #geom_sf函数用于绘制空间几何图形
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  theme_void()+
  labs(x="", y="")+
  guides(fill = guide_legend(title='ASDR in 2021 per 100,000 persons', ncol =2))+
  theme(text = element_text(size = 6),
        legend.position = c(0.06,0.1),
        legend.key.size = unit(0.4, "cm")
  )+
  coord_sf(xlim = c(-180,208), expand = FALSE)
DALY2021_ASR_map_plot
```

#4 小地图数据清洗_Prevalence_ASR_2019
```{R}
##4.1 Caribbean and central America#### （每个小地图，涉及2大步：1.筛选作图数据；2.利用ggplot作图，在开始第二步前，需要先准备主题设置以及X和Y边界）
###4.1.1 准备作图数据####
a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['Caribbean and central America']]$Location.Name,]

###4.1.2 ggplot前准备####
####准备1：主题theme （适用于所有小地图）
theme_map_sub <- theme_void()+labs(x="", y="")+theme_bw()+
  theme(text = element_text(size = 4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(vjust = 1, hjust = 0.5)) 
####准备2：确定小地图的X和Y边界（每个小地图不同）
x_location = c(-90,-59) #经度区间
y_location = c(7,28) #纬度区间

###4.1.3 可视化####
sub1 <- ggplot(data = a, aes(fill = val2)) +   #创建ggplot对象“名为sub1”，并设置了数据源为a。fill = val2指定了在图中使用val2变量的值作为填充颜色
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Caribbean and central America')+
  theme_map_sub


## 4.2 Persian Gulf ####
###4.2.1 准备作图数据
a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['Persian Gulf']]$Location.Name,]

###4.2.2 ggplot前准备
x_location = c(45,55)
y_location = c(18.5,31.5)

###4.2.3 可视化
sub2 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Persian Gulf')+
  theme_map_sub


## 4.3 Balkan Peninsula ####

a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['Balkan Peninsula']]$Location.Name,]

x_location = c(12.5,32)
y_location = c(35,53)

sub3 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Balkan Peninsula')+
  theme_map_sub

## 4.4 Southeast Asia ####
a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['Southeast Asia']]$Location.Name,]

x_location = c(97.5,119.7)
y_location = c(-9.2,9)

sub4 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Southeast Asia')+
  theme_map_sub


## 4.5.West Africa ####
a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['West Africa']]$Location.Name,]

x_location = c(-17.5,-7)
y_location = c(6.8,16.7)

sub5 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('West Africa')+
  theme_map_sub


## 4.6.Eastern Mediterranean ####
a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['Eastern Mediterranean']]$Location.Name,]

x_location = c(30.5,38.5)
y_location = c(29.1,34.9)

sub6 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Eastern Mediterranean')+
  theme_map_sub

## 4.7.Northern Europe ####
a <- DALY2021_ASR_map[DALY2021_ASR_map$location_name %in% subregions_shp[['Nothern Europe']]$Location.Name,]

x_location = c(2.5,27)
y_location = c(48,59)
sub7 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Northern Europe')+
  theme_map_sub
```

#5.拼图
```{R}
plot1 <- (sub1 + sub2 + sub3 + sub4) + plot_layout(nrow = 1) 

plot2 <- (sub5 | sub6) / sub7 + plot_layout(height = c(1, 1.2))

plot3 <- plot1|plot2  + plot_layout(widths = c(1, 15))

DALY2021_ASR_map_plot <- DALY2021_ASR_map_plot / plot3 + plot_layout(height = c(2,1),widths = c(2,1))
```
DALY2021_ASR_map_plot

#6.保存
```{R}
ggsave(DALY2021_ASR_map_plot, file = 'DALY2021_ASR_map.pdf', units = 'cm', width = 24, height = 17.8)
save.image(file = "Figure2.RData")
load('Figure2.RData')
```  