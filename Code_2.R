###Figure 1###
library(readxl)
library(ggplot2)

df <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/公共充电站数量.xlsx")

ggplot(df, aes(x = Year, y = Number, color = type, linetype = type)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(limits=c(-500,37000), breaks=seq(0,36000,4000),expand = c(0, 0))  +
  scale_x_continuous(limits=c(2009.8,2024.2), breaks=seq(2010,2024,1),expand = c(0, 0))  +
  theme_bw() + # 去掉背景和刻度轴
  theme(
    panel.grid.major.x=element_line(linetype="dashed"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.y=element_line(linetype="dashed"),
    panel.grid.minor.y=element_blank(),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank(),                     
    legend.text = element_text(size = 10),             
    legend.position = c(0.01, 0.98),                   
    legend.justification = c("left", "top"),         
    plot.margin = margin(2, 6, 1, 5) 
  )


###Figure 2###
library(readxl)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(cowplot)
library(showtext)
library(dplyr)
china_map_c <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/县.shp')
nine_lines <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/九段线.shp')

new_county <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本.xlsx",sheet = 17)

china_map_c <- china_map_c %>% 
  left_join(new_county %>% select(县代码, value), by = "县代码")

cdz_spatial <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本.xlsx",sheet = 13)
cdz_spatial$gd_update_date <- as.Date(cdz_spatial$gd_update_time)
cdz_spatial_1 <- cdz_spatial[, c("id", "gd_update_date", "Longitude", "Latitude")]
cdz_spatial_1 <- cdz_spatial_1 %>%
  mutate(type = "Added during sample period")

cdz2_spatial <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本.xlsx",sheet = 16)
cdz2_spatial_1 <- cdz2_spatial[, c("id", "Longitude", "Latitude")]
cdz2_spatial_1 <- cdz2_spatial_1 %>%
  mutate(type = "Existing prior to sample period")

df_merged <- bind_rows(cdz2_spatial_1, cdz_spatial_1)

df_merged2 <- df_merged[123533:123534, ]

library(ggstar)
library(scales)
library(ggnewscale)
m2 <- ggplot() +
  geom_sf(data = china_map_c, aes(fill = value), color='grey50', size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  annotation_scale(location='bl', text_cex = 0.85) + 
  coord_sf(ylim = c(13,53)) +
  scale_fill_manual(values = c('#fdfcdc'), labels=c('A'= 'No EVCS installations until the sample period'),
                    name = "", na.value = 'grey95',na.translate = FALSE) +
  new_scale_fill() +
  geom_star(data = df_merged, aes(x = Longitude, y = Latitude, fill = type), starshape = 15, size = 0.27, 
            alpha = 0.55, col = "black" , starstroke = 0.05) +
  scale_fill_manual(
    values = c(
      "Added during sample period" = "#ffc2d1",   
      "Existing prior to sample period" = "#00a6fb" 
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(0.005, 0.05), 
    legend.justification = c("left", "bottom"),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(override.aes = list(size = 1.2))) +
  labs(x = "", y = "", title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

ggsave("/Users/guoliyangccc/Desktop/222.png", m2, width = 25, height = 15, units = "cm", dpi = 600)


m3 <- ggplot() +
  geom_sf(data=china_map_c,color='grey50',fill = "grey95", size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  coord_sf(ylim = c(0,23),xlim = c(105,120)) +
  geom_star(data = df_merged, aes(x = Longitude, y = Latitude, fill = type), starshape = 15, size = 0.27, 
            alpha = 0.55, col = "black" , starstroke = 0.05) +
  scale_fill_manual(
    values = c(
      "Added during sample period" = "#ffc2d1",   
      "Existing prior to sample period" = "#00a6fb" 
    )
  ) +
  theme_bw() +
  theme(aspect.ratio = 1.25, #调节长宽比
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="grey10",linetype=1,linewidth=0.5),
        legend.position = "none")

ggsave("/Users/guoliyangccc/Desktop/333.png", m3, width = 10, height = 6, units = "cm", dpi = 600)



###Figure 3###
library(readxl)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(cowplot)
library(showtext)
library(dplyr)
library(ggstar)
library(scales)

china_map_c <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/县.shp')
nine_lines <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/九段线.shp')

cdz_spatial <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本.xlsx",sheet = 13)
cdz_spatial$gd_update_date <- as.Date(cdz_spatial$gd_update_time)

cdz_spatial_1 <- cdz_spatial[, c("id", "gd_update_date", "Longitude", "Latitude")]

m1<-ggplot() +
  geom_sf(data=china_map_c,color='grey50',fill = "#fdfcdc", size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  annotation_scale(location='bl', text_cex = 0.85) +  #加比例尺
  coord_sf(ylim = c(13,53)) + #截取
  geom_star(data = cdz_spatial_1, aes(x = Longitude, y = Latitude, fill = gd_update_date), starshape = 15, size = 0.4, 
            alpha = 0.55, col = "black" , starstroke = 0.06) +
  scale_fill_gradient(low = "#ff006e", high = "#00a6fb",name = "Opening date",
                      breaks = seq(min(cdz_spatial_1$gd_update_date), max(cdz_spatial_1$gd_update_date), by = "1 month"),  
                      labels = date_format("%Y-%m")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "none"
  ) +
  labs(x = "", y = "", title = "Charging stations across China") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

ggsave("/Users/guoliyangccc/Desktop/222.png", m1, width = 5, height = 3, units = "in", dpi = 300)


m4 <- ggplot() +
  geom_sf(data=china_map_c,color='grey50',fill = "#fdfcdc", size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  coord_sf(ylim = c(0,23),xlim = c(105,120)) +
  geom_star(data = cdz_spatial_1, aes(x = Longitude, y = Latitude, fill = gd_update_date), starshape = 15, size = 0.4, 
            alpha = 0.55, col = "black" , starstroke = 0.06) +
  scale_fill_gradient(low = "#ff006e", high = "#00a6fb",name = "Opening date",
                      breaks = seq(min(cdz_spatial_1$gd_update_date), max(cdz_spatial_1$gd_update_date), by = "1 month"),  
                      labels = date_format("%Y-%m")) +
  theme_bw() +
  theme(aspect.ratio = 1.25, #调节长宽比
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="grey10",linetype=1,linewidth=0.5),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.position = "none")


ggsave("/Users/guoliyangccc/Desktop/222.png", m4, width = 5, height = 3, units = "in", dpi = 300)


###Figure 4###
library(readxl)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(cowplot)
library(showtext)
library(dplyr)

colors = c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B","#03045e")

china_map_c <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/县.shp')
nine_lines <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/九段线.shp')

policy_shock <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电设施推广县.xlsx",sheet = 2)

china_map_c <- china_map_c %>%
  left_join(
    policy_shock %>% select(县代码, town_I, p_policy),
    by = "县代码"
  )

china_map_c$town_I_level <- cut(china_map_c$town_I,breaks = c(0.5,1.5,5.5,10.5,15.5,20.5,25.5,30.5),
                                include.lowest = T,labels = 1:7)

m5 <- ggplot() +
  geom_sf(data=china_map_c,color='grey35',aes(fill=town_I_level),linewidth=0.1) + 
  geom_sf(data = nine_lines,aes(), linewidth=0.5) +
  scale_fill_manual(name = "Number of pilot townships" ,
                    values = colors,
                    labels=c('1','2~5','6~10','11~15','16~20','21~25','26~30','none'),na.value = "white") +
  annotation_scale(location='bl', text_cex = 0.85) + 
  coord_sf(ylim = c(13,53)) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = c(0.005, 0.05), 
    legend.justification = c("left", "bottom"),
    legend.key.size = unit(0.4, "cm") 
  )


m6<-ggplot() +
  geom_sf(data=china_map_c,color='grey35',aes(fill=town_I_level),linewidth=0.1) + 
  geom_sf(data = nine_lines,aes(), linewidth=0.5) +
  scale_fill_manual(name = "Number of pilot townships" ,
                    values = colors,
                    labels=c('1','2~5','6~10','11~15','16~20','21~25','26~30','none'),na.value = "white") +
  coord_sf(ylim = c(0,23),xlim = c(105,120)) +
  theme_bw() +
  theme(aspect.ratio = 1.25, 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="grey10",linetype=1,size=0.5),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.position = "none")


ggsave("/Users/guoliyangccc/Desktop/777.png", m6, width = 10, height = 6, units = "in", dpi = 600)

library(cowplot)
m5_6 <- ggdraw() +
  draw_plot(m5, 0, 0, 1, 1) + 
  draw_plot(m6, 0.693, 0.05, 0.25, 0.25)

ggsave("/Users/guoliyangccc/Desktop/777.png", m5_6, width = 10, height = 6, units = "in", dpi = 600)


###Figure 5###
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

tg_county <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/推广县充电站增加散点.xlsx")

tg_county <- tg_county %>%
  mutate(
    date = as.Date(time_day),
    cutoff = as.Date("2024-07-02"),
    running_c = as.numeric(date - cutoff),
    post = if_else(type == "B", 1, 0)
  )

model_rdd <- lm(
  station_count ~ post + running_c + post:running_c,
  data = tg_county
)

summary(model_rdd)


cutoff_date <- as.Date("2024-07-02")  
#（post = 0）
new_left <- data.frame(
  running_c = seq(min(tg_county$running_c), 0, length.out = 200),
  post = 0
) %>%
  mutate(date = cutoff_date + running_c)

pred_left <- predict(model_rdd, newdata = new_left,
                     interval = "confidence")

new_left <- cbind(new_left, pred_left)

#（post = 1）
new_right <- data.frame(
  running_c = seq(0, max(tg_county$running_c), length.out = 200),
  post = 1
) %>%
  mutate(date = cutoff_date + running_c)

pred_right <- predict(model_rdd, newdata = new_right,
                      interval = "confidence")

new_right <- cbind(new_right, pred_right)

ggplot() +
  geom_point(
    data = tg_county,
    aes(x = date, y = station_count, color = factor(post)),
    alpha = 0.5, size = 1.5
  ) +
  scale_color_manual(values = c("0" = "#f26a8d", "1" = "#00bbf9")) +
  
  geom_ribbon(
    data = new_left,
    aes(x = date, ymin = lwr, ymax = upr),
    fill = "#bc4749", alpha = 0.2
  ) +

  geom_line(
    data = new_left,
    aes(x = date, y = fit),
    color = "#bc4749", linewidth = 1
  ) +

  geom_ribbon(
    data = new_right,
    aes(x = date, ymin = lwr, ymax = upr),
    fill = "#3a86ff", alpha = 0.2
  ) +

  geom_line(
    data = new_right,
    aes(x = date, y = fit),
    color = "#3a86ff", linewidth = 1
  ) +
  geom_vline(
    xintercept = as.numeric(cutoff_date),
    linetype = "dashed"
  ) +
  theme_bw() +
  labs(x = "Date", y = "Number of new EVCS installations") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %Y") +
  scale_y_continuous(limits=c(0, 58), breaks = seq(0,60, by = 5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x=element_line(linetype="dashed"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none")



###Figure 6###
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
fss <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本2.xlsx",sheet = 5)

ggplot(fss, aes(x = stations, y = value, group = type)) +
  geom_ribbon(aes(ymin = ci_95_lower, ymax = ci_95_higher, fill = type),
              alpha = 0.3) +
  scale_fill_manual(values = c(
    "IV: Policy_shock" = "#ee6c4d",  
    "IV: Slope_cable" = "#00a896" )) +
  geom_line(aes(color = type), linewidth = 0.8) +
  scale_color_manual(values = c(
    "IV: Policy_shock" = "#ee6c4d",  
    "IV: Slope_cable" = "#00a896" )) +
  geom_vline(xintercept = 753, linetype = "dashed") +
  scale_x_continuous(limits=c(-5, 805), breaks = seq(0, 800, by = 50), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) {
    labs <- label_number(accuracy = 0.001)(x) 
    sub("^(-?)0\\.", "\\1.", labs)           
  }) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x=element_line(linetype="dashed"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
  ) +
  theme(
    legend.position = c(0.001, 0.999),     
    legend.text = element_text(size = 10),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha('white', 0)),
    legend.title = element_blank()
  ) +
  labs(x = "Number of new EVCS installations", y = "Marginal effect on PM2.5") +
  theme(
    axis.title.x = element_text(size = 10), 
    axis.title.y = element_text(size = 10)   
  ) +
  annotate("text", x = 750, y = -0.015, label = "Sample maximum", 
           hjust = 1, size = 3.7, fontface = "italic")



###Figure 7###
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
wenjian1 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本2.xlsx",sheet = 1)
wenjian2 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本2.xlsx",sheet = 2)
wenjian3 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本2.xlsx",sheet = 3)

wenjian1$type <- factor(wenjian1$type, levels = rev(unique(wenjian1$type))) 
wenjian2$type <- factor(wenjian2$type, levels = rev(unique(wenjian2$type)))  
wenjian3$type <- factor(wenjian3$type, levels = rev(unique(wenjian3$type)))

p1 <- ggplot(wenjian1, aes(x = coefficient, y = type, col = type2)) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_higher),
    height = 0.27, linewidth = 0.6,
    na.rm = TRUE  
  ) + 
  geom_point(shape = 9, size = 1.7) +
  scale_color_manual(values = c(
    "A" = "#005f73",  
    "B" = "#da627d" )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits=c(-0.026, 0.001), breaks = seq(-0.025, 0.000, by = 0.005), expand = c(0, 0), 
                     labels = function(x) {
                       labs <- label_number(accuracy = 0.001)(x) 
                       sub("^(-?)0\\.", "\\1.", labs)             
                     }) +
  scale_y_discrete(labels = c(
    "Alternative measures" = expression(bold("Alternative measures")),
    "Confounding factors"  = expression(bold("Confounding factors")),
    "Data sample"          = expression(bold("Data sample")),
    "Data matching"        = expression(bold("Data matching"))
  )) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x=element_line(linetype="dashed"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 0, 1, 1)
  ) +
  labs(title = "OLS estimates") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p2 <- ggplot(wenjian2, aes(x = coefficient, y = type, col = type2)) +
  geom_errorbarh(
    aes(xmin = ci_95_lower, xmax = ci_95_higher),
    height = 0.27, linewidth = 0.6,
    na.rm = TRUE   
  ) +
  geom_point(shape = 10, size = 1.7) +
  scale_color_manual(values = c(
    "A" = "#005f73",  
    "B" = "#7678ed" )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits=c(-0.016, 0.001), breaks = seq(-0.015, 0.000, by = 0.005), expand = c(0, 0), 
                     labels = function(x) {
                       labs <- label_number(accuracy = 0.001)(x) 
                       sub("^(-?)0\\.", "\\1.", labs)          
                     }) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x=element_line(linetype="dashed"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 0, 1, 0)
  ) +
  labs(title = "Policy_shock IV estimates") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10)
  )

p3 <- ggplot(wenjian3, aes(x = coefficient, y = type, col = type2)) +
  geom_errorbarh(
    aes(xmin = ci_95_lower, xmax = ci_95_higher),
    height = 0.27, linewidth = 0.6, 
    na.rm = TRUE  
  ) +
  geom_point(shape = 12, size = 1.7) +
  scale_color_manual(values = c(
    "A" = "#005f73",  
    "B" = "#00b4d8" )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits=c(-0.016, 0.001), breaks = seq(-0.015, 0.000, by = 0.005), expand = c(0, 0), 
                     labels = function(x) {
                       labs <- label_number(accuracy = 0.001)(x)
                       sub("^(-?)0\\.", "\\1.", labs)           
                     }) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x=element_line(linetype="dashed"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 1, 0)
  ) +
  labs(title = "Slope_cable IV estimates") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10)
  )

library(patchwork)
p <- p1 + p2 + p3
p



###Figure 8 was generated using ArcGIS 10.2###


###Figure 9###
library(readxl)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(cowplot)
library(showtext)
library(dplyr)
china_map_c <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/县.shp')
nine_lines <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/九段线.shp')
coal_power <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/区县是否含火电厂.xlsx")
coal_power_2 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/煤电厂方位.xlsx",sheet=2)

coal_power_10_50 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/区县0-50km是否含火电厂.xlsx")

##merged_data <- merge(china_map_c, coal_power, by = "县代码", all.x = TRUE)

merged_data <- merge(china_map_c, coal_power_10_50, by = "县代码", all.x = TRUE)


merged_data$plant_nearby_0<- rep(0, 2877)

merged_data$c_0_10 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_10
merged_data$c_0_20 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_20
merged_data$c_0_30 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_30
merged_data$c_0_40 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_40
merged_data$c_0_50 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_50

merged_data$c_10_50 <- merged_data$c_0_10 + merged_data$c_0_20 + merged_data$c_0_30 + merged_data$c_0_40 + merged_data$c_0_50

merged_data$type <- cut(merged_data$c_10_50,breaks = c(-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5),
                        include.lowest = T,labels = 1:6)
table(merged_data$type)

merged_data$type[merged_data$省代码 == "710000"] <- NA
merged_data$type[merged_data$省代码 == "810000"] <- NA
merged_data$type[merged_data$省代码 == "820000"] <- NA

colors <- c("#FDB863", "#E5F5F9", "#CCECE6", "#99D8C9", "#66C2A4", "#41AE76")

p1 <- ggplot() +
  geom_sf(data=merged_data,color='grey50',aes(fill=type),size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  geom_point(data=coal_power_2,aes(x=longitude, y=latitude), size=0.6) +
  scale_fill_manual(values = colors,
                    labels=c('[0km, 10km]','(10km, 20km]','(20km, 30km]',
                             '(30km, 40km]','(40km, 50km]','> 50km'), na.value = "white") +
  annotation_scale(location='bl',text_cex = 1) +  
  coord_sf(ylim = c(16,55)) + 
  theme_bw() +
  theme(legend.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13)) +
  xlab('') +
  ylab('') +
  labs(fill = "Distance to coal-fired power plant")


ggsave("/Users/guoliyangccc/Desktop/充电桩/pkm1.png", p1, width = 30, height = 24, units = "cm", dpi = 300)



p2 <- ggplot() +
  geom_sf(data=merged_data,color='grey50',aes(fill=type),size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  geom_point(data=coal_power_2,aes(x=longitude, y=latitude), size=0.6) +
  scale_fill_manual(values = colors,
                    labels=c('without coal-fired power plant','with coal-fired power plant')) +
  coord_sf(ylim = c(3,23),xlim = c(105,120)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
  ) +
  xlab('') +
  ylab('')

ggsave("/Users/guoliyangccc/Desktop/充电桩/pkm2.png", p2, width = 10, height = 8, units = "cm", dpi = 300)



###Figure 10###
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
sywr1 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本2.xlsx",sheet = 8)
sywr2 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/充电桩-稳健性检验IV更新版本2.xlsx",sheet = 9)

sywr1$type <- factor(sywr1$type, levels = c("Within 10km", "Beyond 10km", "Beyond 20km", 
                                            "Beyond 30km", "Beyond 40km", "Beyond 50km"))

sywr2$type <- factor(sywr2$type, levels = c("[q0, q25)", "[q25, q50)", "[q50, q75)", "[q75, q100]"))


s1 <- ggplot(sywr1, aes(x = type, group = type2)) +
  geom_ribbon(aes(ymin = ci_95_lower, ymax = ci_95_higher, fill = type2), alpha = 0.25, colour = NA) +
  geom_line(aes(y = coefficient, colour = type2), linewidth = 0.8) +
  geom_point(aes(y = coefficient, colour = type2, shape = type2), size = 2.2, stroke = 0.8) +
  scale_shape_manual(values = c(
    "OLS estimates" = 0,   
    "Policy_shock IV estimates" = 1,
    "Slope_cable IV estimates" = 5
  )) +
  scale_fill_manual(values = c(
    "OLS estimates" = "#ee4b6a",   
    "Policy_shock IV estimates" = "#119da4",
    "Slope_cable IV estimates" = "#a06cd5"
  )) +
  scale_colour_manual(values = c(
    "OLS estimates" = "#ee4b6a",   
    "Policy_shock IV estimates" = "#119da4",
    "Slope_cable IV estimates" = "#a06cd5"
  )) +
  scale_y_continuous(limits=c(-0.020, 0.000), breaks = seq(-0.020, 0.000, by = 0.004), expand = c(0, 0), 
                     labels = function(x) {
                       labs <- label_number(accuracy = 0.001)(x)  
                       sub("^(-?)0\\.", "\\1.", labs)       
                     }) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) + 
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
        plot.margin = margin(5, 5, 1, 5) 
  ) +
  labs(x = "Exposure distance to coal-fired power plants", y = "Effect on PM2.5") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 1)) +
  theme(
    legend.position = c(0.002, 0.002),     
    legend.justification = c("left", "bottom"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = alpha('white', 30)),
    legend.title = element_blank()
  ) 





s2 <- ggplot(sywr2, aes(y = coefficient, x = type, fill = type2,col = type2)) +
  geom_col(position = position_dodge(width = 0.5), col='grey40', size = 0.2,width = 0.5) +
  geom_errorbar(
    aes(ymin = ci_95_lower, ymax = ci_95_higher),
    width = 0.2, linewidth = 0.7, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(
    "OLS estimates" = "#ee4b6a",   
    "Policy_shock IV estimates" = "#119da4",
    "Slope_cable IV estimates" = "#a06cd5"
  )) +
  scale_fill_manual(values = c(
    "OLS estimates" = "#ffafcc",   
    "Policy_shock IV estimates" = "#b9fbc0",
    "Slope_cable IV estimates" = "#cdb4db"
  )) +
  scale_y_continuous(limits=c(-0.020, 0.000), breaks = seq(-0.020, 0.000, by = 0.004), expand = c(0, 0), 
                     labels = function(x) {
                       labs <- label_number(accuracy = 0.001)(x)  
                       sub("^(-?)0\\.", "\\1.", labs)            
                     }) +
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +  
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(linetype="dashed"),
        panel.grid.minor.y=element_blank(),
        plot.margin = margin(5, 1, 1, 5) 
  ) +
  labs(x = "Quantiles of the non-fossil energy generation share", y = "Effect on PM2.5") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 1)) +
  theme(
    legend.position = c(0.002, 0.002),
    legend.justification = c("left", "bottom"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = alpha('white', 30)),
    legend.title = element_blank()
  ) 

library(patchwork)
library(cowplot)

s1 + s2



###Supplementary Figure A3###
library(readxl)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(cowplot)
library(showtext)
library(dplyr)
china_map_c <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/县.shp')
nine_lines <- read_sf('/Users/guoliyangccc/rstudio/mydata/地图/行政区划2022/九段线.shp')
coal_power <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/区县是否含火电厂.xlsx")
coal_power_2 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/煤电厂方位.xlsx",sheet=2)

coal_power_10_50 <- read_xlsx("/Users/guoliyangccc/Desktop/充电桩/区县0-50km是否含火电厂.xlsx")

##merged_data <- merge(china_map_c, coal_power, by = "县代码", all.x = TRUE)

merged_data <- merge(china_map_c, coal_power_10_50, by = "县代码", all.x = TRUE)


merged_data$plant_nearby_0<- rep(0, 2877)

merged_data$c_0_10 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_10
merged_data$c_0_20 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_20
merged_data$c_0_30 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_30
merged_data$c_0_40 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_40
merged_data$c_0_50 <- merged_data$plant_nearby_0 - merged_data$plant_nearby_50

merged_data$c_10_50 <- merged_data$c_0_10 + merged_data$c_0_20 + merged_data$c_0_30 + merged_data$c_0_40 + merged_data$c_0_50




colors <- c("white", "#E08214")


merged_data$type <- cut(merged_data$has_coal_power,breaks = c(-1,0.5,2),
                        include.lowest = T,labels = 1:2)

p1 <- ggplot() +
  geom_sf(data=merged_data,color='grey50',aes(fill=type),size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  geom_point(data=coal_power_2,aes(x=longitude, y=latitude), size=0.6) +
  scale_fill_manual(values = colors,
                    labels=c('without coal-fired power plant','with coal-fired power plant')) +
  annotation_scale(location='bl',text_cex = 1) +  
  coord_sf(ylim = c(16,55)) +
  theme_bw() +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) +
  xlab('') +
  ylab('')


ggsave("/Users/guoliyangccc/Desktop/充电桩/p1.png", p1, width = 30, height = 24, units = "cm", dpi = 300)


p2 <- ggplot() +
  geom_sf(data=merged_data,color='grey50',aes(fill=type),size=0.2) + 
  geom_sf(data = nine_lines,aes(), size=0.9) +
  geom_point(data=coal_power_2,aes(x=longitude, y=latitude), size=0.6) +
  scale_fill_manual(values = colors,
                    labels=c('without coal-fired power plant','with coal-fired power plant')) +
  coord_sf(ylim = c(3,23),xlim = c(105,120)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank() 
  ) +
  xlab('') +
  ylab('')

ggsave("/Users/guoliyangccc/Desktop/充电桩/p3.png", p2, width = 10, height = 8, units = "cm", dpi = 300)