library(ggplot2)
library(readr)

# 设置文件路径
file_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD_all.csv"

# 读取CSV文件
data <- read_csv(file_path)

# 确保Year是字符型，再转换为因子（防止mean被当成数值）
data$Year <- as.character(data$Year)
data$Year <- as.factor(data$Year)

# 设置折线类型
line_types <- c("2018" = "dashed", "2019" = "dashed", "2020" = "dashed", 
                "2021" = "dashed", "2022" = "dashed", "2023" = "dashed",
                "mean" = "solid")  # mean 设为实线

# 设置颜色（更柔和）
line_colors <- c(  "2018" = "#c7c7c7", "2019" = "#aec7e8", "2020" = "#98df8a",
                   "2021" = "#dbdb8d", "2022" = "#ffbb78", "2023" = "#c49c94",
                   "mean" = "#ff0000")  # mean 设为亮红色

# 绘制曲线
ggplot(data, aes(x = PD, y = NDVI, color = Year, linetype = Year)) +
  geom_line(linewidth = 1.0) +  # 线条更粗
  labs(x = "Distance to edge (m)",  
       y = "NDVI",
       color = "Year",
       linetype = "Year") +
  scale_color_manual(values = line_colors) +  # 颜色映射
  scale_linetype_manual(values = line_types) +  # 线条类型映射
  theme_minimal(base_size = 28) +  # 主题
  theme(
    plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),  
    panel.grid = element_blank(),  # 去除网格线
    axis.title = element_text(size = 28),  
    axis.text = element_text(size = 28),   
    legend.title = element_text(size = 28), 
    legend.text = element_text(size = 28),  
    axis.line = element_line(linewidth = 1.0, color = "black"),  
    axis.ticks.x = element_line(linewidth = 1.0, color = "black"),  
    axis.ticks.y = element_line(linewidth = 1.0, color = "black")  
  ) +
  # 添加 100m 处的垂直虚线
  geom_vline(xintercept = 100, linetype = "dashed", color = "#1f77b4", size = 1) 

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD_all3.2.png", width = 12, height = 8, dpi = 600)

