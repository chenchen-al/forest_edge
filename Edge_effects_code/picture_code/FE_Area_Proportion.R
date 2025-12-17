# -------------------------------------------------------------------------------堆积图（前100m）
# 1. 导入必要的包
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# 2. 读取CSV文件
data <- read.csv("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_all.csv")

# 3. 确保 Year 列为因子类型
data$Year <- as.factor(data$Year)

# 4. 将 distance 转换为数值类型并按降序排列
data$distance <- as.numeric(as.character(data$distance))  # 确保 distance 为数值型
data$distance <- factor(data$distance, levels = sort(unique(data$distance), decreasing = TRUE))  # 按降序排序

# 5. 获取 distance的唯一值并为其生成颜色
num_colors <- length(unique(data$distance))  # 获取唯一的距离数量
colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_colors)  # 生成颜色

# 6. 绘制堆积柱状图
ggplot(data, aes(x = Year, y = FE_Area, fill = distance)) +
  geom_bar(stat = "identity", position = "stack") +  # 堆积显示
  scale_fill_manual(values = colors) +  # 使用自动生成的颜色
  labs(fill = "Distance(m)",
       x = "Year",
       y = bquote(bold("FE Area (× 10"^10 * " m"^2 * ")"))  # 纵坐标标题加粗
  ) +
  scale_y_continuous(
    limits = c(0, 60),  # 设置纵坐标轴最大值为60
    breaks = seq(0, 60, by = 15)  # 设置纵坐标轴分段为15
  ) +
  theme_minimal(base_size = 30) +  # 使用简洁主题并调整字体大小
  theme(
    axis.text = element_text(color = "black", size = 30),  # 坐标轴文本为黑色
    axis.title.x = element_text(face = "bold"),  # 横坐标标题不加粗
    axis.title.y = element_text(face = "bold"),  # 纵坐标标题不加粗
    panel.grid.major = element_blank(),  # 去除主要网格线
    panel.grid.minor = element_blank(),  # 去除次要网格线
    legend.position = "none",  # 去除图例
    axis.line = element_line(color = "black", size = 0.6),  # 添加坐标轴线
    axis.ticks = element_line(color = "black", size = 0.6),  # 添加坐标轴刻度线
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # 添加黑色外框线
  ) 

# 7. 保存图表为PNG文件
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_area_variation_within_100_meters.png",
       width = 12, height = 12, dpi = 1000,bg = "white")

# -------------------------------------------------------------------------------堆积图（总）
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# 读取 CSV 数据
forest_data <- read.csv("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_Proportion.csv")

# 确保列名正确
colnames(forest_data) <- trimws(colnames(forest_data))

# 仅选择需要的列，并转换数据格式
forest_long <- forest_data %>%
  select(-total_forest) %>%  # 去掉 total_forest
  pivot_longer(cols = c(edge_0_100m, edge_100_1000m, edge_1000m_plus),
               names_to = "Edge_Zone", values_to = "Area") %>%
  mutate(Edge_Zone = factor(Edge_Zone,
                            levels = c("edge_1000m_plus", "edge_100_1000m", "edge_0_100m"), 
                            labels = c(">1000", "100-1000", "0-100")))  

# 计算每个边缘类别在各年的比例
forest_long <- forest_long %>%
  group_by(Year) %>%
  mutate(Proportion = Area / sum(Area, na.rm = TRUE)) %>%
  filter(!is.na(Proportion) & !is.infinite(Proportion))  # 过滤无效数据

# 绘制堆积柱状图，并在柱状图上标注百分比
# 绘图部分（前面代码不变）
ggplot(forest_long, aes(x = factor(Year), y = Area, fill = Edge_Zone)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.01)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 8, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#1A9850", "#91BFDB", "#D73027")) +
  labs(
    x = "Year",
    y = "Proportion of forest",
    fill = "Distance(m)") +
  theme_minimal(base_size = 30) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
    axis.text = element_text(color = "black", size = 30),
    axis.title = element_text(face = "bold", size = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line.x = element_line(color = "black", size = 0.8),
    axis.line.y = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8, linewidth = 3),
    panel.border = element_blank()
  )

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_Proportion.png",
       width = 10, height = 12, dpi = 1000,bg = "white")
# -------------------------------------------------------------------------------分布图（前300m）

# 1. 导入必要的包
library(ggplot2)

# 2. 读取CSV文件
data <- read.csv("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_distribution.csv")

# 3. 绘制柱状图并叠加趋势线
ggplot(data, aes(x = distance, y = area)) +  
  geom_bar(stat = "identity", fill = "#66a3a5", width = 9, alpha = 0.6) +  
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +  
  labs(
    x = "Distance to Edge (m)",
    y = bquote(bold("FE Area (× 10"^10 * " m"^2 * ")"))  # 确保加粗
  ) +
  theme_minimal(base_size = 30) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
    axis.title.x = element_text(face = "bold", size = 30),  # X轴标题加粗
    axis.title.y = element_text(face = "bold", size = 30),  # Y轴标题加粗
    axis.text.x = element_text(color = "black", size = 30),  # X轴刻度文本颜色为黑色且加粗
    axis.text.y = element_text(color = "black", size = 30),  # Y轴刻度文本颜色为黑色且加粗, face = "bold"
    panel.grid.major = element_blank(),  # 去除主要网格线
    panel.grid.minor = element_blank(),  # 去除次要网格线
    axis.line = element_line(color = "black", size = 0.8),  # 添加坐标轴线
    axis.ticks = element_line(color = "black", size = 0.8),  # 添加坐标轴刻度线
    legend.position = "none",  # 去除图例
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # 添加黑色外框线
  )

# 4. 保存图表为PNG文件
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_300m_2023.png",
       width = 12, height = 12, dpi = 1000,bg = "white")


# ------------------------------------------------------------------------------累积分布折线图
# 加载必要的包
library(ggplot2)
library(dplyr)
library(scales)  # 用于百分比格式化
library(viridis) # 更美观的颜色

# 读取 CSV 文件
data <- read.csv("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_Percent.csv")  

# 确保列名正确
colnames(data) <- c("area", "distance")

# 计算百分比和累积分布
data <- data %>%
  mutate(
    area_percent = area / sum(area),  # 计算每个区域的百分比
    cum_area = cumsum(area_percent)  # 计算累积百分比
  )

# 绘图
ggplot(data, aes(x = factor(distance, levels = distance), fill = as.factor(distance))) +  
  # 柱状图显示百分比（绿色色调）
  geom_bar(aes(y = area_percent), stat = "identity", alpha = 0.8) +  
  scale_fill_viridis_d(option = "viridis") +  
  
  # 绘制直线，并在每个折点处添加点
  geom_line(aes(y = cum_area), color = "#1b7837", size = 1.2, group = 1) +  # 直线连接
  geom_point(aes(y = cum_area), color = "#1b7837", size = 0.8) +  # 折点
  
  # 设置 Y 轴为百分比
  scale_y_continuous(
    name = "Proportion of forest",  # 左侧纵轴：柱状图百分比
    labels = percent  # 使左侧纵轴显示百分比
  ) +
  
  # 标题 & 主题优化
  labs(
    x = "Distance to Edge (m)"  # X轴标签
  ) +
  theme_minimal(base_size = 30) +  # 使用简洁的主题，调整字体大小
  theme(
    panel.grid = element_blank(),  # 移除背景网格线
    axis.line = element_line(color = "black", size = 1),  # 添加XY轴线
    axis.ticks = element_line(color = "black", size = 0.8),  # 调整刻度线样式
    axis.ticks.length = unit(0.25, "cm"),  # 刻度线长度
    axis.text = element_text(color = "black"),  # 让XY轴坐标刻度文字变黑
    axis.text.x = element_text(angle = 45, hjust = 1),  # 旋转X轴标签，避免重叠
    plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中加粗
    axis.title.x = element_text(face = "bold"),  # 横坐标标题加粗
    axis.title.y = element_text(face = "bold"),  # 纵坐标标题加粗
    legend.position = "none"  # 去掉图例
  )

# 5. 保存图表为PNG文件
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_Percent2018.png",
       width = 12, height = 10, dpi = 1000,bg = "white")


