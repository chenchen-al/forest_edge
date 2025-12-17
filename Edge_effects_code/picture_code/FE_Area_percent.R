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
  
  # # 标题 & 主题优化
  # labs(
  #   x = "Distance to Edge (m)"  # X轴标签
  # ) +
  # theme_minimal(base_size = 30) +  # 使用简洁的主题，调整字体大小
  # theme(
  #   panel.grid = element_blank(),  # 移除背景网格线
  #   axis.line = element_line(color = "black", size = 1),  # 添加XY轴线
  #   axis.ticks = element_line(color = "black", size = 0.8),  # 调整刻度线样式
  #   axis.ticks.length = unit(0.25, "cm"),  # 刻度线长度
  #   axis.text = element_text(color = "black"),  # 让XY轴坐标刻度文字变黑
  #   axis.text.x = element_text(angle = 45, hjust = 1),  # 旋转X轴标签，避免重叠
  #   plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中加粗
  #   axis.title.x = element_text(face = "bold"),  # 横坐标标题加粗
  #   axis.title.y = element_text(face = "bold"),  # 纵坐标标题加粗
  #   legend.position = "none"  # 去掉图例
  # )
  labs(
    x = "Distance to Edge (m)"  # X轴标签
  ) +
  theme_minimal(base_size = 30) +  # 基础字体大小为30
  theme(
    panel.grid = element_blank(),  # 移除背景网格线
    axis.line = element_line(color = "black", size = 1),  # 添加XY轴线
    axis.ticks = element_line(color = "black", size = 0.8),  # 调整刻度线样式
    axis.ticks.length = unit(0.25, "cm"),  # 刻度线长度
    
    axis.text = element_text(color = "black", size = 30),  # 坐标刻度字体大小
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),  # X轴刻度字体大小+旋转
    
    axis.title.x = element_text(face = "bold", size = 30),  # X轴标题字体大小
    axis.title.y = element_text(face = "bold", size = 30),  # Y轴标题字体大小
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 30),  # 图标题大小
    legend.position = "none"  # 去掉图例
  )


# 5. 保存图表为PNG文件
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_Area_cbl2023.png",
       width = 12, height = 10, dpi = 600)

