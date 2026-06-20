# ------------------------------------------------------------------------------region每100m面积分布
# 1. 导入必要的包
library(ggplot2)

# 2. 读取CSV文件
data <- read.csv("D:/Forest_Fragmentation/Picture/FE_area/region_distribution.csv")

# 3. 按CSV原始顺序设置 distance 为有序因子
data$distance <- factor(data$distance, levels = unique(data$distance))

# 4. 绘图（淡粉色柱状图，带边框）"#8b4513"/"#6b8e23"
p <- ggplot(data, aes(x = distance, y = area)) +  
  geom_bar(stat = "identity", fill ="#8b4513", width = 0.6, alpha = 0.6) +  
  geom_errorbar(aes(ymin = area - sd, ymax = area + sd), width = 0.2, size = 1) +  # 添加误差棒
  labs(
    x = "Distance to Edge (m)",
    y = bquote(bold("FE Area (× 10"^10 * " m"^2 * ")"))
  ) +
  theme_minimal(base_size = 30) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 30),
    axis.title.y = element_text(face = "bold", size = 30),
    axis.text.x = element_text(color = "black", size = 40, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 40),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  scale_y_continuous(limits = c(0, 10))  # 设置Y轴范围为0到10
# 显示图形
print(p)
# 5. 保存图表为PNG文件，设置背景为白色
ggsave("D:/Forest_Fragmentation/Picture/FE_area/NW_distribution.png",
       plot = p, width = 12, height = 12, dpi = 1000, bg = "white")

# ------------------------------------------------------------------------------region边缘类型分布
library(ggplot2)
library(dplyr)
library(tidyr)

# 读取CSV文件
file_path <- "D:/Forest_Fragmentation/Picture/FE_area/region_tape.csv"
data <- read.csv(file_path)

# 重命名列（如果需要）
colnames(data) <- c("region", "Anthropogenic_Edge", "sd1", "Natural_Edge", "sd2")

# 按原始顺序设置 region 为因子
data$region <- factor(data$region, levels = unique(data$region))

# 整理数据为长格式，同时整理标准差
plot_data <- data %>%
  pivot_longer(cols = c("Anthropogenic_Edge", "Natural_Edge"),
               names_to = "Type", values_to = "Value") %>%
  pivot_longer(cols = c("sd1", "sd2"),
               names_to = "SD_Type", values_to = "SD") %>%
  filter((Type == "Anthropogenic_Edge" & SD_Type == "sd1") |
           (Type == "Natural_Edge" & SD_Type == "sd2")) %>%
  mutate(
    Type = recode(Type,
                  "Anthropogenic_Edge" = "Anthropogenic Edge",
                  "Natural_Edge" = "Natural Edge")
  )

# 设置柱子位置偏移
dodge <- position_dodge(width = 0.8)

# 绘图
p <- ggplot(plot_data, aes(x = region, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black",size = 0.3,alpha = 0.8) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = dodge, width = 0.2, size = 0.6) +
  scale_y_continuous(limits = c(0, 2.5), expand = c(0, 0)) +
  scale_fill_manual(values = c("Anthropogenic Edge" = "#ADFF2F", "Natural Edge" = "#00008B")) +
  labs(
    y = "FE Area (million ha)",
    x = "Region",
    fill = "Type"
  ) +
  theme_minimal(base_size = 30) +
  theme(
    legend.position = "bottom",                       # 图例放底部
    legend.direction = "horizontal",                          # 横向排列
    legend.box.margin = margin(t = -20),                      # 减少图例与x轴之间的距离
    legend.margin = margin(b = -10),                          # 减少图例自身外边距
    axis.text.x = element_text(hjust = 0.5),# x轴刻度标签
    axis.text.y = element_text(color = "black"),      # y轴刻度标签
    axis.title = element_text(face = "bold"),
    axis.ticks.x = element_blank(),                                 # 去掉x轴刻度线
    axis.ticks.y = element_line(color = "black", size = 1),         # 保留y轴刻度线
    axis.ticks.length = unit(0.2, "cm"),                                 # 刻度线长度
    panel.grid = element_blank(),                     # 去除所有网格线
    axis.line.y = element_line(color = "black", size = 1),
    axis.line.x = element_line(color = "black", size = 1),
    panel.border = element_blank()
  )

# 显示图形
print(p)

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/FE_area/region_tape.png",
       plot = p, width = 8, height = 8, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------region边缘tape分布change
library(ggplot2)
library(dplyr)
library(tidyr)

# 读取CSV文件
file_path <- "D:/Forest_Fragmentation/Picture/FE_area/region_tape_change.csv"
data <- read.csv(file_path)

# 重命名列（如果需要）
colnames(data) <- c("region", "Anthropogenic_Edge",  "Natural_Edge")

# 设置 region 为因子，保留原始顺序
data$region <- factor(data$region, levels = unique(data$region))

# 整理为长格式，并去除 SD 相关列
plot_data <- data %>%
  pivot_longer(cols = c("Anthropogenic_Edge", "Natural_Edge"),
               names_to = "Type", values_to = "Value") %>%
  mutate(Type = recode(Type,
                       "Anthropogenic_Edge" = "Anthropogenic Edge",
                       "Natural_Edge" = "Natural Edge"))

# 设置柱子位置偏移
dodge <- position_dodge(width = 0.6)

# 绘图
p <- ggplot(plot_data, aes(x = region, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black",size = 0.3,alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 1) +  # 添加中线
  scale_y_continuous(
    limits = c(-0.05, 0.15),
    expand = c(0, 0),
    breaks = seq(-0.05, 0.15, by = 0.05),
    labels = scales::number_format(accuracy = 0.01)  # 保留1位小数
  ) +
  scale_fill_manual(values = c("Anthropogenic Edge" = "#ADFF2F", "Natural Edge" = "#00008B")) +
  labs(
    y = "FE Area (million ha)",
    x = "Region",
    fill = "Type"
  ) +
  theme_minimal(base_size = 30) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.margin = margin(t = -20),
    legend.margin = margin(b = -10),
    axis.text.x = element_text(hjust = 0.5),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid = element_blank(),
    axis.line.y = element_line(color = "black", size = 1),
    axis.line.x = element_line(color = "black", size = 1),
    panel.border = element_blank()
  )

# 显示图形
print(p)

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/FE_area/region_tape_change1.0.png",
       plot = p, width = 8, height = 8, dpi = 600, bg = "white")


