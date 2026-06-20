# 10 m 和 1000 m NDVI 差异显著性检验（Mann–Whitney U / Wilcoxon rank sum test）
# --------------------------------------------------------------------------
library(ggplot2)

# 定义两列数据
edge_NDVI <- c(0.673954238366069, 0.693500917986595, 0.697161643104331, 
               0.701813534362224, 0.707148791388752, 0.703204837871722)
core_NDVI <- c(0.795582105711266, 0.794990282067891, 0.809877539626996, 
               0.79762037940727, 0.813402589708142, 0.80532545175688)

# 计算均值（Mean）和标准差（SD）
edge_mean <- mean(edge_NDVI)
edge_sd <- sd(edge_NDVI)
core_mean <- mean(core_NDVI)
core_sd <- sd(core_NDVI)

# Mann–Whitney U 检验 (Wilcoxon rank sum test)
wilcox_result <- wilcox.test(
  edge_NDVI, core_NDVI,
  paired = FALSE,    # 独立样本
  exact = TRUE,      # 小样本建议使用精确 p 值
  correct = TRUE,    # 连续性校正
  alternative = "two.sided" # 双侧检验
)

# 输出均值 ± SD
cat("边缘区 NDVI: 平均值 =", round(edge_mean, 5), ", 标准差 =", round(edge_sd, 5), "\n")
cat("核心区 NDVI: 平均值 =", round(core_mean, 5), ", 标准差 =", round(core_sd, 5), "\n")

# 输出 Mann–Whitney 检验结果
cat("Mann–Whitney U 检验 W =", wilcox_result$statistic, ", p 值 =", signif(wilcox_result$p.value, 3), "\n")

# ------------------------------------------------------------------------------10m和1000m NDVI差异平行坐标轴
library(ggplot2)

# 数据
edge_NDVI <- c(0.673954238366069, 0.693500917986595, 0.697161643104331, 
               0.701813534362224, 0.707148791388752, 0.703204837871722)
core_NDVI <- c(0.795582105711266, 0.794990282067891, 0.809877539626996, 
               0.79762037940727, 0.813402589708142, 0.80532545175688)
# 构造数据框
df <- data.frame(
  NDVI = c(edge_NDVI, core_NDVI),
  Type = rep(c("edge", "core"), each = length(edge_NDVI)),
  Year = rep(2018:2023, 2)
)
# 绘图
p <- ggplot(df, aes(x = Year, y = NDVI, color = Type)) +
  geom_point(size = 8) +
  scale_color_manual(values = c("edge" = "#D73027", "core" = "#1A9850")) +
  # 设置 y 轴范围、刻度间隔和标签格式
  scale_y_continuous(
    limits = c(0.66, 0.82),
    breaks = seq(0.66, 0.82, by = 0.04),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  coord_cartesian(ylim = c(min(df$NDVI- 0.01), max(df$NDVI) + 0.01)) + # 上下留白
  theme_minimal() +  # minimal 方便自定义边框
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_text(size = 32, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(size = 1, color = "black"),   # 刻度加粗
    panel.border = element_rect(color = "black", fill = NA, size = 1), # 外框
    axis.line = element_line(color = "black", size = 1)      # 内框线
  )

print(p)

# 保存图片
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_10vs1000m.png",
       plot = p, width = 10, height = 9, dpi = 600)
# ------------------------------------------------------------------------------Nat.Hum NDVI差异显著性检验Mann–Whitney U 检验（Wilcoxon 秩和检验）
# 加载必要库
library(readr)
library(ggplot2)

# 读取数据
data_raw <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Nat_Hum.csv", header = TRUE)

# 提取 NDVI 值
nat_NDVI <- data_raw[[1]]  # 自然边缘
hum_NDVI <- data_raw[[2]]  # 人为边缘

# 计算基本统计
nat_mean <- mean(nat_NDVI, na.rm = TRUE)
nat_sd <- sd(nat_NDVI, na.rm = TRUE)
hum_mean <- mean(hum_NDVI, na.rm = TRUE)
hum_sd <- sd(hum_NDVI, na.rm = TRUE)

cat("自然边缘 NDVI: 平均值 =", round(nat_mean, 5), ", 标准差 =", round(nat_sd, 5), "\n")
cat("人为边缘 NDVI: 平均值 =", round(hum_mean, 5), ", 标准差 =", round(hum_sd, 5), "\n")

# Mann-Whitney U 检验 (Wilcoxon rank sum test)
wilcox_result <- wilcox.test(
  nat_NDVI, hum_NDVI,
  paired = FALSE,    # 独立样本
  exact = TRUE,      # 小样本建议使用精确 p 值
  correct = TRUE,    # 连续性校正
  alternative = "two.sided" # 双侧检验
)

# 输出结果
print(wilcox_result)
# ------------------------------------------------------------------------------Nat.Hum NDVI差异显著平行坐标轴
# 加载必要库
library(ggplot2)
library(tidyr)
library(dplyr)

# 读取数据
data_raw <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Nat_Hum.csv", header = TRUE)

# 假设 CSV 三列分别是：Natural, Human, Distance
nat_NDVI <- data_raw[[1]]       # 自然边缘 NDVI
hum_NDVI <- data_raw[[2]]       # 人为边缘 NDVI
distance_full <- data_raw[[3]]  # 横坐标 Distance

# 构建数据框
df <- data.frame(
  Distance = distance_full,
  Natural  = nat_NDVI,
  Human    = hum_NDVI
)

# 转换为长格式
df_long <- df %>%
  pivot_longer(cols = c("Natural", "Human"), names_to = "Type", values_to = "NDVI")

# 绘图
p <- ggplot(df_long, aes(x = Distance, y = NDVI, color = Type)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6, size = 2) +
  scale_color_manual(values = c("Natural" = "#00008B", "Human" = "#ADFF2F")) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_text(size = 32, color = "black"),
    axis.text.x = element_text(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(size = 1, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black", size = 1)
  )

print(p)

# 保存图片
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_NatHum_0-1000.png",
       plot = p, width = 10, height = 9, dpi = 600)




