# ------------------------------------------------------------------------------10m和1000m NDVI差异显著性检验
# 导入必要的库
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

# 进行独立样本 t 检验（Welch's t-test）
t_test_result <- t.test(edge_NDVI, core_NDVI, var.equal = FALSE)

# 输出结果
cat("边缘区 NDVI: 平均值 =", round(edge_mean, 5), ", 标准差 =", round(edge_sd, 5), "\n")
cat("核心区 NDVI: 平均值 =", round(core_mean, 5), ", 标准差 =", round(core_sd, 5), "\n")
cat("T 检验 p 值 =", round(t_test_result$p.value, 5), "\n")

# 输出完整 t 检验结果
print(t_test_result)

# 创建数据框
data <- data.frame(
  NDVI = c(edge_NDVI, core_NDVI),
  Region = rep(c("Edge", "Core"), each = length(edge_NDVI))
)
ggplot(data, aes(x = Region, y = NDVI, color = Region)) + 
  geom_jitter(width = 0.2, height = 0, size = 4, stroke = 1, alpha = 0.6) +
  
  # 设置 y 轴范围、刻度间隔和标签格式
  scale_y_continuous(
    limits = c(0.66, 0.82),
    breaks = seq(0.66, 0.82, by = 0.04),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  theme_minimal(base_size = 30) + 
  
  theme(
    panel.background = element_rect(fill = "transparent", color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linetype = "dashed", linewidth = 0.5),
    
    # 字体
    axis.text = element_text(size = 30), 
    axis.title = element_text(size = 30, margin = margin(t = 15)),
    
    # ✅ 关键设置：添加刻度线与长度
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    
    # 图例隐藏
    legend.position = "none"
  ) +
  
  labs(x = "Region", y = "NDVI") + 
  scale_color_manual(values = c("Edge" = "blue", "Core" = "red"))

# 保存图片
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_t_test.png", width = 10, height = 9, dpi = 600)

edge_ndvi <- 0.69613
core_ndvi <- 0.8028
decline_percent <- ((core_ndvi - edge_ndvi) / core_ndvi) * 100
decline_percent

# --------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------NDVI增速分析
# 加载必要的库
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)

# 读取数据（确保列名正确）
data <- read_csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope.csv") %>%
  select(where(~ !all(is.na(.))))  # 仅保留非空列

# 确保列名正确解析
colnames(data)[1] <- "Year"  # 重新命名第一列

# 转换数据格式（从宽格式到长格式）
data_long <- data %>%
  pivot_longer(cols = -Year, names_to = "Distance", values_to = "Slope")

# 确保 Year 是因子，并按时间顺序排列
data_long$Year <- as.character(data_long$Year)  # 先转换为字符，避免因子问题

# 提取均值数据
mean_data <- data_long %>%
  filter(Year == "mean")

# 确保 mean 存在
print(mean_data)

# 重新定义 Year 为因子，确保 mean 被包含
data_long$Year <- factor(data_long$Year, levels = c(setdiff(unique(data_long$Year), "mean"), "mean"))

# 确保 Distance 变量按数值排序
data_long$Distance <- factor(data_long$Distance, levels = unique(data_long$Distance))
mean_data$Distance <- factor(mean_data$Distance, levels = unique(mean_data$Distance))

# 定义颜色和线型
line_styles <- c(
  "2018" = "dashed", "2019" = "dashed", "2020" = "dashed",
  "2021" = "dashed", "2022" = "dashed", "2023" = "dashed",
  "mean" = "solid"
)

line_colors <- c(
  "2018" = "#7570b3", "2019" = "#4daaf2", "2020" = "#33a02c",
  "2021" = "#ffed6f", "2022" = "#bf5b17", "2023" = "#fccde5",
  "mean" = "#ff0000"
)

# 绘制折线图
ggplot(data_long, aes(x = Distance, y = Slope, group = Year, color = Year, linetype = Year)) +
  geom_line(size = 0.8) +  
  # mean 单独绘制，确保 group 也是 mean
  geom_line(data = mean_data, aes(x = Distance, y = Slope, group = Year, linetype = Year, color = Year), size = 1.2) +
  scale_linetype_manual(values = line_styles) +
  scale_color_manual(values = line_colors) +
  labs(
    x = "Distance to Edge (m)",
    y = "NDVI Slope",
    linetype = "Year",
    color = "Year"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", size = 30),
    axis.text = element_text(size = 30),
    axis.text.x = element_text(angle = 45, hjust = 1),  # 调整横坐标文本为45°倾斜
    legend.position = "bottom",
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # 添加外框线
    legend.box = "horizontal"
  ) +
  guides(linetype = guide_legend(nrow = 2, byrow = TRUE))


# 保存图片
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope_plot.png", width = 12, height = 12, dpi = 600)

# ------------------------------------------------------------------------------非线性拟合曲线a、b、c均值和sd
# 定义数据
values <- c(0.1, 0.09, 0.09, 0.08, 0.09, 0.09)

# 计算均值
mean_value <- mean(values)

# 计算标准差
sd_value <- sd(values)

# 输出结果
cat("均值 =", mean_value, "\n")
cat("标准差 =", sd_value, "\n")

# 定义数据
values <- c(0.68, 0.7, 0.71, 0.71, 0.71, 0.71)

# 计算均值
mean_value <- mean(values)

# 计算标准差
sd_value <- sd(values)

# 输出结果
cat("均值 =", mean_value, "\n")
cat("标准差 =", sd_value, "\n")

# 定义数据
values <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01)

# 计算均值
mean_value <- mean(values)

# 计算标准差
sd_value <- sd(values)

# 输出结果
cat("均值 =", mean_value, "\n")
cat("标准差 =", sd_value, "\n")

# 定义数据
values <- c(0.951, 0.95, 0.944, 0.939, 0.951, 0.953)

# 计算均值
mean_value <- mean(values)

# 计算标准差
sd_value <- sd(values)

# 输出结果
cat("均值 =", mean_value, "\n")
cat("标准差 =", sd_value, "\n")

# ---------------------------------------------------------------------------衰减函数拟合slope真实值饱和点
# 1. 读取数据
library(readr)
library(ggplot2)

data <- read_csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope1.csv")

# 检查数据列是不是Distance和slope
str(data)

# 假设data已经读入，且排序了（如果没排序，先排序）
data <- data[order(data$Distance), ]

slope_start <- data$slope[1]  # 起始值
slope_end <- data$slope[nrow(data)]  # 稳定值（末尾点）

# 计算阈值（95%稳定点）
threshold <- slope_end + 0.05 * (slope_start - slope_end)

# 找到第一个slope小于等于阈值的距离
saturation_index <- which(data$slope <= threshold)[1]

if (!is.na(saturation_index)) {
  saturation_distance <- data$Distance[saturation_index]
  cat("根据数据计算的饱和点（95%稳定）距离为:", saturation_distance, "m\n")
} else {
  cat("数据中未找到达到95%稳定的点\n")
}

# ---------------------------------------------------------------------------衰减函数拟合
# 1. 读取数据
library(readr)
library(ggplot2)

data <- read_csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope1.csv")

# 检查数据列是不是Distance和slope
str(data)

# 2. 指数衰减拟合
model <- nls(slope ~ a * exp(-b * Distance) + c,
             data = data,
             start = list(a = max(data$slope), b = 0.001, c = min(data$slope))
)

# 检查拟合结果
summary(model)


# 从拟合模型中获取参数
coeff <- coef(model)
a <- coeff["a"]
b <- coeff["b"]
c <- coeff["c"]

# 95%饱和点：
Distance_95 <- -log(0.05) / b

Distance_95

# 3. 准备拟合曲线数据进行渲染
newdat <- data.frame(Distance = seq(min(data$Distance), max(data$Distance), length.out = 500))
newdat$slope_pred <- predict(model, newdata = newdat)

# 4. 渲染为散点 +拟合曲线
ggplot(data, aes(x = Distance, y = slope)) + 
  geom_point(size = 2, color = "darkgrey") + 
  geom_line(data = newdat, aes(x = Distance, y = slope_pred), color = "darkblue", size = 1) + 
  labs(x = "Distance (m)", y = "Slope", title = "指数衰减拟合") + 
  theme_minimal()

# ---------------------------------------------------------------------------做垂线（拟合后）
# 1. 读取数据
library(readr)
library(ggplot2)

# 读数据
data <- read_csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope1.csv")

# 检查数据列是不是Distance和slope
str(data)

# 2. 指数衰减拟合
model <- nls(slope ~ a * exp(-b * Distance) + c,
             data = data,
             start = list(a = max(data$slope), b = 0.001, c = min(data$slope)))

# 检查拟合摘要
summary(model)

# 3. 预测拟合曲线
newdat <- data.frame(Distance = seq(min(data$Distance), max(data$Distance), length.out = 500))
newdat$slope_pred <- predict(model, newdata = newdat)

# 4. 计算每个拟合曲线上到起点-终点连线的垂直距离
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(numerator / denominator)
}

p1 <- c(min(newdat$Distance), newdat$slope_pred[1])  # 曲线起点
p2 <- c(max(newdat$Distance), newdat$slope_pred[nrow(newdat)])  # 曲线终点

newdat$dist_to_line <- mapply(point_to_line_dist,
                              x0 = newdat$Distance,
                              y0 = newdat$slope_pred,
                              MoreArgs = list(x1 = p1[1],
                                              y1 = p1[2],
                                              x2 = p2[1],
                                              y2 = p2[2]))

# 5. 找出垂直距离最大的点
idx_max <- which.max(newdat$dist_to_line)
break_point <- newdat$Distance[idx_max]
break_y <- newdat$slope_pred[idx_max]

cat("拟合曲线上垂直距离最大的点Distance为:", break_point, "，Slope为", break_y, "\n")

# 6. 渲染为图片（包括拟合曲线、直线、数据）
ggplot(data, aes(x = Distance, y = slope)) + 
  geom_point(size = 2, color = "darkgrey") + 
  # 拟合曲线
  geom_line(data = newdat, aes(x = Distance, y = slope_pred), color = "darkblue", size = 1) + 
  # 拟合曲线起点-终点直线
  geom_segment(aes(x = p1[1], y = p1[2],
                   xend = p2[1], yend = p2[2]),
               color = "blue", size = 1, linetype = "dashed") + 
  # 垂直距离最大的点
  geom_point(aes(x = break_point, y = break_y), color = "red", size = 4) + 
  labs(title = paste("拟合曲线上垂直距离最大的点Distance =", round(break_point, 2)),
       x = "Distance", y = "Slope") + 
  theme_minimal()


# ---------------------------------------------------------------------------做垂线
library(readr)
library(ggplot2)

# 1. 读取数据
data <- read_csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope1.csv")

# 2. 获取首尾点
p1 <- c(data$Distance[1], data$slope[1])
p2 <- c(data$Distance[nrow(data)], data$slope[nrow(data)])

# 3. 计算点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 4. 计算所有点到首尾连线的距离
data$dist_to_line <- mapply(point_to_line_dist,
                            x0 = data$Distance,
                            y0 = data$slope,
                            MoreArgs = list(x1 = p1[1], y1 = p1[2], x2 = p2[1], y2 = p2[2]))

# 5. 找最大距离点
idx_max_dist <- which.max(data$dist_to_line)
break_point <- data$Distance[idx_max_dist]

cat("拐点对应的Distance是:", break_point, "\n")

# 6. 可视化
ggplot(data, aes(x = Distance, y = slope)) +
  geom_point() +
  geom_segment(aes(x = p1[1], y = p1[2], xend = p2[1], yend = p2[2]),
               color = "blue", size = 1) +
  geom_segment(aes(xend = Distance, yend = slope),
               data = data[idx_max_dist, , drop = FALSE],
               color = "red", size = 1) +
  geom_point(data = data[idx_max_dist, , drop = FALSE], aes(x = Distance, y = slope),
             color = "red", size = 3) +
  labs(title = paste("拐点距离:", round(break_point, 2)))

# ---------------------------------------------------------------------------小图，趋势线
library(readr)
library(ggplot2)

# 1. 读取数据
data <- read_csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope1.csv")

# 2. 绘图：散点 + 误差棒 + 平滑趋势线（loess）
p <- ggplot(data, aes(x = Distance, y = slope)) +
  geom_errorbar(aes(ymin = slope - sd, ymax = slope + sd),
                width = 8, color = "grey8", alpha = 1) +  # 误差棒
  geom_point(shape = 16, size = 2.5, color = "blue", alpha = 0.8) +  # 蓝色点
  geom_smooth(method = "loess", span = 0.6, se = FALSE,
              color = "red", size = 0.8, linetype = "solid") +  # 趋势线
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title = element_blank(),
    axis.text = element_text(size = 36, color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
print(p)
# 3. 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_slope_trendline.png",
       plot = p, width = 10, height = 10, dpi = 600)


# ------------------------------------------------------------------------------Nat.Hum NDVI差异显著性检验
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

# t 检验（Welch’s t-test，允许方差不等）
t_test_result <- t.test(nat_NDVI, hum_NDVI, var.equal = FALSE)

# 输出结果
cat("自然边缘 NDVI: 平均值 =", round(nat_mean, 5), ", 标准差 =", round(nat_sd, 5), "\n")
cat("人为边缘 NDVI: 平均值 =", round(hum_mean, 5), ", 标准差 =", round(hum_sd, 5), "\n")
cat("T 检验 p 值 =", round(t_test_result$p.value, 5), "\n")

print(t_test_result)

# 创建数据框用于作图
plot_data <- data.frame(
  NDVI = c(nat_NDVI, hum_NDVI),
  EdgeType = rep(c("Natural", "Anthropogenic"), each = length(nat_NDVI))
)

# 作图
p <- ggplot(plot_data, aes(x = EdgeType, y = NDVI, color = EdgeType)) + 
  geom_jitter(width = 0.2, height = 0, size = 4, stroke = 1, alpha = 0.6) + 
  scale_y_continuous(
    limits = c(0.66, 0.82),
    breaks = seq(0.66, 0.82, by = 0.04),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linetype = "dashed", linewidth = 0.5),
    axis.text = element_text(size = 30), 
    axis.title = element_text(size = 30, margin = margin(t = 15)),
    axis.ticks = element_line(color = "black", size = 1),         # ✅ 显式打开刻度线
    axis.ticks.length = unit(0.2, "cm")                           # ✅ 设置小短线长度
  )+ 
  labs(x = "Edge Type", y = "NDVI") + 
  scale_color_manual(values = c("Natural" = "#00008B", "Anthropogenic" = "#ADFF2F")) + 
  theme(legend.position = "none")

# 显示图像
print(p)

# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_EdgeType_final.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")
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

# ------------------------------------------------------------------------------Nat.Hum CV差异显著性检验
# 加载必要库
library(readr)
library(ggplot2)

# 读取数据
data_raw <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/CV_Nat_Hum.csv", header = TRUE)

# 提取 CV 值
# 转换为百分数
nat_CV <- data_raw[[1]] * 100  # 自然边缘
hum_CV <- data_raw[[2]] * 100  # 人为边缘


# 计算基本统计
nat_mean <- mean(nat_CV, na.rm = TRUE)
nat_sd <- sd(nat_CV, na.rm = TRUE)
hum_mean <- mean(hum_CV, na.rm = TRUE)
hum_sd <- sd(hum_CV, na.rm = TRUE)

# t 检验（Welch’s t-test，允许方差不等）
t_test_result <- t.test(nat_CV, hum_CV, var.equal = FALSE)

# 输出结果
cat("自然边缘 CV: 平均值 =", round(nat_mean, 2), "%, 标准差 =", round(nat_sd, 2), "%\n")
cat("人为边缘 CV: 平均值 =", round(hum_mean, 2), "%, 标准差 =", round(hum_sd, 2), "%\n")
cat("T 检验 p 值 =", round(t_test_result$p.value, 5), "\n")
print(t_test_result)

# 创建数据框用于作图
plot_data <- data.frame(
  CV = c(nat_CV, hum_CV),
  EdgeType = rep(c("Natural", "Anthropogenic"), each = length(nat_CV))
)

# ------------------------------------------------------------------------------Nat.Hum Hurst差异显著性检验
# 加载必要库
library(readr)
library(ggplot2)

# 读取数据
data_raw <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/H_Nat_Hum.csv", header = TRUE)

# 提取 H 值
nat_H <- data_raw[[1]]  # 自然边缘
hum_H <- data_raw[[2]]  # 人为边缘

# 计算基本统计
nat_mean <- mean(nat_H, na.rm = TRUE)
nat_sd <- sd(nat_H, na.rm = TRUE)
hum_mean <- mean(hum_H, na.rm = TRUE)
hum_sd <- sd(hum_H, na.rm = TRUE)

# t 检验（Welch’s t-test，允许方差不等）
t_test_result <- t.test(nat_H, hum_H, var.equal = FALSE)

# 输出结果
cat("自然边缘 H: 平均值 =", round(nat_mean, 5), ", 标准差 =", round(nat_sd, 5), "\n")
cat("人为边缘 H: 平均值 =", round(hum_mean, 5), ", 标准差 =", round(hum_sd, 5), "\n")
cat("T 检验 p 值 =", round(t_test_result$p.value, 5), "\n")
print(t_test_result)

# ------------------------------------------------------------------------------NDVI混合像元差异显著性检验SW为例
# 1. 读取 CSV 文件
file_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/混合像元显著性/NDVI_PD_mixtureSW.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# 查看前几行数据
head(df)

# 2. 检查列名是否正确
colnames(df)  # 确保有 "NDVI1" 和 "NDVI2"

# 3. 配对 t 检验
t_test_result <- t.test(df$NDVI1, df$NDVI2, paired = TRUE)

# 4. 查看结果
print(t_test_result)






