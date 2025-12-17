# NDVI稳定性分析----------------------------------------------------CV
# 加载必要的库
library(ggplot2)

# 导入CSV文件 (根据文件路径修改)
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_Hum.csv")  # 假设文件名为 "ndvi_data.csv"

# # 计算变异系数 (CV)
# calculate_cv <- function(ndvi_values) {
#   return(sd(ndvi_values) / mean(ndvi_values) * 100)
# }
calculate_cv <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma_p <- sqrt(mean((x - mu)^2, na.rm = TRUE))  # 总体标准差
  return(sigma_p / mu * 100)
}


# 计算每个距离梯度的 CV，去掉年份列 (假设数据从第二列开始)
cv_values <- apply(ndvi_data[, -1], 2, calculate_cv)  # 去掉年份列

# 自定义的距离序列：10到100步长为10，100到1000步长为100
distance_values <- c(seq(10, 300, by = 10), seq(400, 1000, by = 100))

# 检查距离值的数量是否和cv_values一致
if(length(distance_values) != length(cv_values)) {
  stop("距离值数量和CV值数量不匹配！")
}

# 生成 CV 数据框
cv_df <- data.frame(
  distance = distance_values,  # 使用手动设置的距离序列
  cv = cv_values
)


# 画图
cv_plot <- ggplot(cv_df, aes(x = distance, y = cv)) +
# geom_line(color = "black", size = 1) +  # 黑色曲线，专业简洁
# geom_point(shape = 21, size = 3, fill = "#0072B5", color = "black", stroke = 1.2) +  # 蓝色填充，黑色边框点
  geom_smooth(method = "loess", se = TRUE, color = "#00aae3",  fill = "#00aae3",linewidth = 1.5,alpha = 0.2) +  # 平滑曲线
  labs(
    x = "Distance (m)",
    y = "Coefficient of Variation (CV, %)"
  ) +
  theme_classic() +  # 经典期刊风格
  theme(
    text = element_text(size = 30, family = "serif"),  # 统一字体风格，serif 更符合期刊要求
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # 加粗标题并居中
    axis.title = element_text(size = 30, face = "bold"),  # x/y 轴标签加粗
    axis.text = element_text(size = 30),  # 坐标轴刻度字体
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加外框线
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed")  # y 轴方向虚线网格
  )

# 显示图形
print(cv_plot)

# 高质量导出图片（透明背景）
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_CV_Distance.png",
      plot = cv_plot, width = 12, height = 10, dpi = 1000, bg = "white")
# CV值加上置信区间--------------------------------------------------------------
# 加载必要的包
library(ggplot2)
library(boot)
library(dplyr)

# 读取数据
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv.csv")

# 自定义 bootstrap CV 函数
get_cv_ci <- function(values, R = 500) {
  boot_fn <- function(data, idx) {
    sd(data[idx]) / mean(data[idx]) * 100
  }
  b <- boot(values, boot_fn, R = R)
  ci <- boot.ci(b, type = "perc")
  return(c(cv = mean(b$t), lower = ci$percent[4], upper = ci$percent[5]))
}

# 自定义距离序列（确保跟你的列数一致）
distance_values <- c(seq(10, 300, by = 10), seq(400, 1000, by = 100))

# 验证列数一致
if(ncol(ndvi_data) - 1 != length(distance_values)) stop("距离值和列数不匹配！")

# 逐列计算 CV + CI（去掉第一列年份）
cv_results <- t(apply(ndvi_data[, -1], 2, get_cv_ci))  # 转置后变成 [distance, 3列]
cv_df <- as.data.frame(cv_results)
cv_df$Distance <- distance_values
# 绘图
cv_plot <- ggplot(cv_df, aes(x = Distance, y = cv)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#A6CEE3", alpha = 0.4) +  # 置信区间
  geom_line(color = "#1F78B4", size = 1.5) +  # 主曲线（蓝色）
  geom_point(color = "#1F78B4", size = 3, shape = 16) +  # 数据点
  labs(
    x = "Distance to Edge (m)",
    y = "Coefficient of Variation (CV, %)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 30, family = "serif"),
    axis.title = element_text(size = 32, face = "bold"),
    axis.text = element_text(size = 28),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed")
  )
# 显示图形
print(cv_plot)
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_CV_Distance_CI.png",
       plot = cv_plot, width = 12, height = 10, dpi = 1000, bg = "white")


# 计算H指数---------------------------------------------------------------------
# 加载必要的库
library(ggplot2)
library(pracma)  # 使用 pracma 包中的 hurstexp 函数

# 导入CSV文件 (根据文件路径修改)
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_Nat.csv")  # 假设文件名为 "ndvi_data.csv"

# 计算 Hurst 指数
calculate_hurst <- function(ndvi_values) {
  hurst_value <- hurstexp(ndvi_values)  # 使用 pracma 包计算 Hurst 指数
  return(hurst_value)  # 返回的是一个数值
}

# 获取数据的列数，去掉年份列
num_columns <- ncol(ndvi_data) - 1  # 去掉第一列的年份列

# 根据数据列数调整距离值序列
distance_values <- c(seq(10, 300, by = 10), seq(400, 1000, by = 100))[1:num_columns]

# 打印调整后的 distance_values
print(distance_values)

# 计算每个距离梯度的 Hurst 指数，去掉年份列 (假设数据从第二列开始)
hurst_values <- apply(ndvi_data[, -1], 2, calculate_hurst)  # 去掉年份列，计算每列的 Hurst 指数

# 检查 Hurst 指数
print(hurst_values)

# 生成 Hurst 数据框
hurst_df <- data.frame(
  distance = distance_values,  # 使用手动设置的距离序列
  hurst = hurst_values
)

# 检查数据框结构
str(hurst_df)  # 查看数据框结构
# 导出 Hurst 指数结果为 CSV 文件
write.csv(hurst_df, file = "D:/Forest_Fragmentation/Picture/R_NDVI/Hurst_ResultNat.csv", row.names = FALSE)

# H指数作图----------------------------------------------------------------------Nat.Hum
# 加载必要的库
library(ggplot2)

# 导入CSV文件 (根据文件路径修改)
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Hce_Nat_Hum.csv", header = FALSE)  # 读取数据，不设置列名

# 提取第一行作为距离，第二行作为 Hurst 指数
distance <- as.numeric(ndvi_data[1, -1])  # 第一行代表距离，去掉第一列
hurst <- as.numeric(ndvi_data[2, -1])     # 第二行代表 Hurst 指数，去掉第一列

# 创建数据框
hurst_df <- data.frame(
  distance = distance,  # 距离
  hurst = hurst         # Hurst 指数
)

# 画图
hurst_plot <- ggplot(hurst_df, aes(x = distance, y = hurst)) +
  geom_line(color = "black", size = 1) +  # 黑色曲线，专业简洁
  geom_point(shape = 21, size = 3, fill = "#0072B2", color = "black", stroke = 1.0) +  # 蓝色填充，黑色边框点
  coord_cartesian(ylim = c(0.45, 0.75)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.45, 0.75, by = 0.10))+
  labs() +
  theme_classic() +  # 经典期刊风格
  theme(
    text = element_text(size = 30),  # 统一字体风格，serif 更符合期刊要求, family = "serif"
    axis.ticks = element_line(color = "black", size = 1),    # 坐标刻度线颜色和粗细
    axis.ticks.length = unit(0.2, "cm"), 
    axis.title = element_blank(),#element_text(size = 30, face = "bold"),  # x/y 轴标签加粗
    axis.text = element_text(size = 30,color = "black"),  # 坐标轴刻度字体
    panel.grid.major = element_blank(),   #element_line(color = "gray80", size = 0.2),  # 主网格线颜色和细度
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # 黑色边框，线宽1.2
  )

# 显示图形
print(hurst_plot)

# 高质量导出图片（透明背景）
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Hce_all.png",
       plot = hurst_plot, width = 10, height = 9, dpi = 600, bg = "transparent")

# H指数作图----------------------------------------------------------------------Nat
# 加载必要的库
library(ggplot2)

# 导入CSV文件 (根据文件路径修改)
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Hce_Nat.csv", header = FALSE)  # 读取数据，不设置列名

# 提取第一行作为距离，第二行作为 Hurst 指数
distance <- as.numeric(ndvi_data[1, -1])  # 第一行代表距离，去掉第一列
hurst <- as.numeric(ndvi_data[2, -1])     # 第二行代表 Hurst 指数，去掉第一列

# 创建数据框
hurst_df <- data.frame(
  distance = distance,  # 距离
  hurst = hurst         # Hurst 指数
)

# 画图
hurst_plot <- ggplot(hurst_df, aes(x = distance, y = hurst)) +
  geom_line(color = "black", size = 1) +  # 黑色曲线，专业简洁
  geom_point(shape = 21, size = 3, fill = "#00008B", color = "black", stroke = 1.0) +  # 蓝色填充，黑色边框点
  coord_cartesian(ylim = c(0.45, 0.75)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.45, 0.75, by = 0.10))+
  labs() +
  theme_classic() +  # 经典期刊风格
  theme(
    text = element_text(size = 30),  # 统一字体风格，serif 更符合期刊要求, family = "serif"
    axis.ticks = element_line(color = "black", size = 1),    # 坐标刻度线颜色和粗细
    axis.ticks.length = unit(0.2, "cm"), 
    axis.title = element_blank(),#element_text(size = 30, face = "bold"),  # x/y 轴标签加粗
    axis.text = element_text(size = 30,color = "black"),  # 坐标轴刻度字体
    panel.grid.major = element_blank(),   #element_line(color = "gray80", size = 0.2),  # 主网格线颜色和细度
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # 黑色边框，线宽1.2
  )

# 显示图形
print(hurst_plot)

# 高质量导出图片（透明背景）
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Hce_Nat.png",
       plot = hurst_plot, width = 10, height = 9, dpi = 600, bg = "transparent")
# H指数作图----------------------------------------------------------------------Hum
# 加载必要的库
library(ggplot2)

# 导入CSV文件 (根据文件路径修改)
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Hce_Hum.csv", header = FALSE)  # 读取数据，不设置列名

# 提取第一行作为距离，第二行作为 Hurst 指数
distance <- as.numeric(ndvi_data[1, -1])  # 第一行代表距离，去掉第一列
hurst <- as.numeric(ndvi_data[2, -1])     # 第二行代表 Hurst 指数，去掉第一列

# 创建数据框
hurst_df <- data.frame(
  distance = distance,  # 距离
  hurst = hurst         # Hurst 指数
)

# 画图
hurst_plot <- ggplot(hurst_df, aes(x = distance, y = hurst)) +
  geom_line(color = "black", size = 1) +  # 黑色曲线，专业简洁
  geom_point(shape = 21, size = 3, fill = "#ADFF2F", color = "black", stroke = 1.0) +  # 蓝色填充，黑色边框点
  coord_cartesian(ylim = c(0.45, 0.75)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.45, 0.75, by = 0.10))+
  labs() +
  theme_classic() +  # 经典期刊风格
  theme(
    text = element_text(size = 30),  # 统一字体风格，serif 更符合期刊要求, family = "serif"
    axis.ticks = element_line(color = "black", size = 1),    # 坐标刻度线颜色和粗细
    axis.ticks.length = unit(0.2, "cm"), 
    axis.title = element_blank(),#element_text(size = 30, face = "bold"),  # x/y 轴标签加粗
    axis.text = element_text(size = 30,color = "black"),  # 坐标轴刻度字体
    panel.grid.major = element_blank(),   #element_line(color = "gray80", size = 0.2),  # 主网格线颜色和细度
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # 黑色边框，线宽1.2
  )

# 显示图形
print(hurst_plot)

# 高质量导出图片（透明背景）
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Hce_Hum.png",
       plot = hurst_plot, width = 10, height = 9, dpi = 600, bg = "transparent")

#  NDVI时空变化特征----------------------------------------------------------------------

library(ggplot2)
library(tidyr)

# 读取 CSV 数据，确保列名不被自动修改
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv.csv", check.names = FALSE, stringsAsFactors = FALSE)

# 数据重塑，使其适用于 ggplot2，并重命名 Distance 为 Distance (m)
ndvi_long <- pivot_longer(ndvi_data, cols = -year, names_to = "Distance", values_to = "NDVI") %>%
  rename(`Distance (m)` = Distance)  # 这里修改列名

# 确保 `Distance (m)` 为文本，并按照 CSV 读取顺序排列，同时在图例中添加单位 (m)
distance_levels <- unique(ndvi_long$`Distance (m)`)  # 记录原始顺序
ndvi_long <- ndvi_long %>%
  mutate(`Distance (m)` = factor(`Distance (m)`, levels = distance_levels, labels = paste0(distance_levels,"m")))

# # 定义存储拟合结果的列表
# fit_results <- list()
# # 对每个区域进行线性回归
# for(region in colnames(ndvi_data)[-1]) {
#   model <- lm(ndvi_data[[region]] ~ ndvi_data$year)
#   fit_results[[region]] <- summary(model)
# }
# # 显示拟合结果（回归方程 & R²值）
# cat("线性回归结果：\n")
# cat("===========================================\n")
# 
# for(region in names(fit_results)) {
#   slope <- round(fit_results[[region]]$coefficients[2, 1], 4)  # 斜率
#   intercept <- round(fit_results[[region]]$coefficients[1, 1], 4)  # 截距
#   r_squared <- round(fit_results[[region]]$r.squared, 4)  # R²值
#   
#   cat(region, ": y =", slope, "x +", intercept, ", R² =", r_squared, "\n")
# }
# 
# cat("===========================================\n")
# # 创建包含拟合方程和 R² 值的文本标签
# fit_labels <- sapply(names(fit_results), function(region) {
#   coeffs <- fit_results[[region]]$coefficients
#   equation <- paste("y =", round(coeffs[2, 1], 4), "x +", round(coeffs[1, 1], 4))
#   r_squared <- paste("R² =", round(fit_results[[region]]$r.squared, 4))
#   return(paste(equation, "\n", r_squared))
# })
# 
# # 设置文本标签的位置
# label_positions <- data.frame(
#   x = rep(max(ndvi_data$year), length(fit_labels)), 
#   y = seq(from = 0.65, to = 0.85, length.out = length(fit_labels)),  # 调整标签位置，防止重叠
#   label = fit_labels
# )

# 画折线图并添加拟合结果文本标签
# 手动定义19种颜色
custom_colors <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", 
  "#f5a623", "#8b0000", "#00008b", "#32cd32", "#ff6347", 
  "#dda0dd", "#ff1493", "#f0e68c", "#98fb98", "#ffff00"
)

# 使用自定义颜色
plot <- ggplot(ndvi_long, aes(x = year, y = NDVI, color = `Distance (m)`)) +
  geom_line(aes(linetype = `Distance (m)`), size = 1.2) +  # 改善线条样式：调整线宽
  geom_point(shape = 24, size = 2, fill = "white", color = "black") +  # 数据点样式
  labs(
    title = "2018-2023年NDVI随时间变化趋势", 
    x = "Year", 
    y = "NDVI"
  ) +
  theme_classic() +  # 使用经典期刊风格
  theme(
    text = element_text(size = 16, family = "serif"),  # 设置字体风格
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题加粗并居中
    axis.title = element_text(size = 16, face = "bold"),  # x/y 轴标题加粗
    axis.text = element_text(size = 14),  # 坐标轴刻度字体大小
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 外框线
    legend.position = "top",  # 图例位置
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed")  # y 轴方向虚线网格
  ) +
  scale_color_manual(values = custom_colors) +  # 使用自定义颜色
  scale_linetype_manual(values = rep("solid", 19)) +  # 使用统一的线型
  ylim(0.65, 0.85) +  # 设置 y 轴范围
  # 调整图例分为 10 列
  guides(color = guide_legend(ncol = 10)) 
  # # 添加拟合结果文本标签
  # + geom_text(data = label_positions, aes(x = x, y = y, label = label), 
  #           color = "black", size = 3, hjust = 2, fontface = "italic")

# 导出图形到指定路径
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_change_trend.png", 
       plot = plot, width = 10, height = 8, dpi = 600)

# ------------------------------------------------------------------------------cv值计算
# 加载必要的库
library(ggplot2)

# 导入CSV文件 (根据文件路径修改)
ndvi_data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv.csv")  # 假设文件名为 "ndvi_data.csv"

# 计算变异系数 (CV)
calculate_cv <- function(ndvi_values) {
  return(sd(ndvi_values) / mean(ndvi_values) * 100)
}

# 计算每个距离梯度的 CV，去掉年份列 (假设数据从第二列开始)
cv_values <- apply(ndvi_data[, -1], 2, calculate_cv)  # 去掉年份列

# 自定义的距离序列：10到100步长为10，100到1000步长为100
distance_values <- c(seq(10, 300, by = 10), seq(400, 1000, by = 100))

# 检查距离值的数量是否和cv_values一致
if(length(distance_values) != length(cv_values)) {
  stop("距离值数量和CV值数量不匹配！")
}

# 生成 CV 数据框
cv_df <- data.frame(
  distance = distance_values,  # 使用手动设置的距离序列
  cv = cv_values
)


# 画图
cv_plot <- ggplot(cv_df, aes(x = distance, y = cv)) +
   geom_line(color = "black", size = 1) +  # 黑色曲线，专业简洁
   geom_point(shape = 21, size = 3, fill = "#0072B5", color = "black", stroke = 1.2) +  # 蓝色填充，黑色边框点
  labs(
    x = "Distance (m)",
    y = "Coefficient of Variation (CV, %)"
  ) +
  theme_classic() +  # 经典期刊风格
  theme(
    text = element_text(size = 30, family = "serif"),  # 统一字体风格，serif 更符合期刊要求
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # 加粗标题并居中
    axis.title = element_text(size = 30, face = "bold"),  # x/y 轴标签加粗
    axis.text = element_text(size = 30),  # 坐标轴刻度字体
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加外框线
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed")  # y 轴方向虚线网格
  )

# 显示图形
print(cv_plot)

# 高质量导出图片（透明背景）
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_CV_Distance.png",
       plot = cv_plot, width = 10, height = 10, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------cv饱和点95%（真实值）
library(minpack.lm)
library(ggplot2)

dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_raw.csv", header = TRUE)
dat <- dat[order(dat$Distance), ]

CV_start <- dat$CV[1]      # 起始CV值（最小距离点）
CV_end <- dat$CV[nrow(dat)] # 最后一个距离点的CV值，近似稳定值

V0_approx <- CV_start - CV_end  # 近似幅度差

threshold <- CV_end + 0.05 * V0_approx  # 95%稳定点阈值（从起始值下降5%以内）

# 找到第一个满足CV <= threshold的距离
saturation_index <- which(dat$CV <= threshold)[1]

if (!is.na(saturation_index)) {
  saturation_distance <- dat$Distance[saturation_index]
  cat("根据数据计算的CV下降趋势稳定点（约95%稳定）在Distance =", saturation_distance, "m\n")
} else {
  cat("数据中未找到满足95%稳定条件的点\n")
}

# ------------------------------------------------------------------------------cv饱和点95%（拟合后）
library(minpack.lm)
library(ggplot2)

dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_raw.csv", header = TRUE)

dat <- dat[order(dat$Distance), ]

# 负指数衰减模型拟合
# CV = V0 * exp(-k * Distance) + C
model <- nlsLM(CV ~ V0 * exp(-k * Distance) + C,
               data = dat,
               start = list(V0 = max(dat$CV) - min(dat$CV), 
                            k = 0.01, 
                            C = min(dat$CV)),
               lower = c(0, 0, 0),
               upper = c(Inf, Inf, Inf))

summary(model)

# 参数提取
params <- coef(model)
V0 <- params["V0"]
k <- params["k"]
C <- params["C"]
coef(model)

model_summary <- summary(model)   # 先赋值保存模型摘要
print(model_summary)              # 可选，查看结果

estimates <- coef(model)          
std_errors <- model_summary$parameters[, "Std. Error"]  # 这一步需要 model_summary 变量

t_values <- estimates / std_errors

df <- nrow(dat) - length(estimates)

p_values <- 2 * pt(-abs(t_values), df)

results <- data.frame(
  Estimate = estimates,
  StdError = std_errors,
  t_value = t_values,
  p_value = p_values
)

print(results)

saturation_distance <- -log(0.05) / k

cat("CV下降趋势稳定点（达到95%稳定）在Distance =", round(saturation_distance, 2), "m\n")

library(ggplot2)

# 生成预测数据，长度也可调整
dat_new <- data.frame(Distance = seq(min(dat$Distance), max(dat$Distance), length.out = 500))
dat_new$CV_pred <- predict(model, newdata = dat_new)

cv_plot <- ggplot(dat, aes(x = Distance, y = CV)) +
  geom_point(size = 3) +  # 点稍大一些更清晰
  geom_line(data = dat_new, aes(x = Distance, y = CV_pred), color = "red", size = 1.5) +
  geom_vline(xintercept = saturation_distance, linetype = "dashed", color = "blue", size = 1) +
  labs(
    x = "Distance (m)",
    y = "CV(%)"
  ) +
  theme_minimal(base_size = 30) +  # 设置基础字体大小为30
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 30),
    axis.title = element_text(size = 30, face = "bold"),
    axis.ticks = element_line(color = "black", size = 1),    # 坐标刻度线颜色和粗细
    axis.ticks.length = unit(0.2, "cm"), 
    panel.grid.major = element_blank(),   #element_line(color = "gray80", size = 0.2),  # 主网格线颜色和细度
    panel.grid.minor = element_blank(),   # 关闭次网格线，避免太密
    axis.text = element_text(size = 30),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # 黑色边框，线宽1.2
  )

# 显示绘图
print(cv_plot)

# 高质量导出，宽高你可以调整
ggsave(filename = "D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_raw.png",
       plot = cv_plot,
       width = 10, height = 10, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------cv饱和点做垂线（拟合后）
# 1. 准备工作
library(minpack.lm)
library(ggplot2)


# 2. 读数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_raw.csv", header = TRUE)

# 检查数据列是不是Distance和CV
str(dat)

# 3. 进行指数衰减拟合
model <- nlsLM(CV ~ V0 * exp(-k * Distance) + C,
               data = dat,
               start = list(V0 = max(dat$CV) - min(dat$CV), 
                            k = 0.01, 
                            C = min(dat$CV)),
               lower = c(0, 0, 0),
               upper = c(Inf, Inf, Inf))

# 检查拟合摘要
summary(model)

# 4. 预测拟合曲线
newdat <- data.frame(Distance = seq(min(dat$Distance), max(dat$Distance), length.out = 500))
newdat$CV_pred <- predict(model, newdata = newdat)

# 5. 计算每个拟合曲线上到起点-终点连线的垂直距离
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(numerator / denominator)
}

p1 <- c(min(newdat$Distance), newdat$CV_pred[1])  # 曲线起点
p2 <- c(max(newdat$Distance), newdat$CV_pred[nrow(newdat)])  # 曲线终点

newdat$dist_to_line <- mapply(point_to_line_dist,
                              x0 = newdat$Distance,
                              y0 = newdat$CV_pred,
                              MoreArgs = list(x1 = p1[1],
                                              y1 = p1[2],
                                              x2 = p2[1],
                                              y2 = p2[2]))

# 6. 找出垂直距离最大的点
idx_max <- which.max(newdat$dist_to_line)
break_point <- newdat$Distance[idx_max]
break_y <- newdat$CV_pred[idx_max]

cat("拟合曲线上垂直距离最大的点Distance为 :", break_point, "，CV为 :", break_y, "\n")

# 7. 渲染为图片（包括数据点、拟合曲线、直线）
ggplot(dat, aes(x = Distance, y = CV)) + 
  geom_point(size = 2, color = "darkgrey") + 
  # 渲染拟合曲线
  geom_line(data = newdat, aes(x = Distance, y = CV_pred), color = "darkblue", size = 1) + 
  # 渲染起点-终点直线
  geom_segment(aes(x = p1[1], y = p1[2],
                   xend = p2[1], yend = p2[2]),
               color = "blue", size = 1, linetype = "dashed") + 
  # 渲染垂直距离最大的点
  geom_point(aes(x = break_point, y = break_y), color = "red", size = 4) + 
  labs(title = paste("拟合曲线上垂直距离最大的点Distance =", round(break_point, 2)),
       x = "Distance", y = "CV") + 
  theme_minimal()
# ------------------------------------------------------------------------------cv饱和点做垂线 Hum.Nat
library(minpack.lm)
library(ggplot2)

# 读取数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_raw_Nat_Hum.csv", header = TRUE)

# 起点与终点
p1 <- c(dat$Distance[1], dat$CV[1])
p2 <- c(dat$Distance[nrow(dat)], dat$CV[nrow(dat)])

# 点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 计算距离
dat$dist_to_line <- mapply(point_to_line_dist,
                           x0 = dat$Distance, y0 = dat$CV,
                           MoreArgs = list(x1 = p1[1], y1 = p1[2],
                                           x2 = p2[1], y2 = p2[2]))

# 拐点索引与坐标
idx_max_dist <- which.max(dat$dist_to_line)
break_point <- dat$Distance[idx_max_dist]

cat("拐点对应的距离是:", break_point, "\n")

# 绘图（无垂线）
p <- ggplot(dat, aes(x = Distance, y = CV)) +
  geom_line(color = "black", size = 1) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#0072B2", color = "black", stroke = 1.0) +  # 蓝色点
  # 蓝色虚线（首尾连线）
  annotate("segment",
           x = p1[1], y = p1[2],
           xend = p2[1], yend = p2[2],
           color = "#D55E00", size = 1, linetype = "dashed") +
  geom_point(data = dat[idx_max_dist, , drop = FALSE],
             aes(x = Distance, y = CV), color = "red", size = 2.5) +     # 红色拐点
  #labs(x = "Distance to Edge (m)", y = "CV(%)") +
  coord_cartesian(ylim = c(0.8, 1.7)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.8, 1.7, by = 0.3))+
  labs() +
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    #axis.title = element_text(size = 30),
    axis.title = element_blank(),
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")
  )
# 显示图像
print(p)
# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_CV_Nat_Hum(tpoint).png",
       plot = p, width = 10, height = 9, dpi = 600)
# ------------------------------------------------------------------------------cv饱和点做垂线 Nat
library(minpack.lm)
library(ggplot2)

# 读取数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_Nat_raw.csv", header = TRUE)

# 起点与终点
p1 <- c(dat$Distance[1], dat$CV[1])
p2 <- c(dat$Distance[nrow(dat)], dat$CV[nrow(dat)])

# 点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 计算距离
dat$dist_to_line <- mapply(point_to_line_dist,
                           x0 = dat$Distance, y0 = dat$CV,
                           MoreArgs = list(x1 = p1[1], y1 = p1[2],
                                           x2 = p2[1], y2 = p2[2]))

# 拐点索引与坐标
idx_max_dist <- which.max(dat$dist_to_line)
break_point <- dat$Distance[idx_max_dist]

cat("拐点对应的距离是:", break_point, "\n")

# 绘图（无垂线）
p <- ggplot(dat, aes(x = Distance, y = CV)) +
  geom_line(color = "black", size = 1) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#00008B", color = "black", stroke = 1.0) +  # 蓝色点
  # 蓝色虚线（首尾连线）
  # annotate("segment",
  #          x = p1[1], y = p1[2],
  #          xend = p2[1], yend = p2[2],
  #          color = "#D55E00", size = 1, linetype = "dashed") +
  # geom_point(data = dat[idx_max_dist, , drop = FALSE],
  #            aes(x = Distance, y = CV), color = "red", size = 2.5) +     # 红色拐点
  #labs(x = "Distance to Edge (m)", y = "CV(%)") +
  coord_cartesian(ylim = c(0.8, 1.7)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.8, 1.7, by = 0.3))+
  labs() +
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    #axis.title = element_text(size = 30),
    axis.title = element_blank(),
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")
  )
# 显示图像
print(p)
# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_CV_Nat(tpoint无拐点).png",
       plot = p, width = 10, height = 9, dpi = 600)

# ------------------------------------------------------------------------------cv饱和点做垂线 Hum
library(minpack.lm)
library(ggplot2)

# 读取数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_cv_Hum_raw.csv", header = TRUE)

# 起点与终点
p1 <- c(dat$Distance[1], dat$CV[1])
p2 <- c(dat$Distance[nrow(dat)], dat$CV[nrow(dat)])

# 点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 计算距离
dat$dist_to_line <- mapply(point_to_line_dist,
                           x0 = dat$Distance, y0 = dat$CV,
                           MoreArgs = list(x1 = p1[1], y1 = p1[2],
                                           x2 = p2[1], y2 = p2[2]))

# 拐点索引与坐标
idx_max_dist <- which.max(dat$dist_to_line)
break_point <- dat$Distance[idx_max_dist]

cat("拐点对应的距离是:", break_point, "\n")

# 绘图（无垂线）
p <- ggplot(dat, aes(x = Distance, y = CV)) +
  geom_line(color = "black", size = 1) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#ADFF2F", color = "black", stroke = 1.0) +  # 蓝色点
  # 蓝色虚线（首尾连线）
  annotate("segment",
           x = p1[1], y = p1[2],
           xend = p2[1], yend = p2[2],
           color = "#D55E00", size = 1, linetype = "dashed") +
  geom_point(data = dat[idx_max_dist, , drop = FALSE],
             aes(x = Distance, y = CV), color = "red", size = 2.5) +     # 红色拐点
  #labs(x = "Distance to Edge (m)", y = "CV(%)") +
  coord_cartesian(ylim = c(0.8, 1.7)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.8, 1.7, by = 0.3))+
  labs() +
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    #axis.title = element_text(size = 30),
    axis.title = element_blank(),
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")
  )
# 显示图像
print(p)
# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_CV_Hum(tpoint).png",
       plot = p, width = 10, height = 9, dpi = 600)
# ------------------------------------------------------------------------------Stability饱和点做垂线 Hum.Nat
library(minpack.lm)
library(ggplot2)

# 读取数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Stability_raw_Nat_Hum.csv", header = TRUE)

# 起点与终点
p1 <- c(dat$Distance[1], dat$Stability[1])
p2 <- c(dat$Distance[nrow(dat)], dat$Stability[nrow(dat)])

lm_model <- lm(Stability ~ Distance, data = dat)
summary(lm_model)
# 提取系数
coef(summary(lm_model))

# 提取 R2
r2 <- summary(lm_model)$r.squared

# 提取 p 值（Distance）
p_value <- coef(summary(lm_model))["Distance", "Pr(>|t|)"]

# 输出
cat("R2 =", round(r2, 3), "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")

# 点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 计算距离
dat$dist_to_line <- mapply(point_to_line_dist,
                           x0 = dat$Distance, y0 = dat$Stability,
                           MoreArgs = list(x1 = p1[1], y1 = p1[2],
                                           x2 = p2[1], y2 = p2[2]))

# 拐点索引与坐标
idx_max_dist <- which.max(dat$dist_to_line)
break_point <- dat$Distance[idx_max_dist]

cat("拐点对应的距离是:", break_point, "\n")

# 绘图（无垂线）
p <- ggplot(dat, aes(x = Distance, y = Stability)) +
  geom_line(color = "black", size = 1) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#0072B2", color = "black", stroke = 1.0) +  # 蓝色点
  geom_smooth(
    method = "lm",
    se = TRUE,
    fill = "grey80",     # 更淡的灰色 SE
    alpha = 0.6,         # 阴影更轻
    color = "#F4A261",   # 柔和浅橙（不影响红色）
    linewidth = 0.5
  ) +
  # 蓝色虚线（首尾连线）
  annotate("segment",
           x = p1[1], y = p1[2],
           xend = p2[1], yend = p2[2],
           color ="red", size = 1, linetype = "dashed") +
  geom_point(data = dat[idx_max_dist, , drop = FALSE],
             aes(x = Distance, y = Stability), color = "red", size = 2.5) +     # 红色拐点
  #labs(x = "Distance to Edge (m)", y = "Stability(%)") +
  coord_cartesian(ylim = c(55, 125)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(55, 125, by = 20))+
  labs() +
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    #axis.title = element_text(size = 30),
    axis.title = element_blank(),
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")
  )
# 显示图像
print(p)
# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Stability_Nat_Hum(tpoint)_line.png",
       plot = p, width = 10, height = 9, dpi = 600)
# ------------------------------------------------------------------------------Stability饱和点做垂线 Nat
library(minpack.lm)
library(ggplot2)

# 读取数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Stability_Nat_raw.csv", header = TRUE)

# 起点与终点
p1 <- c(dat$Distance[1], dat$Stability[1])
p2 <- c(dat$Distance[nrow(dat)], dat$Stability[nrow(dat)])

lm_model <- lm(Stability ~ Distance, data = dat)
summary(lm_model)
# 提取系数
coef(summary(lm_model))

# 提取 R2
r2 <- summary(lm_model)$r.squared

# 提取 p 值（Distance）
p_value <- coef(summary(lm_model))["Distance", "Pr(>|t|)"]

# 输出
cat("R2 =", round(r2, 3), "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")

# 点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 计算距离
dat$dist_to_line <- mapply(point_to_line_dist,
                           x0 = dat$Distance, y0 = dat$Stability,
                           MoreArgs = list(x1 = p1[1], y1 = p1[2],
                                           x2 = p2[1], y2 = p2[2]))

# 拐点索引与坐标
idx_max_dist <- which.max(dat$dist_to_line)
break_point <- dat$Distance[idx_max_dist]

cat("拐点对应的距离是:", break_point, "\n")

# 绘图（无垂线）
p <- ggplot(dat, aes(x = Distance, y = Stability)) +
  geom_line(color = "black", size = 1) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#00008B", color = "black", stroke = 1.0) +  # 蓝色点
  geom_smooth(
    method = "lm",
    se = TRUE,
    fill = "grey80",     # 更淡的灰色 SE
    alpha = 0.6,         # 阴影更轻
    color = "#F4A261",   # 柔和浅橙（不影响红色）
    linewidth = 0.5,
    linetype = "dashed"  # ★ 虚线
  ) +
  # 蓝色虚线（首尾连线）
  # annotate("segment",
  #          x = p1[1], y = p1[2],
  #          xend = p2[1], yend = p2[2],
  #          color = "#D55E00", size = 1, linetype = "dashed") +
  # geom_point(data = dat[idx_max_dist, , drop = FALSE],
  #            aes(x = Distance, y = Stability), color = "red", size = 2.5) +     # 红色拐点
  #labs(x = "Distance to Edge (m)", y = "Stability(%)") +
  coord_cartesian(ylim = c(55, 125)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(55, 125, by = 20))+
  labs() +
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    #axis.title = element_text(size = 30),
    axis.title = element_blank(),
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")
  )
# 显示图像
print(p)
# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Stability_Nat(tpoint无拐点)_line.png",
       plot = p, width = 10, height = 9, dpi = 600)

# ------------------------------------------------------------------------------Stability饱和点做垂线 Hum
library(minpack.lm)
library(ggplot2)

# 读取数据
dat <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Stability_Hum_raw.csv", header = TRUE)

# 起点与终点
p1 <- c(dat$Distance[1], dat$Stability[1])
p2 <- c(dat$Distance[nrow(dat)], dat$Stability[nrow(dat)])

lm_model <- lm(Stability ~ Distance, data = dat)
summary(lm_model)
# 提取系数
coef(summary(lm_model))

# 提取 R2
r2 <- summary(lm_model)$r.squared

# 提取 p 值（Distance）
p_value <- coef(summary(lm_model))["Distance", "Pr(>|t|)"]

# 输出
cat("R2 =", round(r2, 3), "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")


# 点到线距离函数
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 计算距离
dat$dist_to_line <- mapply(point_to_line_dist,
                           x0 = dat$Distance, y0 = dat$Stability,
                           MoreArgs = list(x1 = p1[1], y1 = p1[2],
                                           x2 = p2[1], y2 = p2[2]))

# 拐点索引与坐标
idx_max_dist <- which.max(dat$dist_to_line)
break_point <- dat$Distance[idx_max_dist]

cat("拐点对应的距离是:", break_point, "\n")

# 绘图（无垂线）
p <- ggplot(dat, aes(x = Distance, y = Stability)) +
  geom_line(color = "black", size = 1) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#ADFF2F", color = "black", stroke = 1.0) +  # 蓝色点
  geom_smooth(
    method = "lm",
    se = TRUE,
    fill = "grey80",     # 更淡的灰色 SE
    alpha = 0.6,         # 阴影更轻
    color = "#F4A261",   # 柔和浅橙
    linewidth = 0.5
  ) +

  # 蓝色虚线（首尾连线）
  annotate("segment",
           x = p1[1], y = p1[2],
           xend = p2[1], yend = p2[2],
           color = "red", size = 1, linetype = "dashed") +
  geom_point(data = dat[idx_max_dist, , drop = FALSE],
             aes(x = Distance, y = Stability), color = "red", size = 2.5) +     # 红色拐点
  #labs(x = "Distance to Edge (m)", y = "Stability(%)") +
  coord_cartesian(ylim = c(55, 125)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(55, 125, by = 20))+
  labs() +
  theme_bw(base_size = 30) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    #axis.title = element_text(size = 30),
    axis.title = element_blank(),
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")
  )
# 显示图像
print(p)
# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_Stability_Hum(tpoint)_line.png",
       plot = p, width = 10, height = 9, dpi = 600)
