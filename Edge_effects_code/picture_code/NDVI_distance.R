# ------------------------------------------------------------------------------逐年函数拟合（sd做阴影）
# 1. 导入CSV文件
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD.csv")

# 2. 提取数据
y <- data[, 1]  # NDVI（纵坐标）
x <- data[, 2]  # FD（横坐标）
sd <- data[, 3] # 标准差

# 3. 设置绘图风格
library(ggplot2)
theme_set(theme_bw(base_size = 14))

# 4. 拟合指数衰减模型
fit_exp_decay <- nls(y ~ A * (1 - exp(-k * x)) + C,
                     start = list(A = 0.8, k = 0.01, C = min(y)))

# 拟合结果提取
summary_fit <- summary(fit_exp_decay)
p_val <- summary_fit$coefficients[2, 4]
p_text <- ifelse(p_val < 0.001, "p < 0.001", "p > 0.001")

# R²计算
fitted_y <- predict(fit_exp_decay)
SS_total <- sum((y - mean(y))^2)
SS_res <- sum((y - fitted_y)^2)
R_squared <- 1 - (SS_res / SS_total)

# 打印拟合函数和R²
cat("拟合函数：y = ", round(coef(fit_exp_decay)[1], 2),
    " * (1 - exp(-", round(coef(fit_exp_decay)[2], 2),
    " * x)) + ", round(coef(fit_exp_decay)[3], 2), "\n")
cat("p值：", p_text, "\n")
cat("R²：", round(R_squared, 3), "\n")

# 创建数据框
plot_df <- data.frame(x = x, y = y, sd = sd)

# 绘图
ggplot(plot_df, aes(x = x, y = y)) +
  # 灰色填充带表示标准差范围"grey80"
  geom_ribbon(aes(ymin = y - sd, ymax = y + sd), fill = "grey80", alpha = 0.6) +
  
  # 散点
  geom_point(shape = 16, size = 2.5, color = "#0072B2", alpha = 0.8) +
  
  # 拟合曲线"#D55E00"
  stat_function(fun = function(x) coef(fit_exp_decay)[1] * (1 - exp(-coef(fit_exp_decay)[2] * x)) + coef(fit_exp_decay)[3],
                color = "#D55E00", linetype = "dashed", linewidth = 1) +
  
  # 标签与样式(x = "Distance to edge (m)",  y = "NDVI") +
  labs(x = "", 
       y = "") +
  
  # 设置y轴范围为0.55到0.85，且间隔为0.075
  #scale_y_continuous(limits = c(0.55, 0.95), breaks = seq(0.55, 0.95, by = 0.1)) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # 外框线设置
    axis.text = element_text(color = "black", size = 30),
    axis.title = element_text(face = "bold", size = 30)
  )

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/Var.NDVI_with_FD(mean).png",
       width = 12, height = 10, dpi = 600)


# ------------------------------------------------------------------------------年平均真实值饱和点
# 1. 导入数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD.csv")

# 2. 提取数据
y <- data[, 1]  # NDVI（纵坐标）
x <- data[, 2]  # FD（横坐标）
sd <- data[, 3] # 标准备差

# 3. 计算稳定时的 95%
stable_max <- y[length(y)]
stable_95 <- 0.95 * stable_max

# 4. 找到对应的 FD
stable_index <- which(y >= stable_95)[1]
stable_FD <- x[stable_index]

# 5. 输出结果
cat("稳定时 NDVI 值为", stable_max, "，其中 95%为", stable_95, "到达时对应的 FD =", stable_FD, "。\n")

# 5. 渲染出数据、稳定线和 95% 线
library(ggplot2)

p <- ggplot(data, aes(x = x, y = y)) + 
  geom_point(color = "darkgreen") + 
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd), color = "darkgrey") + 
  geom_line(color = "darkgreen") + 
  geom_hline(yintercept = stable_max, color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = stable_95, color = "red", linetype = "dotted") + 
  geom_vline(xintercept = stable_FD, color = "red", linetype = "dotted") + 
  labs(title = "NDVI随着距离发生的稳定过程",
       x = "Distance to Edge (m)", 
       y = "NDVI") + 
  theme_minimal(base_size = 14)

print(p)


# ------------------------------------------------------------------------------年平均函数拟合（sd做bar）饱和点
# 1. 导入CSV文件
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD.csv")

# 2. 提取数据
y <- data[, 1]  # NDVI（纵坐标）
x <- data[, 2]  # FD（横坐标）
sd <- data[, 3] # 标准差

# 3. 设置绘图风格
library(ggplot2)
theme_set(theme_bw(base_size = 14))

# 4. 拟合指数衰减模型
fit_exp_decay <- nls(y ~ A * (1 - exp(-k * x)) + C,
                     start = list(A = 0.8, k = 0.01, C = min(y)))

# 拟合结果提取
summary_fit <- summary(fit_exp_decay)
p_val <- summary_fit$coefficients[2, 4]
p_text <- ifelse(p_val < 0.001, "p < 0.001", "p > 0.001")

# R²计算
fitted_y <- predict(fit_exp_decay)
SS_total <- sum((y - mean(y))^2)
SS_res <- sum((y - fitted_y)^2)
R_squared <- 1 - (SS_res / SS_total)

# 打印拟合函数和R²
cat("拟合函数：y = ", round(coef(fit_exp_decay)[1], 2),
    " * (1 - exp(-", round(coef(fit_exp_decay)[2], 2),
    " * x)) + ", round(coef(fit_exp_decay)[3], 2), "\n")
cat("p值：", p_text, "\n")
cat("R²：", round(R_squared, 3), "\n")
# 提取参数
params <- coef(fit_exp_decay)
A <- params["A"]
k <- params["k"]
C <- params["C"]

# 计算95%饱和点
saturation_point <- -log(0.05) / k

cat("模型的95%饱和点（x值）为:", round(saturation_point, 2), "\n")

# 创建数据框
plot_df <- data.frame(x = x, y = y, sd = sd)

# 绘图
ggplot(plot_df, aes(x = x, y = y)) +
  # 误差棒
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd),
                width = 8, color = "grey80", alpha = 1) + 
  
  # 散点"#0072B2"
  geom_point(shape = 16, size = 2.5, color = "#0072B2", alpha = 0.8) +
  
  # 拟合曲线"#D55E00"
  stat_function(fun = function(x) coef(fit_exp_decay)[1] * (1 - exp(-coef(fit_exp_decay)[2] * x)) + coef(fit_exp_decay)[3],
                color = "#D55E00", linetype = "dashed", linewidth = 1) +
  
  # 标签与样式(x = "Distance to edge (m)",  y = "NDVI") +
  labs(x = "", 
       y = "") +
  
  # 设置y轴范围为0.55到0.85，且间隔为0.075
  #scale_y_continuous(limits = c(0.55, 0.95), breaks = seq(0.55, 0.95, by = 0.1)) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # 外框线设置
    axis.text = element_text(color = "black", size = 30),
    axis.title = element_text(face = "bold", size = 30)
  )

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/Var.NDVI_with_FD(mean).png",
       width = 12, height = 10, dpi = 600)

# ------------------------------------------------------------------------------NDVI饱和点做垂线段（拟合后）
# 1. 导入数据
library(readr)
library(ggplot2)

# 读数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD.csv", header = TRUE)

# 检查数据列
str(data)

# 备用为数据框
y <- data[, 1]  # NDVI（纵坐标）
x <- data[, 2]  # Distance（横坐标）
sd <- data[, 3] # NDVI 的标准差

data <- data.frame(x = x, y = y, sd = sd)

# ---------------------------------------------------------------------------
# 2. 指数衰减拟合
fit_exp_decay <- nls(y ~ A * (1 - exp(-k * x)) + C,
                     data = data,
                     start = list(A = 0.8, k = 0.01, C = min(y)))

# 检查拟合结果
summary_fit <- summary(fit_exp_decay)
p_val <- summary_fit$coefficients[2, 4]
p_text <- ifelse(p_val < 0.001, "p < 0.001", "p > 0.001")

# R²计算
fitted_y <- predict(fit_exp_decay)
SS_total <- sum((y - mean(y))^2)
SS_res <- sum((y - fitted_y)^2)
R_squared <- 1 - (SS_res / SS_total)

# 打印拟合函数和R²
cat("拟合函数：y = ",
    round(coef(fit_exp_decay)[1], 2),
    " * (1 - exp(-", round(coef(fit_exp_decay)[2], 2),
    " * x )) + ",
    round(coef(fit_exp_decay)[3], 2), "\n")
cat("p 值：", p_text, "\n")
cat("R²：", round(R_squared, 3), "\n")

# ---------------------------------------------------------------------------
# 4. 准备用于渲染的数据（拟合）
newdat <- data.frame(x = seq(min(data$x), max(data$x), length.out = 500))
newdat$y <- predict(fit_exp_decay, newdata = newdat)

# ---------------------------------------------------------------------------
# 5. 计算每个拟合点到直线的距离（垂直距离）
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

p1 <- c(min(newdat$x), newdat$y[1])  # 直线起点
p2 <- c(max(newdat$x), newdat$y[nrow(newdat)])  # 直线终点

newdat$dist_to_line <- mapply(point_to_line_dist,
                              x0 = newdat$x,
                              y0 = newdat$y,
                              x1 = p1[1],
                              y1 = p1[2],
                              x2 = p2[1],
                              y2 = p2[2])

idx_max_dist <- which.max(newdat$dist_to_line)
break_point <- newdat$x[idx_max_dist]
break_y <- newdat$y[idx_max_dist]

# 单独准备断点数据框
break_data <- data.frame(
  x = break_point,
  y = break_y
)
print("拟合曲线到直线距离最大的断点:")
print(break_data)

# ---------------------------------------------------------------------------
# 渲染为图片（包括误差棒、拟合曲线）
ggplot(data, aes(x = x, y = y)) + 
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd), width = 0.5, color = "darkgrey") + 
  geom_point(size = 2, color = "darkgrey") + 
  # 这条是从第一个到最后一个点的线
  geom_segment(aes(x = p1[1], y = p1[2],
                   xend = p2[1], yend = p2[2]),
               color = "blue", size = 1) + 
  # 断点
  geom_point(data = break_data,
             aes(x = x, y = y),
             color = "red", size = 3) + 
  # 拟合曲线
  geom_line(data = newdat,
            aes(x = x, y = y),
            color = "darkgreen", size = 1) + 
  labs(title = paste("拟合曲线到直线距离最大的断点Distance =", round(break_point, 2)),
       x = "Distance", y = "NDVI") + 
  theme_minimal()

# ------------------------------------------------------------------------------NDVI饱和点做垂线段（全部Nat.&Hum）
library(readr)
library(ggplot2)

# 读取数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD.csv", header = TRUE)
x <- data[, 2]  # Distance
y <- data[, 1]  # NDVI
sd <- data[, 3] # SD

data <- data.frame(x = x, y = y, sd = sd)

# 线性回归
# =========================
lm_model <- lm(y ~ x, data = data)

# 回归摘要
summary(lm_model)

# =========================
# 提取关键统计量
# =========================
# R²
r2 <- summary(lm_model)$r.squared

# Distance 的 p 值
p_value <- coef(summary(lm_model))["x", "Pr(>|t|)"]

# 输出
cat("R² =", round(r2, 3), "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")

# 函数：计算点到线距离
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 找断点
p1 <- c(data$x[1], data$y[1])
p2 <- c(data$x[nrow(data)], data$y[nrow(data)])
data$dist_to_line <- mapply(point_to_line_dist,
                            x0 = data$x, y0 = data$y,
                            x1 = p1[1], y1 = p1[2],
                            x2 = p2[1], y2 = p2[2])
# === 加这两行 ===
idx_max_dist <- which.max(data$dist_to_line)
cat("拐点对应的 Distance 是:", data$x[idx_max_dist], "\n")
# 绘图
p <- ggplot(data, aes(x = x, y = y)) +
  # 误差棒
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd),
                width = 10, color = "grey80", alpha = 1.2) +
  geom_line(color = "black", size = 0.8) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#0072B2", color = "black", stroke = 1) +  # 蓝色点
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
           color = "red", size = 1, linetype = "dashed") +
  # turning point 红点
  geom_point(data = data[idx_max_dist, ], aes(x = x, y = y),
             color = "red", size = 2.5) +
  coord_cartesian(ylim = c(0.65, 0.85)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.65, 0.85, by = 0.05))+
  # 标签和主题设置
  labs() +
  theme_bw(base_size = 30) +  # 白色背景 + 统一字体
  theme(
    panel.grid = element_blank(),     # 无网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 黑色外框
    axis.title = element_blank(), 
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")  # 小刻度线
  )

# 显示图像
print(p)

# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_FD(tpoint)_line.png",
       plot = p,
       width = 10, height = 9, dpi = 600)

# ------------------------------------------------------------------------------NDVI饱和点做垂线段(分类Nat.)
library(readr)
library(ggplot2)

# 读取数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD_Nat.csv", header = TRUE)
x <- data[, 2]  # Distance
y <- data[, 1]  # NDVI
sd <- data[, 3] # SD

data <- data.frame(x = x, y = y, sd = sd)

# 线性回归
# =========================
lm_model <- lm(y ~ x, data = data)

# 回归摘要
summary(lm_model)

# =========================
# 提取关键统计量
# =========================
# R²
r2 <- summary(lm_model)$r.squared

# Distance 的 p 值
p_value <- coef(summary(lm_model))["x", "Pr(>|t|)"]

# 输出
cat("R² =", round(r2, 3), "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")

# 函数：计算点到线距离
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 找断点
p1 <- c(data$x[1], data$y[1])
p2 <- c(data$x[nrow(data)], data$y[nrow(data)])
data$dist_to_line <- mapply(point_to_line_dist,
                            x0 = data$x, y0 = data$y,
                            x1 = p1[1], y1 = p1[2],
                            x2 = p2[1], y2 = p2[2])
# === 加这两行 ===
idx_max_dist <- which.max(data$dist_to_line)
cat("拐点对应的 Distance 是:", data$x[idx_max_dist], "\n")
# 绘图
p <- ggplot(data, aes(x = x, y = y)) +
  # 误差棒
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd),
                width = 10, color = "grey80", alpha = 1.2) +
  geom_line(color = "black", size = 0.8) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#00008B", color = "black", stroke = 1) +  # 蓝色点
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
           color = "red", size = 1, linetype = "dashed") +
  # turning point 红点
  geom_point(data = data[idx_max_dist, ], aes(x = x, y = y),
             color = "red", size = 2.5) +
  coord_cartesian(ylim = c(0.65, 0.85)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.65, 0.85, by = 0.05))+
  # 标签和主题设置
  labs() +
  theme_bw(base_size = 30) +  # 白色背景 + 统一字体
  theme(
    panel.grid = element_blank(),     # 无网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 黑色外框
    axis.title = element_blank(), 
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")  # 小刻度线
  )

# 显示图像
print(p)

# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_FD_Nat(tpoint)_line.png",
       plot = p,
       width = 10, height = 9, dpi = 600)

# ------------------------------------------------------------------------------NDVI饱和点做垂线段(分类Hum.)
library(readr)
library(ggplot2)

# 读取数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_PD_Hum.csv", header = TRUE)
x <- data[, 2]  # Distance
y <- data[, 1]  # NDVI
sd <- data[, 3] # SD

data <- data.frame(x = x, y = y, sd = sd)

# 线性回归
# =========================
lm_model <- lm(y ~ x, data = data)

# 回归摘要
summary(lm_model)

# =========================
# 提取关键统计量
# =========================
# R²
r2 <- summary(lm_model)$r.squared

# Distance 的 p 值
p_value <- coef(summary(lm_model))["x", "Pr(>|t|)"]

# 输出
cat("R² =", round(r2, 3), "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")

# 函数：计算点到线距离
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 找断点
p1 <- c(data$x[1], data$y[1])
p2 <- c(data$x[nrow(data)], data$y[nrow(data)])
data$dist_to_line <- mapply(point_to_line_dist,
                            x0 = data$x, y0 = data$y,
                            x1 = p1[1], y1 = p1[2],
                            x2 = p2[1], y2 = p2[2])
# === 加这两行 ===
idx_max_dist <- which.max(data$dist_to_line)
cat("拐点对应的 Distance 是:", data$x[idx_max_dist], "\n")
# 绘图
p <- ggplot(data, aes(x = x, y = y)) +
  # 误差棒
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd),
                width = 10, color = "grey80", alpha = 1.2) +
  geom_line(color = "black", size = 0.8) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill =  "#ADFF2F", color = "black", stroke = 1) +  # 蓝色点
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
           color = "red", size = 1, linetype = "dashed") +
  # turning point 红点
  geom_point(data = data[idx_max_dist, ], aes(x = x, y = y),
             color = "red", size = 2.5) +
  coord_cartesian(ylim = c(0.65, 0.85)) +  # 限制 y 轴范围
  scale_y_continuous(breaks = seq(0.65, 0.85, by = 0.05))+
  # 标签和主题设置
  labs() +
  theme_bw(base_size = 30) +  # 白色背景 + 统一字体
  theme(
    panel.grid = element_blank(),     # 无网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 黑色外框
    axis.title = element_blank(), 
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")  # 小刻度线
  )

# 显示图像
print(p)

# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/NDVI_FD_Hum(tpoint)_line.png",
       plot = p,
       width = 10, height = 9, dpi = 600)


# ------------------------------------------------------------------------------NDVI\EVI\NIRv\Height饱和点做垂线段,敏感性分析（SW全部Nat.&Hum）
library(readr)
library(ggplot2)

# 读取数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/NIRv_PD_SW.csv", header = TRUE)
x <- data[, 2]  # Distance
y <- data[, 1]  # NDVI
sd <- data[, 3] # SD

data <- data.frame(x = x, y = y, sd = sd)

# 函数：计算点到线距离
point_to_line_dist <- function(x0, y0, x1, y1, x2, y2) {
  numerator <- abs((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))
  denominator <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  dist <- numerator / denominator
  return(dist)
}

# 找断点
p1 <- c(data$x[1], data$y[1])
p2 <- c(data$x[nrow(data)], data$y[nrow(data)])
data$dist_to_line <- mapply(point_to_line_dist,
                            x0 = data$x, y0 = data$y,
                            x1 = p1[1], y1 = p1[2],
                            x2 = p2[1], y2 = p2[2])
# === 加这两行 ===
idx_max_dist <- which.max(data$dist_to_line)
cat("拐点对应的 Distance 是:", data$x[idx_max_dist], "\n")
# 绘图
p <- ggplot(data, aes(x = x, y = y)) +
  # 误差棒
  geom_errorbar(aes(ymin = y - sd, ymax = y + sd),
                width = 10, color = "grey80", alpha = 1.2) +
  # # 误差阴影带（±sd）
  # geom_ribbon(aes(ymin = y - sd, ymax = y + sd),
  #             fill = "grey80", alpha = 0.6) +  # 半透明阴影
  geom_line(color = "black", size = 0.8) +  # 加上折线（蓝色线）
  geom_point(shape = 21, size = 3, fill = "yellow", color = "black", stroke = 1) +  # 蓝色点
  # 蓝色虚线（首尾连线）
  annotate("segment",
           x = p1[1], y = p1[2],
           xend = p2[1], yend = p2[2],
           color = "#D55E00", size = 1, linetype = "dashed") +
  # turning point 红点
  geom_point(data = data[idx_max_dist, ], aes(x = x, y = y),
             color = "red", size = 2.5) +
  # coord_cartesian(ylim = c(0.67, 0.82)) +  # 限制 y 轴范围
  # scale_y_continuous(breaks = seq(0.67, 0.82, by = 0.05))+
  # 标签和主题设置
  labs() +
  theme_bw(base_size = 30) +  # 白色背景 + 统一字体
  theme(
    panel.grid = element_blank(),     # 无网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 黑色外框
    axis.title = element_blank(), 
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")  # 小刻度线
  )

# 显示图像
print(p)

# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/NIRv_FD_SW(tpoint).png",
       plot = p,
       width = 10, height = 9, dpi = 600)
# ------------------------------------------------------------------------------NDVI\EVI\Height,敏感性分析（SW全部Nat.&Hum）
# 加载必要包
library(ggplot2)
library(tidyr)

# 读取 CSV 文件
file_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/PD_SW.csv"
df <- read.csv(file_path)

# 检查数据
head(df)

# 将数据从宽格式转换为长格式，方便 ggplot 绘图
df_long <- pivot_longer(df, cols = c("Height", "EVI", "NDVI"), 
                        names_to = "Index", values_to = "Value")
# 相关性分析
# -----------------------------

library(corrplot)
# 选择需要计算相关性的列
df_corr <- df %>% select(Height, EVI, NDVI)

# 计算相关系数矩阵
corr_matrix <- cor(df_corr, use = "complete.obs", method = "pearson")
print("两两相关性矩阵 (Pearson):")
print(round(corr_matrix, 4))
corrplot(corr_matrix, method = "number", type = "upper", tl.cex = 1, number.digits = 4)

# ------------------------------------------------------------------------------NDVI\EVI\Height,敏感性分析（SW全部Nat.&Hum）
library(ggplot2)
library(tidyr)
library(dplyr)

# 读取 CSV
file_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/PD_SW.csv"
df <- read.csv(file_path)

# 检查列名
colnames(df)
# 假设列名为: distance, Height, EVI, NDVI, NIRv_StdDev, Height_StdDev, NDVI_StdDev

# 将值和对应的 SD 都转换成长格式
df_long <- df %>%
  pivot_longer(cols = c("Height", "EVI", "NDVI"), names_to = "Index", values_to = "Value") %>%
  pivot_longer(cols = c("Height_StdDev", "EVI_StdDev", "NDVI_StdDev"),
               names_to = "Index_SD", values_to = "SD") %>%
  # 保证 Value 和 SD 对应
  filter(
    (Index == "Height" & Index_SD == "Height_StdDev") |
      (Index == "EVI" & Index_SD == "EVI_StdDev") |
      (Index == "NDVI" & Index_SD == "NDVI_StdDev")
  ) %>%
  select(-Index_SD)

# 对每个指数做 loess 平滑
smooth_df <- df_long %>%
  group_by(Index) %>%
  arrange(distance) %>%
  mutate(
    smoothed = predict(loess(Value ~ distance, span = 0.3)),
    ymin = smoothed - SD,
    ymax = smoothed + SD
  ) %>%
  ungroup()

p <- ggplot(smooth_df, aes(x = distance, y = smoothed, color = Index, fill = Index)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, color = NA) +
  geom_point(aes(y = Value), alpha = 0.5, size = 1) +
  scale_color_manual(values = c("Height" = "#2ca02c", "EVI" = "#7570b3", "NDVI" = "#0072B2")) +
  scale_fill_manual(values = c("Height" = "#2ca02c", "EVI" = "#7570b3", "NDVI" = "#0072B2"),
                    name = "Standard Deviation") +
  labs(x =NULL, , y =NULL, , color = "Vegetation Index", fill = "Standard Deviation") +
  theme_minimal(base_size = 32) +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.minor = element_line(linewidth = 0.5),
    legend.position = "none" ,
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 外框黑色
  )

print(p)


# 设置输出文件路径和文件名
output_file <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/NDVI_Index_Smooth_SD.png"

# 保存图形
ggsave(filename = output_file, plot = p, width = 10, height = 10, dpi = 600)

# ------------------------------------------------------------------------------NDVI\EVI\Height相关性分析
library(corrplot)
library(dplyr)

# 选择需要计算相关性的列
df_corr <- df %>% select(Height, EVI, NDVI)

# 计算相关系数矩阵
corr_matrix <- cor(df_corr, use = "complete.obs", method = "pearson")

# 打印矩阵
cat("两两相关性矩阵 (Pearson):\n")
print(round(corr_matrix, 4))

# ==== Step 1: 在 R 窗口中显示 ====
corrplot(
  corr_matrix,
  method = "number",      # 用数字显示
  type = "upper",         # 上三角矩阵
  tl.cex = 1.5,           # 标签字体大小（约12号）
  number.cex = 1.5,       # 数值字体大小
  number.digits = 4,      # 保留4位小数
  tl.col = "black",       # 标签颜色
  tl.srt = 0,             # 标签水平放置
  number.font = 2,        # 数字加粗
  tl.pos = "td",          # 标签在上方（top diagonal）
  tl.offset = 0.6         # 标签上移一些
)

# ==== Step 2: 输出为 PNG 文件 ====
output_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/correlation_matrix.png"
png(filename = output_path, width = 10000, height = 8000, res = 600)
corrplot(
  corr_matrix,
  method = "number",
  type = "upper",
  tl.cex = 1.5,
  number.cex = 1.5,
  number.digits = 4,
  tl.col = "black",
  tl.srt = 0,
  number.font = 2,
  tl.pos = "td",          # 标签在上方
  tl.offset = 0.6         # 上移一些（可根据效果再微调）
)
dev.off()

cat("\n✅ 相关性矩阵图已保存到：", output_path, "\n")
# ------------------------------------------------------------------------------NDVI\EVI\Height相关性分析加背景值，字体放大
# 加载必要包
library(corrplot)
library(dplyr)

# 选择需要计算相关性的列
df_corr <- df %>% select(Height, EVI, NDVI)

# 计算相关系数矩阵
corr_matrix <- cor(df_corr, use = "complete.obs", method = "pearson")

# 打印矩阵
cat("两两相关性矩阵 (Pearson):\n")

output_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/correlation_matrix.png"
png(filename = output_path, width = 10000, height = 8000, res = 1000)
corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "white",
  tl.cex = 1.8,
  number.cex = 1.8,
  number.digits = 4,
  tl.col = "black",
  tl.srt = 0,
  number.font = 2,
  tl.pos = "td",
  tl.offset = 2,
  cl.cex = 1.5,       # 图例字体大小
  cl.offset = 0.08,   # 右移图例数字
  mar = c(0, 0, 4, 0)
)
dev.off()

cat("\n✅ 相关性矩阵图已保存到：", output_path, "\n")

# ------------------------------------------------------------------------------NDVI\EVI\NIRv\Height相关性分析加背景值，字体放大
# 加载必要包
library(corrplot)
library(dplyr)
library(tidyr)

# 读取 CSV
file_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/PD_SW_all.csv"
df <- read.csv(file_path)

# 检查列名
colnames(df)
# 假设列名为: distance, NIRv, EVI, NDVI, Height, NIRv_StdDev, NIRv_StdDev, NDVI_StdDev, Height_StdDev

# 将值和对应的 SD 都转换成长格式
df_long <- df %>%
  pivot_longer(cols = c("NIRv", "EVI", "NDVI", "Height"), names_to = "Index", values_to = "Value") %>%
  pivot_longer(cols = c("NIRv_StdDev", "EVI_StdDev", "NDVI_StdDev", "Height_StdDev"),
               names_to = "Index_SD", values_to = "SD") %>%
  # 保证 Value 和 SD 对应
  filter(
    (Index == "NIRv" & Index_SD == "Height_StdDev") |
      (Index == "EVI" & Index_SD == "EVI_StdDev") |
       (Index == "NDVI" & Index_SD == "NDVI_StdDev")|
       (Index == "Height" & Index_SD == "Height_StdDev")
  ) %>%
  select(-Index_SD)

# 选择需要计算相关性的列
df_corr <- df %>% select(NIRv, EVI, NDVI, Height)

# 计算相关系数矩阵
corr_matrix <- cor(df_corr, use = "complete.obs", method = "pearson")

# 打印矩阵
cat("两两相关性矩阵 (Pearson):\n")

output_path <- "D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/NDVI敏感性分析_SW/correlation_matrix_all.png"
png(filename = output_path, width = 10000, height = 8000, res = 1000)
corrplot(
    corr_matrix,
    method = "color",
    type = "upper",
    addCoef.col = "white",
    tl.cex = 1.8,
    number.cex = 1.8,
    number.digits = 4,
    tl.col = "black",
    tl.srt = 0,
    number.font = 2,
    tl.pos = "td",
    tl.offset = 2,
    cl.cex = 1.5,       # 图例字体大小
    cl.offset = 0.08,   # 右移图例数字
    mar = c(0, 0, 4, 0)
  )
dev.off()

cat("\n✅ 相关性矩阵图已保存到：", output_path, "\n")

