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
