# ------------------------------------------------------------------------------树高饱和点做垂线段（全部Nat.&Hum）
library(readr)
library(ggplot2)

# 读取数据
data <- read.csv("D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/树高数据/Tree_height_PD_China.csv", header = TRUE)
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
  geom_point(shape = 21, size = 3, fill =  "#2ca02c", color = "black", stroke = 1) +  # 蓝色点
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
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # 黑色外框
    axis.title = element_blank(), 
    axis.text = element_text(size = 30, color = "black"),  # 坐标轴字体为黑色
    axis.ticks.length = unit(0.2, "cm")  # 小刻度线
  )

# 显示图像
print(p)

# 保存图像
ggsave("D:/Forest_Fragmentation/Picture/R_NDVI/Sensitive_test/树高数据/height_FD_china(tpoint).png",
       plot = p,
       width = 10, height = 9, dpi = 600)

