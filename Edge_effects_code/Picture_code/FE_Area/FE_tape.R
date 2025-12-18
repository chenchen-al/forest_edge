#-------------------------------------------------------------------------------叠加1km和未叠加显著性差异
# 安装必要包（只需一次）
if (!require(effsize)) install.packages("effsize", dependencies = TRUE)
library(effsize)

# 导入数据
human_edge_with <- c(95.81084892, 95.65749856, 95.87711069, 95.89579596, 96.17585632, 96.280174)
human_edge_without <- c(94.19519742, 94.08058263, 94.33526411, 94.41473173, 94.69062524, 94.8642845)
data <- data.frame(
  human_edge = c(human_edge_with, human_edge_without),
  category = factor(rep(c("叠加1km人工林", "未叠加1km人工林"), each = 6),
                    levels = c("未叠加1km人工林", "叠加1km人工林"))  # 固定顺序
)

# 计算平均值差和置信区间
mean_with <- mean(human_edge_with)
mean_without <- mean(human_edge_without)
mean_diff <- mean_with - mean_without
ci <- t_test_result$conf.int
p_value <- t_test_result$p.value

# 计算效应量（Cohen's d）
cohen_d_result <- cohen.d(human_edge ~ category, data = data)
d_value <- cohen_d_result$estimate
d_magnitude <- cohen_d_result$magnitude  # small / medium / large

# 输出分析结果
cat("==== 分析结果汇总 ====\n")
cat("叠加人工林组平均值：", round(mean_with, 3), "\n")
cat("未叠加人工林组平均值：", round(mean_without, 3), "\n")
cat("平均值差异为：", round(mean_diff, 3), "\n")
cat("差异的 95% 置信区间为：[", round(ci[1], 3), ", ", round(ci[2], 3), "]\n", sep = "")

# 设置英文分类名
data$group <- factor(data$category,
                     levels = c("叠加1km人工林", "未叠加1km人工林"),
                     labels = c("Overlay", "Non-overlay"))


# 设置输出图像为 PNG，尺寸为正方形，分辨率高
png("D:/Forest_Fragmentation/Picture/FE_area/是否叠加人工林对比结果图.png",
    width = 10, height = 8, units = "in", res = 1000)

# 设置边距，左边距加大，防止 y 轴标题遮挡
par(mar = c(5, 7, 4, 2))  # 下左上右边距

# 绘图
boxplot(human_edge ~ group, data = data,
        main = "",
        xlab = "Group",
        ylab = "Anthropogenic Edge %",
        col = c("red", "blue"),
        border = "black",
        boxwex = 0.4,
        lwd = 2,
        cex.lab = 2.3,
        cex.axis = 2.3,
        names = c("Overlay", "Non-overlay"))

# 添加平均值虚线
abline(h = mean_with, col = "red", lty = 2, lwd = 2)
abline(h = mean_without, col = "blue", lty = 2, lwd = 2)

# 加粗外边框
box(lwd = 1.5)

# 关闭图形设备，真正写入文件
dev.off()


# 两组数据
human_edge_with <- c(95.81084892, 95.65749856, 95.87711069, 95.89579596, 96.17585632, 96.280174)
human_edge_without <- c(94.19519742, 94.08058263, 94.33526411, 94.41473173, 94.69062524, 94.8642845)

# 计算最大值、最小值、均值
stats_with <- c(
  max = max(human_edge_with),
  min = min(human_edge_with),
  mean = mean(human_edge_with)
)

stats_without <- c(
  max = max(human_edge_without),
  min = min(human_edge_without),
  mean = mean(human_edge_without)
)

# 输出两组数据的统计值
print(stats_with)
print(stats_without)

# 计算差异（with - without）
diff_stats <- stats_with - stats_without

cat("最大值差异:", diff_stats["max"], "\n")
cat("最小值差异:", diff_stats["min"], "\n")
cat("平均值差异:", diff_stats["mean"], "\n")

# ------------------------------------------------------------------------------叠加柱状图
# 加载必要的库
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# 读取数据
data <- read_csv("D:/Forest_Fragmentation/Picture/FE_area/FE_tape.csv")

# 计算总森林边缘面积
data <- data %>%
  mutate(Total_edge = Human_edge + Natural_edge)

# 设置最大 Y 轴范围
max_area <- 7  # 设定最大森林边缘面积 (10 × 10¹⁰ m²)

# 重新整理数据，便于绘制柱状图
data_long <- data %>%
  pivot_longer(cols = c("Human_edge", "Natural_edge"), 
               names_to = "Edge_Type", values_to = "Edge_Area")

# 重新命名边缘类型（保证堆积时 Natural Edge 在下面）
data_long$Edge_Type <- factor(data_long$Edge_Type, 
                              levels = c("Natural_edge", "Human_edge"),
                              labels = c("Natural Edge", "Anthropogenic Edge"))

# 堆积柱状图
ggplot(data, aes(x = factor(Year))) +
  geom_bar(data = data_long, 
           aes(y = Edge_Area, fill = Edge_Type),
           stat = "identity", 
           width = 0.8, 
           color = "black",      # 这句话为每条柱子添加细线
           size = 0.3, 
           alpha = 2.5) + 
  scale_fill_manual(values = c( "Anthropogenic Edge" ="#ADFF2F","Natural Edge" = "#00008B"),
                    guide = "none") + 
  labs(
    x = "Year",
    y = "FE Area (million ha)"
  ) + 
  theme_minimal(base_size = 30) + 
  theme(
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
  ) + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max_area),
    breaks = seq(0, max_area, by = 1),
    labels = scales::number_format(accuracy = 0.1)
  ) + 
  scale_x_discrete(labels = as.character(data$Year))

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_tape1.0.png", 
       width = 8, 
       height = 8, 
       dpi = 600, 
       bg = "white")


# ------------------------------------------------------------------------------饼状图
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# 读取数据
data <- read_csv("D:/Forest_Fragmentation/Picture/FE_area/FE_tape.csv")

# 计算 6 年平均值
mean_data <- data %>% 
  summarise(
    Natural = mean(Natural_edge, na.rm = TRUE),
    Anthropogenic = mean(Human_edge, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Edge_Type", 
               values_to = "Area")

# 计算百分比
mean_data <- mean_data %>% 
  mutate(Percent = Area / sum(Area) * 100)

# 设置颜色
edge_colors <- c(
  "Natural" = "#00008B",
  "Anthropogenic" = "#ADFF2F"
)

# 绘制饼状图（无数字标注）
ggplot(mean_data, aes(x = "", y = Percent, fill = Edge_Type)) +
  geom_bar(width = 1, stat = "identity", color = "black", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = edge_colors) +
  labs(
    fill = "",
    y = "",
    x = ""  ) +
  theme_minimal(base_size = 25) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 保存图表
ggsave("D:/Forest_Fragmentation/Picture/FE_area/FE_pie_mean.png",
       width = 8, height = 8, dpi = 600, bg = "white")
