# ------------------------------------------------------------------------------9km聚合提取
library(terra)

# 1. 读取你的 1km 栅格数据
r <- rast("D:/Forest_Fragmentation/人为和自然归因/相关性分析/NDVI/NDVI_NaturalEdge_2020.tif")

# 2. 设置聚合因子（假设原图是1km）
fact <- 9

# 3. 聚合栅格，分别计算四个统计量
r_max    <- aggregate(r, fact=fact, fun=max,    na.rm=TRUE)
r_min    <- aggregate(r, fact=fact, fun=min,    na.rm=TRUE)
r_mean   <- aggregate(r, fact=fact, fun=mean,   na.rm=TRUE)
r_median <- aggregate(r, fact=fact, fun=median, na.rm=TRUE)

# 4. 合并成一个多波段栅格
r_summary <- c(r_max, r_min, r_mean, r_median)
names(r_summary) <- c("max", "min", "mean", "median")

# 5. 提取所有格网的中心坐标和统计值（保留NA格）
df <- as.data.frame(r_summary, xy=TRUE, na.rm=FALSE)

# 6. 保存为 CSV 文件
output_csv <- "D:/Forest_Fragmentation/人为和自然归因/相关性分析/NDVI/NDVI_NaturalEdge_2020_9km.csv"
write.csv(df, output_csv, row.names = FALSE, na = "NA")

cat("✅ 9km 聚合统计已保存至：", output_csv, "\n")


# 批量清洗文件夹下的所有 CSV 文件（去除 NA，保证所有文件行数一致）
# ------------------------------------------------------------------------------数据清洗

input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/edge_climate"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")


# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/NDVI_NaturalEdge_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]

# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/NDVI_HumanEdge_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 查看前几行
head(edge_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:6]  # NDVI ~ VPD
hum_values <- edge_df[edge_df$type == "Hum", 2:6]

# 检查行数是否一致
if(nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 可以加上因子名称和类型
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD
)

# 查看前几行
head(diff_df)

# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型
lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff, data = diff_std)

# 3️⃣ 查看多重共线性
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型的 VIF
vif_final <- vif(lm_step)
print("最终模型 VIF：")
print(vif_final)

# 7️⃣ 标准化回归系数
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

library(corrplot)

# 导出为高分辨率 PNG
png("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/correlation_matrix.png", width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",       # 使用颜色显示
         addCoef.col = "black",  # 显示相关系数
         number.cex = 1.0,       # 相关系数字体大小
         tl.cex = 1.0,           # 标签字体大小
         tl.srt = 90,            # 标签垂直显示
         cl.cex = 1.0,           # 图例字体大小
         mar = c(0,0,1,0))       # 边距

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")

# -----------------------------------------------------------------------------
library(ggplot2)

# 提取标准化回归系数（去掉截距）
coef_df <- data.frame(
  Variable = names(coef_final)[-1],
  Coefficient = coef_final[-1]
)

# 添加变量顺序（可选，保证按绝对值大小排序）
coef_df$Variable <- factor(coef_df$Variable, levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])

# 提取模型解释力
r_squared <- summary(lm_step)$r.squared

# 绘图
p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # 给柱子加黑色边框
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(
       y = "Standardized Coefficient",
       x = "") +
  theme_minimal(base_size = 30) +  # 整体基础字体加大
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"), # 横坐标加大加粗
    axis.text.y = element_text(size = 30, color = "black"),                        # 纵坐标加大加粗
    axis.title.y = element_text(size = 30, color = "black"),                       # y轴标题
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),            # 标题居中加粗
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)          # 外框线
  )

# 保存高分辨率 PNG
ggsave("D:/Forest_Fragmentation/人为和自然归因/相关性分析2.0(AET)/regression_coefficients_beautified.png",
       plot = p, width = 10, height = 9, dpi = 600)

# 显示图
print(p)

# ==============================================================================加入树高数据Height
# ------------------------------------------------------------------------------数据清洗

input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/edge_climate"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")


# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/NDVI_NaturalEdge_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/NDVI_HumanEdge_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  Height  = height_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  Height  = height_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 查看前几行
head(edge_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:7]  # NDVI ~ VPD
hum_values <- edge_df[edge_df$type == "Hum", 2:7]

# 检查行数是否一致
if(nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 可以加上因子名称和类型
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  Height_diff  = diff_values$Height
)

# # 查看前几行
# head(diff_df)
# 
# # 1️⃣ 标准化差值数据
# diff_std <- as.data.frame(scale(diff_df))
# 
# # 2️⃣ 构建全模型
# lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff + Height_diff, data = diff_std)
# 
# # 3️⃣ 查看多重共线性
# vif_values <- vif(lm_full)
# print("全模型 VIF：")
# print(vif_values)
# 
# # 4️⃣ 逐步回归（AIC 准则）
# lm_step <- step(lm_full, direction = "both", trace = FALSE)
# 
# # 5️⃣ 最终模型结果
# summary(lm_step)
# 
# # 6️⃣ 最终模型的 VIF
# vif_final <- vif(lm_step)
# print("最终模型 VIF：")
# print(vif_final)
# 
# # 7️⃣ 标准化回归系数
# coef_final <- coef(lm_step)
# print("标准化系数：")
# print(coef_final)
# 
# # 计算原始差值数据的相关性矩阵
# cor_matrix <- cor(diff_df, use = "complete.obs")
# print("差值变量相关性矩阵：")
# print(round(cor_matrix, 3))
# # ------------------------------------------------------------------------------相关性矩阵作图
# library(corrplot)
# 
# # 导出为高分辨率 PNG
# png("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/correlation_matrix.png", width = 1200, height = 1200, res = 150)
# 
# corrplot(cor_matrix,
#          method = "color",       # 使用颜色显示
#          addCoef.col = "black",  # 显示相关系数
#          number.cex = 1.0,       # 相关系数字体大小
#          tl.cex = 1.0,           # 标签字体大小
#          tl.srt = 90,            # 标签垂直显示
#          cl.cex = 1.0,           # 图例字体大小
#          mar = c(0,0,1,0))       # 边距
# 
# dev.off()
# 
# # 查看全模型和逐步回归模型的解释力
# cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
# cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")
# 
# # -----------------------------------------------------------------------------
# library(ggplot2)
# 
# # 提取标准化回归系数（去掉截距）
# coef_df <- data.frame(
#   Variable = names(coef_final)[-1],
#   Coefficient = coef_final[-1]
# )
# 
# # 添加变量顺序（可选，保证按绝对值大小排序）
# coef_df$Variable <- factor(coef_df$Variable, levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])
# 
# # 提取模型解释力
# r_squared <- summary(lm_step)$r.squared
# 
# # 绘图
# p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
#   geom_bar(stat = "identity", width = 0.6, color = "black") +  # 给柱子加黑色边框
#   scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
#   geom_hline(yintercept = 0, color = "black", size = 0.8) +
#   labs(
#     y = "Standardized Coefficient",
#     x = "") +
#   theme_minimal(base_size = 30) +  # 整体基础字体加大
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"), # 横坐标加大加粗
#     axis.text.y = element_text(size = 30, color = "black"),                        # 纵坐标加大加粗
#     axis.title.y = element_text(size = 30, color = "black"),                       # y轴标题
#     plot.title = element_text(size = 30, face = "bold", hjust = 0.5),            # 标题居中加粗
#     panel.border = element_rect(color = "black", fill = NA, size = 1.2)          # 外框线
#   )
# 
# # 保存高分辨率 PNG
# ggsave("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/regression_coefficients_beautified.png",
#        plot = p, width = 10, height = 9, dpi = 600)
# 
# # 显示图
# print(p)
# 
## 查看前几行
head(diff_df)

# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型
lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff + Height_diff, data = diff_std)

# 3️⃣ 查看多重共线性
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# ⭐ 关键修改：如果逐步回归中 VPD_diff 被剔除，就重新加回去
if (!"VPD_diff" %in% names(coef(lm_step))) {
  lm_step <- update(lm_step, . ~ . + VPD_diff)
}

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型的 VIF
vif_final <- vif(lm_step)
print("最终模型 VIF：")
print(vif_final)

# 7️⃣ 标准化回归系数
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------相关性矩阵作图
png("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/correlation_matrix.png", width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.0,
         tl.srt = 90,
         cl.cex = 1.0,
         mar = c(0,0,1,0))

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")

# -----------------------------------------------------------------------------
# 可视化部分保持不变
# -----------------------------------------------------------------------------
coef_df <- data.frame(
  Variable = names(coef_final)[-1],
  Coefficient = coef_final[-1]
)

coef_df$Variable <- factor(coef_df$Variable,
                           levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])

r_squared <- summary(lm_step)$r.squared

p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +  # 森林绿
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(x = "", y = "") +  # 去掉横纵坐标轴标题
  theme_minimal(base_size = 30) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )

ggsave("D:/Forest_Fragmentation/人为和自然归因/相关性分析3.0(AET+Height)/regression_coefficients_beautified_VPD.png",
       plot = p, width = 10, height = 9, dpi = 600)

print(p)

# ==============================================================================加入树高数据Height和Age数据
# ------------------------------------------------------------------------------数据清洗

input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/edge_climate"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")


# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/NDVI_NaturalEdge_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/NDVI_HumanEdge_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]
# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  Height  = height_nat,
  Age  = age_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  Height  = height_hum,
  Age  = age_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 查看前几行
head(edge_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:8]  # NDVI ~ VPD
hum_values <- edge_df[edge_df$type == "Hum", 2:8]

# 检查行数是否一致
if(nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 可以加上因子名称和类型
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  Height_diff  = diff_values$Height,
  Age_diff  = diff_values$Age
)

# # 查看前几行
# head(diff_df)

# # 1️⃣ 标准化差值数据
# diff_std <- as.data.frame(scale(diff_df))
# 
# # 2️⃣ 构建全模型
# lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff + Height_diff+ Age_diff, data = diff_std)
# 
# # 3️⃣ 查看多重共线性
# vif_values <- vif(lm_full)
# print("全模型 VIF：")
# print(vif_values)
# 
# # 4️⃣ 逐步回归（AIC 准则）
# lm_step <- step(lm_full, direction = "both", trace = FALSE)
# 
# # 5️⃣ 最终模型结果
# summary(lm_step)
# 
# # 6️⃣ 最终模型的 VIF
# vif_final <- vif(lm_step)
# print("最终模型 VIF：")
# print(vif_final)
# 
# # 7️⃣ 标准化回归系数
# coef_final <- coef(lm_step)
# print("标准化系数：")
# print(coef_final)
# 
# # 计算原始差值数据的相关性矩阵
# cor_matrix <- cor(diff_df, use = "complete.obs")
# print("差值变量相关性矩阵：")
# print(round(cor_matrix, 3))
# # ------------------------------------------------------------------------------相关性矩阵作图
# library(corrplot)
# 
# # 导出为高分辨率 PNG
# png("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/correlation_matrix.png", width = 1200, height = 1200, res = 150)
# 
# corrplot(cor_matrix,
#          method = "color",       # 使用颜色显示
#          addCoef.col = "black",  # 显示相关系数
#          number.cex = 1.0,       # 相关系数字体大小
#          tl.cex = 1.0,           # 标签字体大小
#          tl.srt = 90,            # 标签垂直显示
#          cl.cex = 1.0,           # 图例字体大小
#          mar = c(0,0,1,0))       # 边距
# 
# dev.off()
# 
# # 查看全模型和逐步回归模型的解释力
# cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
# cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")
# 
# # -----------------------------------------------------------------------------
# library(ggplot2)
# 
# # 提取标准化回归系数（去掉截距）
# coef_df <- data.frame(
#   Variable = names(coef_final)[-1],
#   Coefficient = coef_final[-1]
# )
# 
# # 添加变量顺序（可选，保证按绝对值大小排序）
# coef_df$Variable <- factor(coef_df$Variable, levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])
# 
# # 提取模型解释力
# r_squared <- summary(lm_step)$r.squared
# 
# # 绘图
# p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
#   geom_bar(stat = "identity", width = 0.6, color = "black") +  # 给柱子加黑色边框
#   scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
#   geom_hline(yintercept = 0, color = "black", size = 0.8) +
#   labs(
#     y = "Standardized Coefficient",
#     x = "") +
#   theme_minimal(base_size = 30) +  # 整体基础字体加大
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"), # 横坐标加大加粗
#     axis.text.y = element_text(size = 30, color = "black"),                        # 纵坐标加大加粗
#     axis.title.y = element_text(size = 30, color = "black"),                       # y轴标题
#     plot.title = element_text(size = 30, face = "bold", hjust = 0.5),            # 标题居中加粗
#     panel.border = element_rect(color = "black", fill = NA, size = 1.2)          # 外框线
#   )
# 
# # 保存高分辨率 PNG
# ggsave("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/regression_coefficients_beautified.png",
#        plot = p, width = 10, height = 9, dpi = 600)
# 
# # 显示图
# print(p)


# 
## 查看前几行
head(diff_df)

# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型
lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff + Height_diff + Age_diff, data = diff_std)

# 3️⃣ 查看多重共线性
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# ⭐ 关键修改：如果逐步回归中 VPD_diff 被剔除，就重新加回去
if (!"VPD_diff" %in% names(coef(lm_step))) {
  lm_step <- update(lm_step, . ~ . + VPD_diff)
}

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型的 VIF
vif_final <- vif(lm_step)
print("最终模型 VIF：")
print(vif_final)

# 7️⃣ 标准化回归系数
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------相关性矩阵作图
png("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/correlation_matrix.png", width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.0,
         tl.srt = 90,
         cl.cex = 1.0,
         mar = c(0,0,1,0))

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")

# -----------------------------------------------------------------------------
# 可视化部分保持不变
# -----------------------------------------------------------------------------
coef_df <- data.frame(
  Variable = names(coef_final)[-1],
  Coefficient = coef_final[-1]
)

coef_df$Variable <- factor(coef_df$Variable,
                           levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])

r_squared <- summary(lm_step)$r.squared

p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +  # 森林绿
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(x = "", y = "") +  # 去掉横纵坐标轴标题
  theme_minimal(base_size = 30) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )

ggsave("D:/Forest_Fragmentation/人为和自然归因/相关性分析4.0(AET+Height+Age)/regression_coefficients_beautified_VPD.png",
       plot = p, width = 10, height = 9, dpi = 600)

print(p)


# ==============================================================================加入树高数据Height和Age数据,内外环境差
# ------------------------------------------------------------------------------数据清洗

input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")

# ------------------------------------------------------------------------------ 环境因子逐列差值（Edge - Core）
# 1. 读取两个清洗后的 CSV 文件
hum_core_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/AET_Nat_core_2020_9km_clean.csv")
hum_edge_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/AET_Nat_edge_2020_9km_clean.csv")

# 2. 检查行列是否匹配
stopifnot(nrow(hum_core_clean) == nrow(hum_edge_clean))
stopifnot(ncol(hum_core_clean) == ncol(hum_edge_clean))

# 3. 前两列为坐标，直接保留；从第3列开始做差值
coord_cols <- hum_edge_clean[, 1:2]
diff_cols <- hum_edge_clean[, 3:ncol(hum_edge_clean)] - hum_core_clean[, 3:ncol(hum_core_clean)]

# 4. 合并坐标列和差值列
hum_diff_all <- cbind(coord_cols, diff_cols)

# 5. 输出结果到新文件
write.csv(hum_diff_all, "D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/AET_NatEdge_minus_Core_2020_9km.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]

lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/LST_NatEdge_minus_Core_2020_9km.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/AET_NatEdge_minus_Core_2020_9km.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/SSM_NatEdge_minus_Core_2020_9km.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/VPD_NatEdge_minus_Core_2020_9km.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/LST_HumEdge_minus_Core_2020_9km.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/AET_HumEdge_minus_Core_2020_9km.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/SSM_HumEdge_minus_Core_2020_9km.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/VPD_HumEdge_minus_Core_2020_9km.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/cleaned/Age_Hum_2020_9km_clean.csv")[,5]
# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  Height  = height_nat,
  Age  = age_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  Height  = height_hum,
  Age  = age_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 查看前几行
head(edge_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:8]  # NDVI ~ VPD
hum_values <- edge_df[edge_df$type == "Hum", 2:8]

# 检查行数是否一致
if(nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 可以加上因子名称和类型
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  Height_diff  = diff_values$Height,
  Age_diff  = diff_values$Age
)
## 查看前几行
head(diff_df)

# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型
lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff + Height_diff + Age_diff, data = diff_std)

# 3️⃣ 查看多重共线性
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# ⭐ 关键修改：如果逐步回归中 VPD_diff 被剔除，就重新加回去
if (!"VPD_diff" %in% names(coef(lm_step))) {
  lm_step <- update(lm_step, . ~ . + VPD_diff)
}

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型的 VIF
vif_final <- vif(lm_step)
print("最终模型 VIF：")
print(vif_final)

# 7️⃣ 标准化回归系数
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------相关性矩阵作图
library(corrplot)
png("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/correlation_matrix.png", width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.0,
         tl.srt = 90,
         cl.cex = 1.0,
         mar = c(0,0,1,0))

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")

# -----------------------------------------------------------------------------
# 可视化部分保持不变
# -----------------------------------------------------------------------------
coef_df <- data.frame(
  Variable = names(coef_final)[-1],
  Coefficient = coef_final[-1]
)

coef_df$Variable <- factor(coef_df$Variable,
                           levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])

r_squared <- summary(lm_step)$r.squared

p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +  # 森林绿
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(x = "", y = "") +  # 去掉横纵坐标轴标题
  theme_minimal(base_size = 30) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )

ggsave("D:/Forest_Fragmentation/人为和自然归因/d相关性分析5.0(AET+Height+Age)/dh-dn/regression_coefficients_beautified_VPD.png",
       plot = p, width = 10, height = 9, dpi = 600)

print(p)


# ==============================================================================加入树高数据Height和Age数据,内外环境差,也包含外环境
# ------------------------------------------------------------------------------数据清洗

input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/dh-dn"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")

# ------------------------------------------------------------------------------ 环境因子逐列差值（Edge - Core）
# 1. 读取两个清洗后的 CSV 文件
hum_core_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/dh-dn/cleaned/dSSM_Hum_core_2020_9km_clean.csv")
hum_edge_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/dh-dn/cleaned/dSSM_Hum_edge_2020_9km_clean.csv")

# 2. 检查行列是否匹配
stopifnot(nrow(hum_core_clean) == nrow(hum_edge_clean))
stopifnot(ncol(hum_core_clean) == ncol(hum_edge_clean))

# 3. 前两列为坐标，直接保留；从第3列开始做差值
coord_cols <- hum_edge_clean[, 1:2]
diff_cols <- hum_edge_clean[, 3:ncol(hum_edge_clean)] - hum_core_clean[, 3:ncol(hum_core_clean)]

# 4. 合并坐标列和差值列
hum_diff_all <- cbind(coord_cols, diff_cols)

# 5. 输出结果到新文件
write.csv(hum_diff_all, "D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/dh-dn/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]
dlst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv")[,5]
daet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dAET_NatEdge_minus_Core_2020_9km.csv")[,5]
dssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dSSM_NatEdge_minus_Core_2020_9km.csv")[,5]
dvpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dVPD_NatEdge_minus_Core_2020_9km.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]
dlst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dLST_HumEdge_minus_Core_2020_9km.csv")[,5]
daet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dAET_HumEdge_minus_Core_2020_9km.csv")[,5]
dssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv")[,5]
dvpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  dLST = dlst_nat,
  dAET = daet_nat,
  dSSM = dssm_nat,
  dVPD = dvpd_nat,
  Height = height_nat,
  Age = age_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  dLST = dlst_hum,
  dAET = daet_hum,
  dSSM = dssm_hum,
  dVPD = dvpd_hum,
  Height = height_hum,
  Age = age_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:12]
hum_values <- edge_df[edge_df$type == "Hum", 2:12]

# 检查行数是否一致
if (nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 生成差值数据框
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  dLST_diff = diff_values$dLST,
  dAET_diff = diff_values$dAET,
  dSSM_diff = diff_values$dSSM,
  dVPD_diff = diff_values$dVPD,
  Height_diff = diff_values$Height,
  Age_diff = diff_values$Age
)
# ------------------------------------------------------------------
# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型（包含新变量）
lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff +
                dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +
                Height_diff + Age_diff, data = diff_std)

# 3️⃣ 多重共线性（在全模型上检查）
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则，按 AIC 自动选变量）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型 VIF（若 lm_step 仅含截距，vif 会报错；为稳健起见先判断）
if (length(coef(lm_step)) > 1) {
  vif_final <- vif(lm_step)
  print("最终模型 VIF：")
  print(vif_final)
} else {
  print("最终模型仅含截距，无法计算 VIF。")
}

# 7️⃣ 标准化回归系数（lm_step 已基于标准化数据拟合）
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 8️⃣ 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------相关性矩阵作图
library(corrplot)
png("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/correlation_matrix.png",
    width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.0,
         tl.srt = 90,
         cl.cex = 1.0,
         mar = c(0,0,1,0))

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")


# -----------------------------------------------------------------------------
# 可视化部分保持不变
# -----------------------------------------------------------------------------
coef_df <- data.frame(
  Variable = names(coef_final)[-1],
  Coefficient = coef_final[-1]
)

coef_df$Variable <- factor(coef_df$Variable,
                           levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])

r_squared <- summary(lm_step)$r.squared

p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +  # 森林绿
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(x = "", y = "") +  # 去掉横纵坐标轴标题
  theme_minimal(base_size = 30) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )

ggsave("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/regression_coefficients_beautified_VPD.png",
       plot = p, width = 10, height = 9, dpi = 600)

print(p)





# ------------------------------------------------------------------------------随机森林
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]
dlst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv")[,5]
daet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dAET_NatEdge_minus_Core_2020_9km.csv")[,5]
dssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dSSM_NatEdge_minus_Core_2020_9km.csv")[,5]
dvpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dVPD_NatEdge_minus_Core_2020_9km.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]
dlst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dLST_HumEdge_minus_Core_2020_9km.csv")[,5]
daet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dAET_HumEdge_minus_Core_2020_9km.csv")[,5]
dssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv")[,5]
dvpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  dLST = dlst_nat,
  dAET = daet_nat,
  dSSM = dssm_nat,
  dVPD = dvpd_nat,
  Height = height_nat,
  Age = age_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  dLST = dlst_hum,
  dAET = daet_hum,
  dSSM = dssm_hum,
  dVPD = dvpd_hum,
  Height = height_hum,
  Age = age_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:12]
hum_values <- edge_df[edge_df$type == "Hum", 2:12]

# 检查行数是否一致
if (nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 生成差值数据框
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  dLST_diff = diff_values$dLST,
  dAET_diff = diff_values$dAET,
  dSSM_diff = diff_values$dSSM,
  dVPD_diff = diff_values$dVPD,
  Height_diff = diff_values$Height,
  Age_diff = diff_values$Age
)

# ------------------------------------------------------------------
# 📊 随机森林分析：人为与自然林缘 NDVI 差异的主导因子
# ------------------------------------------------------------------
library(randomForest)
library(ggplot2)

# 1️⃣ 设置随机种子，保证结果可重复
set.seed(1234)

# 2️⃣ 构建随机森林模型
rf_model <- randomForest(
  NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff +
    dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +
    Height_diff + Age_diff,
  data = diff_df,
  ntree = 1000,        # 树的数量（一般 500–2000 较稳定）
  importance = TRUE    # 启用变量重要性计算
)

# 3️⃣ 输出模型基本信息
print(rf_model)

# 4️⃣ 计算变量重要性
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# 两种指标：
# %IncMSE 表示变量被置换后模型误差增加的百分比 → 越大越重要
# IncNodePurity 表示基于节点纯度的贡献 → 越大越重要

# 5️⃣ 按 %IncMSE 排序
importance_df <- importance_df[order(importance_df[,"%IncMSE"], decreasing = TRUE), ]

# 6️⃣ 绘制变量重要性图
# 输出路径（可自行修改）
output_path <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/RF_importance_plot.png"

# 绘图对象
p <- ggplot(importance_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "#4C9F70") +
  coord_flip() +
  theme_bw(base_size = 20) +
  labs(x = "Variables", 
       y = "% Increase in MSE",
       title = "Variable Importance for NDVI Differences (Random Forest)") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 保存高分辨率图片（推荐 600 dpi）
ggsave(output_path, plot = p, width = 10, height = 8, dpi = 600)

# 控制台提示
cat("✅ 随机森林变量重要性图已保存至：", output_path, "\n")

# ------------------------------------------------------------------
# 7️⃣ 查看前几名最重要变量
head(importance_df, 8)



# ===========================================================================
library(randomForest)
library(ggplot2)

set.seed(1234)

# 1️⃣ 构建随机森林模型
rf_model <- randomForest(
  NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff +
    dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +
    Height_diff + Age_diff,
  data = diff_df,
  ntree = 1000,
  importance = TRUE
)

# 2️⃣ 计算模型整体 R²
y <- diff_df$NDVI_diff
y_pred <- rf_model$predicted
model_R2 <- 1 - mean((y - y_pred)^2) / var(y)
cat("模型整体解释力 R² =", model_R2, "\n")

# 3️⃣ 计算变量 %IncMSE
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# 4️⃣ 将每个变量的重要性归一化到模型 R²
ER_raw <- importance_df$`%IncMSE`
ER_normalized <- ER_raw * model_R2 / sum(ER_raw)
importance_df$Explained_R2 <- ER_normalized

# 5️⃣ 按贡献排序
importance_df <- importance_df[order(importance_df$Explained_R2, decreasing = TRUE), ]
# 输出路径（可自行修改）
output_path <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析6.0(AET+Height+Age)/RF_importance_r2_ver.2.png"

# p <- ggplot(importance_df, aes(x = reorder(Variable, -Explained_R2), y = Explained_R2)) +
#   geom_bar(stat = "identity", fill = "#4C9F70") +
#   #coord_flip() +
#   theme_bw(base_size = 20) +
#   labs(x = "Variables", 
#        y = "Explained Contribution (R²)",
#        title = "Variable Contributions to NDVI Differences (Random Forest)") +
#   theme(plot.title = element_text(hjust = 0.5),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
p <- ggplot(importance_df, aes(x = reorder(Variable, -Explained_R2), y = Explained_R2)) +
  geom_bar(stat = "identity", fill = "#4C9F70", width = 0.6) +
  scale_y_continuous(
    limits = c(0, 0.065),                 # ✅ Y 轴范围
    breaks = seq(0, 0.065, by = 0.02)     # ✅ 间隔 0.02
    #expand = c(0, 0)                       # ✅ 柱子从 0 贴地（论文风）
  ) +
  theme_bw(base_size = 20) +
  labs(
    x = "Variables", 
    y = "Explained Contribution (R²)",
    title = "Variable Contributions to NDVI Differences (Random Forest)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    # ✅ 外框线
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # ✅ 坐标轴主线
    axis.line = element_line(color = "black", linewidth = 0.8),
    
    # ✅ 坐标轴刻度（小短轴）
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.25, "cm"),
    
    # 去掉网格
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # x 轴文字旋转（防重叠）
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black")
  )

ggsave(output_path, plot = p, width = 20, height = 7, dpi = 600)

cat("✅ 随机森林变量解释力图已保存至：", output_path, "\n")

# 7️⃣ 输出前几名变量
head(importance_df, 10)
cat("✅ 验证：所有变量解释力总和 =", sum(importance_df$Explained_R2), "≈ 模型 R² =", model_R2, "\n")

# ==============================================================================加入树高数据Height和Age数据,内外环境差,不包含外环境
# ------------------------------------------------------------------------------数据清洗

input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/dh-dn"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")

# ------------------------------------------------------------------------------ 环境因子逐列差值（Edge - Core）
# 1. 读取两个清洗后的 CSV 文件
hum_core_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/dh-dn/cleaned/dVPD_Hum_core_2020_9km_clean.csv")
hum_edge_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/dh-dn/cleaned/dVPD_Hum_edge_2020_9km_clean.csv")

# 2. 检查行列是否匹配
stopifnot(nrow(hum_core_clean) == nrow(hum_edge_clean))
stopifnot(ncol(hum_core_clean) == ncol(hum_edge_clean))

# 3. 前两列为坐标，直接保留；从第3列开始做差值
coord_cols <- hum_edge_clean[, 1:2]
diff_cols <- hum_edge_clean[, 3:ncol(hum_edge_clean)] - hum_core_clean[, 3:ncol(hum_core_clean)]

# 4. 合并坐标列和差值列
hum_diff_all <- cbind(coord_cols, diff_cols)

# 5. 输出结果到新文件
write.csv(hum_diff_all, "D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/dh-dn/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]
dlst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv")[,5]
daet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dAET_NatEdge_minus_Core_2020_9km.csv")[,5]
dssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dSSM_NatEdge_minus_Core_2020_9km.csv")[,5]
dvpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dVPD_NatEdge_minus_Core_2020_9km.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
dlst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dLST_HumEdge_minus_Core_2020_9km.csv")[,5]
daet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dAET_HumEdge_minus_Core_2020_9km.csv")[,5]
dssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv")[,5]
dvpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  dLST = dlst_nat,
  dAET = daet_nat,
  dSSM = dssm_nat,
  dVPD = dvpd_nat,
  Height = height_nat,
  Age = age_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  dLST = dlst_hum,
  dAET = daet_hum,
  dSSM = dssm_hum,
  dVPD = dvpd_hum,
  Height = height_hum,
  Age = age_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:8]
hum_values <- edge_df[edge_df$type == "Hum", 2:8]

# 检查行数是否一致
if (nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 生成差值数据框
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  dLST_diff = diff_values$dLST,
  dAET_diff = diff_values$dAET,
  dSSM_diff = diff_values$dSSM,
  dVPD_diff = diff_values$dVPD,
  Height_diff = diff_values$Height,
  Age_diff = diff_values$Age
)
# ------------------------------------------------------------------
# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型（包含新变量）
lm_full <- lm(NDVI_diff ~ 
                dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +
                Height_diff + Age_diff, data = diff_std)

# 3️⃣ 多重共线性（在全模型上检查）
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则，按 AIC 自动选变量）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型 VIF（若 lm_step 仅含截距，vif 会报错；为稳健起见先判断）
if (length(coef(lm_step)) > 1) {
  vif_final <- vif(lm_step)
  print("最终模型 VIF：")
  print(vif_final)
} else {
  print("最终模型仅含截距，无法计算 VIF。")
}

# 7️⃣ 标准化回归系数（lm_step 已基于标准化数据拟合）
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 8️⃣ 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------相关性矩阵作图
library(corrplot)
png("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/correlation_matrix.png",
    width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.0,
         tl.srt = 90,
         cl.cex = 1.0,
         mar = c(0,0,1,0))

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")


# -----------------------------------------------------------------------------
# 可视化部分保持不变
# -----------------------------------------------------------------------------
coef_df <- data.frame(
  Variable = names(coef_final)[-1],
  Coefficient = coef_final[-1]
)

coef_df$Variable <- factor(coef_df$Variable,
                           levels = coef_df$Variable[order(abs(coef_df$Coefficient), decreasing = TRUE)])

r_squared <- summary(lm_step)$r.squared

p <- ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +  # 森林绿
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  labs(x = "", y = "") +  # 去掉横纵坐标轴标题
  theme_minimal(base_size = 30) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )

ggsave("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/regression_coefficients_beautified_VPD.png",
       plot = p, width = 10, height = 9, dpi = 600)

print(p)





# ------------------------------------------------------------------------------随机森林
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]
dlst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv")[,5]
daet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dAET_NatEdge_minus_Core_2020_9km.csv")[,5]
dssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dSSM_NatEdge_minus_Core_2020_9km.csv")[,5]
dvpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dVPD_NatEdge_minus_Core_2020_9km.csv")[,5]
height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
dlst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dLST_HumEdge_minus_Core_2020_9km.csv")[,5]
daet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dAET_HumEdge_minus_Core_2020_9km.csv")[,5]
dssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv")[,5]
dvpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv")[,5]
height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  dLST = dlst_nat,
  dAET = daet_nat,
  dSSM = dssm_nat,
  dVPD = dvpd_nat,
  Height = height_nat,
  Age = age_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  dLST = dlst_hum,
  dAET = daet_hum,
  dSSM = dssm_hum,
  dVPD = dvpd_hum,
  Height = height_hum,
  Age = age_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:8]
hum_values <- edge_df[edge_df$type == "Hum", 2:8]

# 检查行数是否一致
if (nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 生成差值数据框
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  dLST_diff = diff_values$dLST,
  dAET_diff = diff_values$dAET,
  dSSM_diff = diff_values$dSSM,
  dVPD_diff = diff_values$dVPD,
  Height_diff = diff_values$Height,
  Age_diff = diff_values$Age
)

# ------------------------------------------------------------------
# 📊 随机森林分析：人为与自然林缘 NDVI 差异的主导因子
# ------------------------------------------------------------------
library(randomForest)
library(ggplot2)

# 1️⃣ 设置随机种子，保证结果可重复
set.seed(1234)

# 2️⃣ 构建随机森林模型
rf_model <- randomForest(
  NDVI_diff ~ 
    dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +
    Height_diff + Age_diff,
  data = diff_df,
  ntree = 1000,        # 树的数量（一般 500–2000 较稳定）
  importance = TRUE    # 启用变量重要性计算
)

# 3️⃣ 输出模型基本信息
print(rf_model)

# 4️⃣ 计算变量重要性
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# 两种指标：
# %IncMSE 表示变量被置换后模型误差增加的百分比 → 越大越重要
# IncNodePurity 表示基于节点纯度的贡献 → 越大越重要

# 5️⃣ 按 %IncMSE 排序
importance_df <- importance_df[order(importance_df[,"%IncMSE"], decreasing = TRUE), ]

# 6️⃣ 绘制变量重要性图
# 输出路径（可自行修改）
output_path <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析7.0(AET+Height+Age)/RF_importance_plot.png"

# 绘图对象
p <- ggplot(importance_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "#4C9F70") +
  coord_flip() +
  theme_bw(base_size = 20) +
  labs(x = "Variables", 
       y = "% Increase in MSE",
       title = "Variable Importance for NDVI Differences (Random Forest)") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 保存高分辨率图片（推荐 600 dpi）
ggsave(output_path, plot = p, width = 10, height = 8, dpi = 600)

# 控制台提示
cat("✅ 随机森林变量重要性图已保存至：", output_path, "\n")

# ------------------------------------------------------------------
# 7️⃣ 查看前几名最重要变量
head(importance_df, 8)
