# 加载必要库
library(terra)  # 或 raster 都可以
library(dplyr)

# 设置文件夹路径
folder_path <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_raw"

# 列出文件夹中所有tif文件
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# 将 0.1 mm 转换为 mm
ppt_stack_mm <- ppt_stack / 10

# 计算年度总降水
ppt_annual <- sum(ppt_stack_mm, na.rm = TRUE)

# 输出年降水tif
output_path <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_annual.tif"
writeRaster(ppt_annual, output_path, overwrite = TRUE)

# 输出最大值、最小值和均值
ppt_max <- global(ppt_annual, "max", na.rm = TRUE)[1,1]
ppt_min <- global(ppt_annual, "min", na.rm = TRUE)[1,1]
ppt_mean <- global(ppt_annual, "mean", na.rm = TRUE)[1,1]

cat("2020年年降水 - 最大值:", ppt_max, "\n")
cat("2020年年降水 - 最小值:", ppt_min, "\n")
cat("2020年年降水 - 均值:", ppt_mean, "\n")

# ------------------------------------------------------------------------------Hum/Nat edge掩膜提取
rm(list = ls())
gc(reset = TRUE)

library(terra)

# 1. 读取掩膜栅格（Hum Edge）
mask_path <- "D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Nat_Edge_raw2.0/2020_1km/Forest_to_NatEdge_2020_1km.tif"
mask <- rast(mask_path)

# 2. 读取 PPT 栅格（2020 年均值）
PPT_path <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_raw/PPT_2020_annual.tif"
PPT <- rast(PPT_path)

# 3. 检查 CRS 是否一致，不一致则重投影 PPT 到 mask 的 CRS
if (!compareGeom(PPT, mask, crs = TRUE, stopOnError = FALSE)) {
  message("CRS 不一致，进行重投影...")
  PPT <- project(PPT, mask, method = "near")
} else {
  message("CRS 一致，无需重投影")
}

# 4. 重采样对齐（确保栅格分辨率和范围匹配）
PPT_aligned <- resample(PPT, mask, method = "near")

# 5. 掩膜操作（保留 mask == 1 区域，其他设 NA）
PPT_masked <- mask(PPT_aligned, mask, maskvalues = 0, updatevalue = NA)

# 6. 输出路径
output_path <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_raw/PPT_Nat_2020.tif"

# 7. 写出结果，保存为浮点型
writeRaster(PPT_masked, output_path, datatype = "FLT4S", NAflag = -9999, overwrite = TRUE)

cat("✅ 掩膜后的 PPT 数据已保存至：", output_path, "\n")

# ------------------------------------------------------------------------------9km聚合提取 (PPT)
library(terra)

# 1. 读取你的 1km 栅格数据（PPT 人为边缘）
r <- rast("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_raw/PPT_Hum_2020.tif")

# 2. 设置聚合因子（假设原图是1km）
fact <- 9

# 3. 聚合栅格，分别计算四个统计量
r_max    <- aggregate(r, fact = fact, fun = max,    na.rm = TRUE)
r_min    <- aggregate(r, fact = fact, fun = min,    na.rm = TRUE)
r_mean   <- aggregate(r, fact = fact, fun = mean,   na.rm = TRUE)
r_median <- aggregate(r, fact = fact, fun = median, na.rm = TRUE)

# 4. 合并成一个多波段栅格
r_summary <- c(r_max, r_min, r_mean, r_median)
names(r_summary) <- c("max", "min", "mean", "median")

# 5. 提取所有格网的中心坐标和统计值（保留NA格）
df <- as.data.frame(r_summary, xy = TRUE, na.rm = FALSE)

# 6. 保存为 CSV 文件
output_csv <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Hum_2020_9km.csv"
write.csv(df, output_csv, row.names = FALSE, na = "NA")

cat("✅ 9km 聚合统计已保存至：", output_csv, "\n")

# ------------------------------------------------------------------------------9km聚合提取类型合并，去除NA (PPT)
# 1. 读取两个 CSV 文件
hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Hum_2020_9km.csv")
nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Nat_2020_9km.csv")

# 2. 找出两个文件中都没有 NA 的行
valid_rows <- complete.cases(hum) & complete.cases(nat)

# 3. 筛选出无NA的行
hum_clean <- hum[valid_rows, ]
nat_clean <- nat[valid_rows, ]

# 4. 可选：确保行数一致
stopifnot(nrow(hum_clean) == nrow(nat_clean))

# 5. 写出为新的 CSV 文件
write.csv(hum_clean, "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Hum_2020_9km_clean.csv", row.names = FALSE)
write.csv(nat_clean, "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Nat_2020_9km_clean.csv", row.names = FALSE)

cat("✅ 清洗后的 PPT 数据已保存。\n")

# ------------------------------------------------------------------------------差值作图优化版
library(ggplot2)

# 1. 读取清理后的两个 CSV 文件
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Hum_2020_9km_clean.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/PPT_Nat_2020_9km_clean.csv")

# 2. 取mean列，计算插值（差值）
interpolation <- hum_clean$mean - nat_clean$mean

# 2.1 进行配对 t 检验（假设每个 Hum 与 Nat 栅格一一对应）
hum_mean <- hum_clean$mean
nat_mean <- nat_clean$mean

t_test_result <- t.test(hum_mean, nat_mean, paired = TRUE)  # 默认 paired=FALSE
cat("T 检验结果：\n")
print(t_test_result)
cat("p值 =", t_test_result$p.value, "\n")

# 3. 计算比例（小于0和大于0）
prop_less_0 <- mean(interpolation < 0, na.rm = TRUE)
prop_greater_0 <- mean(interpolation > 0, na.rm = TRUE)

cat("小于 0 的比例 =", round(prop_less_0, 3), "\n")
cat("大于 0 的比例 =", round(prop_greater_0, 3), "\n")

# 4. 计算95%分位数范围（用于坐标限制）
x_min <- quantile(interpolation, 0.025, na.rm = TRUE)
x_max <- quantile(interpolation, 0.975, na.rm = TRUE)
# 5. 计算密度曲线
dens <- density(interpolation, na.rm = TRUE)
df_dens <- data.frame(x = dens$x, y = dens$y)

# 6. 绘图
p <- ggplot() +
  # 左边填充 (x < 0, 更浅)
  geom_area(data = subset(df_dens, x < 0),
            aes(x = x, y = y), fill = "orange", alpha = 0.4) +
  # 右边填充 (x >= 0, 更深)
  geom_area(data = subset(df_dens, x >= 0),
            aes(x = x, y = y), fill = "orange", alpha = 0.8) +
  # 黑色虚线 x=0
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 0.1)) +   # 限制横纵坐标范围
  #labs(x = "∆LST(℃)", y = "Density") +
  theme_minimal(base_size = 35) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5), # 外边框加粗
    axis.line = element_line(color = "black", linewidth = 1.0),
    axis.ticks = element_line(color = "black", linewidth = 1.0),
    axis.text = element_text(color = "black", size = 35),
    #axis.title = element_text(color = "black", size = 35),
    axis.title = element_blank(),                           # 隐藏坐标标题
    plot.background = element_rect(fill = "white", color = NA)
  )

# 7. 显示图形
print(p)

# 8. 保存图像
ggsave("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/data.summary/∆PPT_day_edge_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------分区裁剪
library(terra)

# 1. 输入路径
shp_dir <- "D:/Forest_Fragmentation/中国标准地图-审图号GS(2020)4619号-shp格式/六大地理分区"
tif_path <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_raw/PPT_Nat_2020.tif"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/NatPPT_2020_region"

# 创建输出文件夹（如果不存在）
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2. 读取参考tif
r <- rast(tif_path)

# 3. 获取所有shp文件路径
shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)

# 检查
print(shp_files)

# 4. 批量裁剪并导出
for (shp in shp_files) {
  # 读取shp
  region <- vect(shp)
  
  # 将shp投影转换到栅格坐标系
  region <- project(region, crs(r))
  
  # 裁剪
  r_crop <- crop(r, region, mask = TRUE)
  
  # 生成输出文件名（根据shp文件名）
  region_name <- tools::file_path_sans_ext(basename(shp))  # 取文件名去掉扩展名
  out_file <- file.path(out_dir, paste0("NatPPT_2020_", region_name, ".tif"))
  
  # 保存
  writeRaster(r_crop, out_file, overwrite = TRUE)
  
  cat("已保存:", out_file, "\n")
}

cat("✅ 所有6个分区裁剪完成！\n")
# ------------------------------------------------------------------------------批量聚合
library(terra)

# 1. 输入目录（换成 PPT 栅格的六大分区）
tif_dir <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/NatPPT_2020_region"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/summary_csv"

# 创建输出文件夹（如果不存在）
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2. 获取所有TIF文件路径
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

# 检查
print(tif_files)

# 3. 设置聚合因子（假设原始分辨率是1km -> 9km）
fact <- 9

# 4. 批量处理
for (tif in tif_files) {
  # 读取栅格
  r <- rast(tif)
  
  # 计算四个统计量
  r_max    <- aggregate(r, fact=fact, fun=max,    na.rm=TRUE)
  r_min    <- aggregate(r, fact=fact, fun=min,    na.rm=TRUE)
  r_mean   <- aggregate(r, fact=fact, fun=mean,   na.rm=TRUE)
  r_median <- aggregate(r, fact=fact, fun=median, na.rm=TRUE)
  
  # 合并为多波段栅格
  r_summary <- c(r_max, r_min, r_mean, r_median)
  names(r_summary) <- c("max", "min", "mean", "median")
  
  # 转换为数据框，保留NA格
  df <- as.data.frame(r_summary, xy=TRUE, na.rm=FALSE)
  
  # 生成输出文件名（基于TIF文件名）
  region_name <- tools::file_path_sans_ext(basename(tif))
  out_file <- file.path(out_dir, paste0(region_name, "_9km_summary.csv"))
  
  # 保存CSV
  write.csv(df, out_file, row.names = FALSE, na = "NA")
  
  cat("已处理并保存:", out_file, "\n")
}

cat("✅ 所有区域完成9km聚合并导出CSV！\n")
# ------------------------------------------------------------------------批量数据清洗
library(stringr)

# 1. 文件夹路径（改为 PPT 汇总文件夹）
csv_dir <- "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/summary_csv"
out_dir <- file.path(csv_dir, "clean")
if (!dir.exists(out_dir)) dir.create(out_dir)

# 2. 获取所有CSV文件
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 提取区域标识（EA、NE、NW...）
regions <- unique(str_extract(basename(csv_files), "_(EA|NE|NW|NO|SO|SW)_"))
regions <- gsub("_", "", regions)  # 去掉下划线

# 4. 循环每个区域处理
for (region in regions) {
  # 找到当前区域对应的两个文件（Hum & Nat）
  hum_file <- csv_files[grepl(paste0("Hum.*", region), csv_files)]
  nat_file <- csv_files[grepl(paste0("Nat.*", region), csv_files)]
  
  if (length(hum_file) == 1 && length(nat_file) == 1) {
    cat("处理区域:", region, "\n")
    
    # 读取数据
    hum <- read.csv(hum_file)
    nat <- read.csv(nat_file)
    
    # 检查行数一致
    if (nrow(hum) != nrow(nat)) {
      warning("文件行数不一致：", hum_file, " & ", nat_file)
    }
    
    # 找到两个文件中都完整的行
    valid_rows <- complete.cases(hum) & complete.cases(nat)
    
    # 筛选
    hum_clean <- hum[valid_rows, ]
    nat_clean <- nat[valid_rows, ]
    
    # 输出文件名
    hum_out <- file.path(out_dir, paste0("Hum_", region, "_clean.csv"))
    nat_out <- file.path(out_dir, paste0("Nat_", region, "_clean.csv"))
    
    write.csv(hum_clean, hum_out, row.names = FALSE)
    write.csv(nat_clean, nat_out, row.names = FALSE)
    
    cat("✅ 已输出:", hum_out, " 和 ", nat_out, "\n")
  } else {
    warning("区域 ", region, " 找不到完整的Hum/Nat文件！")
  }
}

cat("✅ 所有区域处理完成，结果保存在: ", out_dir, "\n")

# ------------------------------------------------------------------------------分区差值作图优化版
library(ggplot2)

# 1. 读取清理后的两个 CSV 文件
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/summary_csv/clean/Hum_NE_clean.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/summary_csv/clean/Nat_NE_clean.csv")

# 2. 取mean列，计算插值（差值）
interpolation <- hum_clean$mean - nat_clean$mean

# 2.1 进行配对 t 检验（假设每个 Hum 与 Nat 栅格一一对应）
hum_mean <- hum_clean$mean
nat_mean <- nat_clean$mean

t_test_result <- t.test(hum_mean, nat_mean, paired = TRUE)  # 默认 paired=FALSE
cat("T 检验结果：\n")
print(t_test_result)
cat("p值 =", t_test_result$p.value, "\n")

# 3. 计算比例（小于0和大于0）
prop_less_0 <- mean(interpolation < 0, na.rm = TRUE)
prop_greater_0 <- mean(interpolation > 0, na.rm = TRUE)

cat("小于 0 的比例 =", round(prop_less_0, 3), "\n")
cat("大于 0 的比例 =", round(prop_greater_0, 3), "\n")

# 4. 计算95%分位数范围（用于坐标限制）
x_min <- quantile(interpolation, 0.025, na.rm = TRUE)
x_max <- quantile(interpolation, 0.975, na.rm = TRUE)
# 5. 计算密度曲线
dens <- density(interpolation, na.rm = TRUE)
df_dens <- data.frame(x = dens$x, y = dens$y)

# 6. 绘图
p <- ggplot() +
  # 左边填充 (x < 0, 更浅)
  geom_area(data = subset(df_dens, x < 0),
            aes(x = x, y = y), fill = "orange", alpha = 0.4) +
  # 右边填充 (x >= 0, 更深)
  geom_area(data = subset(df_dens, x >= 0),
            aes(x = x, y = y), fill = "orange", alpha = 0.8) +
  # 黑色虚线 x=0
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 0.1)) +   # 限制横纵坐标范围
  #coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, max(df_dens$y) * 1.05)) +
  #labs(x = "∆LST(℃)", y = "Density") +
  theme_minimal(base_size = 35) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5), # 外边框加粗
    axis.line = element_line(color = "black", linewidth = 1.0),
    axis.ticks = element_line(color = "black", linewidth = 1.0),
    axis.text = element_text(color = "black", size = 35),
    #axis.title = element_text(color = "black", size = 35),
    axis.title = element_blank(),                           # 隐藏坐标标题
    plot.background = element_rect(fill = "white", color = NA)
  )

# 7. 显示图形
print(p)

# 8. 保存图像
ggsave("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT_2020_six_region/summary_csv/clean/∆PPT_day_2020_NE_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------SSM和PPT相关性数据清洗
# ==== 1. 读取数据 ====
hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/PPT_Hum_2020_9km.csv")
nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/SSM_Hum_2020_9km.csv")

# ==== 2. 检查两个数据框的行数是否一致 ====
if (nrow(hum) != nrow(nat)) {
  stop("错误：两个文件的行数不一致，无法逐行对应进行清洗！")
}

# ==== 3. 找出同时无 NA 的行 ====
valid_rows <- complete.cases(hum) & complete.cases(nat)

# ==== 4. 筛选出无 NA 的行 ====
hum_clean <- hum[valid_rows, ]
nat_clean <- nat[valid_rows, ]

# ==== 5. 输出清洗后的行数 ====
cat("清洗后行数:", nrow(hum_clean), "（原行数:", nrow(hum), ")\n")

# ==== 6. 如需保存清洗后的结果 ====
write.csv(hum_clean, "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/PPT_Hum_2020_9km_clean_相关性.csv", row.names = FALSE)
write.csv(nat_clean, "D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/SSM_Hum_2020_9km_clean_相关性.csv", row.names = FALSE)

cat("清洗后的文件已保存。\n")
# ------------------------------------------------------------------------------SSM和PPT相关性拟合
library(ggplot2)

# ==== 1. 读取清洗后的数据 ====
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/PPT_Hum_2020_9km_clean_相关性.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/SSM_Hum_2020_9km_clean_相关性.csv")

# ==== 2. 提取第 5 列（mean） ====
x <- hum_clean[, 5]
y <- nat_clean[, 5]

# ==== 3. 线性回归 ====
lm_model <- lm(y ~ x)
summary(lm_model)

# ==== 4. 整理画图数据 ====
df <- data.frame(Hum = x, Nat = y)

# ==== 5. Sci.Adv 风格绘图 ====
p <- ggplot(df, aes(x = Hum, y = Nat)) +
  geom_point(size = 1.8, alpha = 0.7, color = "#ADFF2F") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, color = "black") +
  labs(
    x = "PPT",
    y = "SSM"
  ) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.5, 0.1)) +   # ★★★ 添加这一行 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1.2),
    axis.ticks = element_line(color = "black", linewidth = 1.2),
    axis.ticks.length = unit(0.2, "cm"),
    text = element_text(size = 34, color = "black"),
    axis.title = element_text(size = 34, color = "black"),
    axis.text = element_text(size = 34, color = "black"),
    plot.margin = margin(10, 30, 10, 10)
  )


print(p)
# ==== 6. 保存高分辨率图片 ====
ggsave("D:/Forest_Fragmentation/人为和自然归因/PPT_edge2.0/PPT和SSM相关性/PPT_SSM_regression_Hum.png",
       p, width = 10, height = 9, dpi = 600)



