# ------------------------------------------------------------------------------Hum/Nat edge掩膜提取
rm(list = ls())
gc(reset = TRUE)

library(terra)

# 1. 读取掩膜栅格（Hum Edge）
mask_path <- "D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Hum_Edge_raw2.0/2020_1km/Forest_to_AnthroEdge_2020_1km.tif"
mask <- rast(mask_path)

# 2. 读取 AET 栅格（2020 年均值）
AET_path <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_raw/AET_2020_mean_monthly.tif"
AET <- rast(AET_path)

# 3. 检查 CRS 是否一致，不一致则重投影 AET 到 mask 的 CRS
if (!compareGeom(AET, mask, crs = TRUE, stopOnError = FALSE)) {
  message("CRS 不一致，进行重投影...")
  AET <- project(AET, mask, method = "near")
} else {
  message("CRS 一致，无需重投影")
}

# 4. 重采样对齐（确保栅格分辨率和范围匹配）
AET_aligned <- resample(AET, mask, method = "near")

# 5. 掩膜操作（保留 mask == 1 区域，其他设 NA）
AET_masked <- mask(AET_aligned, mask, maskvalues = 0, updatevalue = NA)

# 6. 输出路径
output_path <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_raw/AET_Hum_2020.tif"

# 7. 写出结果，保存为浮点型
writeRaster(AET_masked, output_path, datatype = "FLT4S", NAflag = -9999, overwrite = TRUE)

cat("✅ 掩膜后的 AET 数据已保存至：", output_path, "\n")

# ------------------------------------------------------------------------------9km聚合提取 (AET)
library(terra)

# 1. 读取你的 1km 栅格数据（AET 人为边缘）
r <- rast("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_raw/AET_Nat_2020.tif")

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
output_csv <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Nat_2020_9km.csv"
write.csv(df, output_csv, row.names = FALSE, na = "NA")

cat("✅ 9km 聚合统计已保存至：", output_csv, "\n")

# ------------------------------------------------------------------------------9km聚合提取类型合并，去除NA (AET)
# 1. 读取两个 CSV 文件
hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Hum_2020_9km.csv")
nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Nat_2020_9km.csv")

# 2. 找出两个文件中都没有 NA 的行
valid_rows <- complete.cases(hum) & complete.cases(nat)

# 3. 筛选出无NA的行
hum_clean <- hum[valid_rows, ]
nat_clean <- nat[valid_rows, ]

# 4. 可选：确保行数一致
stopifnot(nrow(hum_clean) == nrow(nat_clean))

# 5. 写出为新的 CSV 文件
write.csv(hum_clean, "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Hum_2020_9km_clean.csv", row.names = FALSE)
write.csv(nat_clean, "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Nat_2020_9km_clean.csv", row.names = FALSE)

cat("✅ 清洗后的 AET 数据已保存。\n")

# ------------------------------------------------------------------------------差值作图优化版
library(ggplot2)

# 1. 读取清理后的两个 CSV 文件
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Hum_2020_9km_clean.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/AET_Nat_2020_9km_clean.csv")

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
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, max(df_dens$y) * 1.05)) +
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
ggsave("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/data.summary/∆AET_day_edge_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------分区裁剪
library(terra)

# 1. 输入路径
shp_dir <- "D:/Forest_Fragmentation/中国标准地图-审图号GS(2020)4619号-shp格式/六大地理分区"
tif_path <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_raw/AET_Hum_2020.tif"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/HumAET_2020_region"

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
  out_file <- file.path(out_dir, paste0("HumAET_2020_", region_name, ".tif"))
  
  # 保存
  writeRaster(r_crop, out_file, overwrite = TRUE)
  
  cat("已保存:", out_file, "\n")
}

cat("✅ 所有6个分区裁剪完成！\n")
# ------------------------------------------------------------------------------批量聚合
library(terra)

# 1. 输入目录（换成 AET 栅格的六大分区）
tif_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/HumAET_2020_region"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv"

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

# 1. 文件夹路径（改为 AET 汇总文件夹）
csv_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv"
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
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv/clean/Hum_NE_clean.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv/clean/Nat_NE_clean.csv")

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
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 0.2)) +   # 限制横纵坐标范围
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
ggsave("D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv/clean/∆AET_day_2020_NE_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")

# ============================================================================================================================================================= core
# ------------------------------------------------------------------------------Hum/Nat edge掩膜提取
rm(list = ls())
gc(reset = TRUE)

library(terra)

# 1. 读取掩膜栅格（Hum Edge）
mask_path <- "D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Nat_core_raw/2020_1km/Nat_core_2020.tif"
mask <- rast(mask_path)

# 2. 读取 AET 栅格（2020 年均值）
AET_path <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_raw/AET_2020_mean_monthly.tif"
AET <- rast(AET_path)

# 3. 检查 CRS 是否一致，不一致则重投影 AET 到 mask 的 CRS
if (!compareGeom(AET, mask, crs = TRUE, stopOnError = FALSE)) {
  message("CRS 不一致，进行重投影...")
  AET <- project(AET, mask, method = "near")
} else {
  message("CRS 一致，无需重投影")
}

# 4. 重采样对齐（确保栅格分辨率和范围匹配）
AET_aligned <- resample(AET, mask, method = "near")

# 5. 掩膜操作（保留 mask == 1 区域，其他设 NA）
AET_masked <- mask(AET_aligned, mask, maskvalues = 0, updatevalue = NA)

# 6. 输出路径
output_path <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_raw/AET_Nat_2020.tif"

# 7. 写出结果，保存为浮点型
writeRaster(AET_masked, output_path, datatype = "FLT4S", NAflag = -9999, overwrite = TRUE)

cat("✅ 掩膜后的 AET 数据已保存至：", output_path, "\n")

# ------------------------------------------------------------------------------9km聚合提取 (AET)
library(terra)

# 1. 读取你的 1km 栅格数据（AET 人为边缘）
r <- rast("D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_raw/AET_Hum_2020.tif")

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
output_csv <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/data.summary/AET_Hum_2020_9km.csv"
write.csv(df, output_csv, row.names = FALSE, na = "NA")

cat("✅ 9km 聚合统计已保存至：", output_csv, "\n")

# ------------------------------------------------------------------------------分区裁剪
library(terra)

# 1. 输入路径
shp_dir <- "D:/Forest_Fragmentation/中国标准地图-审图号GS(2020)4619号-shp格式/六大地理分区"
tif_path <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_raw/AET_Nat_2020.tif"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_six_region/NatAET_2020_region"


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
  out_file <- file.path(out_dir, paste0("NatAET_2020_", region_name, ".tif"))
  
  # 保存
  writeRaster(r_crop, out_file, overwrite = TRUE)
  
  cat("已保存:", out_file, "\n")
}

cat("✅ 所有6个分区裁剪完成！\n")

# ------------------------------------------------------------------------------批量聚合
library(terra)

# 1. 输入目录
tif_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_six_region/HumAET_2020_region"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_six_region/summary_csv"

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
# ============================================================================================================================================================= edge&core
# ------------------------------------------------------------------------------ 9km 聚合提取类型合并，去除 NA（四个文件同步）
# 1. 读取四个 CSV 文件
hum_core <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_core_2020_9km.csv")
hum_edge <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_edge_2020_9km.csv")
nat_core <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Nat_core_2020_9km.csv")
nat_edge <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Nat_edge_2020_9km.csv")

# 2. 找出所有文件中都没有 NA 的行
valid_rows <- complete.cases(hum_core) & complete.cases(hum_edge) & complete.cases(nat_core) & complete.cases(nat_edge)

# 3. 筛选出无 NA 的行
hum_core_clean <- hum_core[valid_rows, ]
hum_edge_clean <- hum_edge[valid_rows, ]
nat_core_clean <- nat_core[valid_rows, ]
nat_edge_clean <- nat_edge[valid_rows, ]

# 4. 可选检查行数是否一致
stopifnot(
  nrow(hum_core_clean) == nrow(hum_edge_clean),
  nrow(hum_edge_clean) == nrow(nat_core_clean),
  nrow(nat_core_clean) == nrow(nat_edge_clean)
)

# 5. 写出为新的 CSV 文件
write.csv(hum_core_clean, "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_core_2020_9km_clean.csv", row.names = FALSE)
write.csv(hum_edge_clean, "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_edge_2020_9km_clean.csv", row.names = FALSE)
write.csv(nat_core_clean, "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Nat_core_2020_9km_clean.csv", row.names = FALSE)
write.csv(nat_edge_clean, "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Nat_edge_2020_9km_clean.csv", row.names = FALSE)


# ------------------------------------------------------------------------------ 逐列差值（Edge - Core）
# 1. 读取两个清洗后的 CSV 文件
hum_core_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_core_2020_9km_clean.csv")
hum_edge_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_edge_2020_9km_clean.csv")

# 2. 检查行列是否匹配
stopifnot(nrow(hum_core_clean) == nrow(hum_edge_clean))
stopifnot(ncol(hum_core_clean) == ncol(hum_edge_clean))

# 3. 前两列为坐标，直接保留；从第3列开始做差值
coord_cols <- hum_edge_clean[, 1:2]
diff_cols <- hum_edge_clean[, 3:ncol(hum_edge_clean)] - hum_core_clean[, 3:ncol(hum_core_clean)]

# 4. 合并坐标列和差值列
hum_diff_all <- cbind(coord_cols, diff_cols)

# 5. 输出结果到新文件
write.csv(hum_diff_all, "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_HumEdge_minus_Core_2020_9km.csv", row.names = FALSE)
# ------------------------------------------------------------------------------差值作图优化版
library(ggplot2)

# 1. 读取清理后无NA的两个CSV文件
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_HumEdge_minus_Core_2020_9km.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_NatEdge_minus_Core_2020_9km.csv")

# 2. 取mean列，计算插值（差值）
interpolation <- hum_clean$mean - nat_clean$mean

hum_mean <- hum_clean$mean
nat_mean <- nat_clean$mean

# 配对 t 检验
t_test_result <- t.test(hum_mean, nat_mean, paired = TRUE)

#  查看结果
print(t_test_result)

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
            aes(x = x, y = y), fill = "skyblue", alpha = 0.4) +
  # 右边填充 (x >= 0, 更深)
  geom_area(data = subset(df_dens, x >= 0),
            aes(x = x, y = y), fill = "skyblue", alpha = 0.8) +
  # 黑色虚线 x=0
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, max(df_dens$y) * 1.05)) +
  #labs(x = "∆AET(℃)", y = "Density") +
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
ggsave("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/∆AET_day_edge&core_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")


# ------------------------------------------------------------------------------箱线图
library(ggplot2)
library(ggpubr)
library(dplyr)

# 1. 读取四个文件
hum_core <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_core_2020_9km_clean.csv")
hum_edge <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Hum_edge_2020_9km_clean.csv")
nat_core <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Nat_core_2020_9km_clean.csv")
nat_edge <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/china清洗/AET_Nat_edge_2020_9km_clean.csv")

# 2. 构造数据框
df_hum_core  <- data.frame(mean = hum_core$mean, group = "Hum-Core")
df_nat_core  <- data.frame(mean = nat_core$mean, group = "Nat-Core")
df_hum_edge  <- data.frame(mean = hum_edge$mean, group = "Hum-Edge")
df_nat_edge  <- data.frame(mean = nat_edge$mean, group = "Nat-Edge")
df_all <- rbind(df_hum_core, df_nat_core, df_hum_edge, df_nat_edge)

df_all %>%
  group_by(group) %>%
  summarise(median_value = format(round(median(mean, na.rm = TRUE), 2), nsmall = 2))

# 3. 设置比较组
my_comparisons <- list(
  c("Hum-Core", "Nat-Core"),
  c("Hum-Edge", "Nat-Edge"),
  c("Hum-Core", "Hum-Edge"),
  c("Nat-Core", "Nat-Edge")
)

# 4. 绘制箱线图 + 显著性比较
p <- ggplot(df_all, aes(x = group, y = mean, fill = group)) +
  geom_boxplot(width = 0.6, outlier.size = 0.75, linewidth = 1) +
  scale_fill_manual(values = c(
    "Hum-Core"  = "#E41A1C",
    "Nat-Core"  = "#377EB8",
    "Hum-Edge"  = "#cc4c02",
    "Nat-Edge"  = "#01665e"
  )) +
  labs(x = NULL, y = expression(AET~(mm))) +
  coord_cartesian(ylim = c(0, 165)) +
  scale_y_continuous(breaks = seq(0, 165, by = 40)) +
  theme_minimal(base_size = 35) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2.5), # 外框加粗
    axis.line = element_line(color = "black", linewidth = 1.5),               # 坐标轴线加粗
    axis.ticks = element_line(color = "black", linewidth = 1.2),
    axis.text = element_text(color = "black", size = 35),                     # 横纵坐标文字黑色
    axis.title = element_text(color = "black", size = 35),                    # 坐标标题黑色
    axis.text.x = element_text(angle = 48, hjust = 1, color = "black"),       # 横坐标倾斜
    legend.position = "none"
  ) +
  stat_compare_means(
    comparisons = my_comparisons,
    method = "t.test",
    label = "p.signif",
    step.increase = 0.1,
    size = 10,    # 显著性符号字体大小
    tip.length = 0.01
  )

# 显示图
print(p)


ggsave("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/AET_China_2020.png",
       plot = p, width = 10, height = 10, dpi = 600 ,bg = "white")

# ------------------------------------------------------------------------------ 9km 聚合提取类型合并，去除 NA（四个文件同步）批量
library(stringr)

# 1. 文件夹路径
hum_edge_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv"   # 人为边缘
nat_edge_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge2.0/AET_2020_six_region/summary_csv"       # 自然边缘
hum_core_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_six_region/summary_csv"    # 人为核心
nat_core_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_core/AET_2020_six_region/summary_csv"       # 自然核心

# 2. 所有 CSV 文件
hum_edge_files <- list.files(hum_edge_dir, pattern = "\\.csv$", full.names = TRUE)
nat_edge_files <- list.files(nat_edge_dir, pattern = "\\.csv$", full.names = TRUE)
hum_core_files <- list.files(hum_core_dir, pattern = "\\.csv$", full.names = TRUE)
nat_core_files <- list.files(nat_core_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 区域代号
regions <- c("EA", "NE", "NW", "NO", "SO", "SW")

# 4. 输出路径
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 5. 清洗每个区域
for (region in regions) {
  # 构造文件名匹配（注意匹配 summary 和 edge 文件）
  hum_core_file <- hum_core_files[str_detect(hum_core_files, paste0("HumAET_2020_", region, "_9km_summary\\.csv"))]
  hum_edge_file <- hum_edge_files[str_detect(hum_edge_files, paste0("HumAET_2020_", region, "_9km_summary\\.csv"))]
  nat_core_file <- nat_core_files[str_detect(nat_core_files, paste0("NatAET_2020_", region, "_9km_summary\\.csv"))]
  nat_edge_file <- nat_edge_files[str_detect(nat_edge_files, paste0("NatAET_2020_", region, "_9km_summary\\.csv"))]
  
  # 检查文件存在
  if (length(hum_core_file) == 0 | length(hum_edge_file) == 0 | 
      length(nat_core_file) == 0 | length(nat_edge_file) == 0) {
    warning("⚠️ 缺失某些区域文件：", region)
    next
  }
  
  # 读取 CSV
  hum_core <- read.csv(hum_core_file)
  hum_edge <- read.csv(hum_edge_file)
  nat_core <- read.csv(nat_core_file)
  nat_edge <- read.csv(nat_edge_file)
  
  # 清洗：四个文件中都完整的行
  valid_rows <- complete.cases(hum_core) & complete.cases(hum_edge) &
    complete.cases(nat_core) & complete.cases(nat_edge)
  
  hum_core_clean <- hum_core[valid_rows, ]
  hum_edge_clean <- hum_edge[valid_rows, ]
  nat_core_clean <- nat_core[valid_rows, ]
  nat_edge_clean <- nat_edge[valid_rows, ]
  
  # 写出
  write.csv(hum_core_clean, file.path(out_dir, paste0("HumCore_", region, "_clean.csv")), row.names = FALSE)
  write.csv(hum_edge_clean, file.path(out_dir, paste0("HumEdge_", region, "_clean.csv")), row.names = FALSE)
  write.csv(nat_core_clean, file.path(out_dir, paste0("NatCore_", region, "_clean.csv")), row.names = FALSE)
  write.csv(nat_edge_clean, file.path(out_dir, paste0("NatEdge_", region, "_clean.csv")), row.names = FALSE)
  
  message("✅ 完成区域：", region)
}
# ------------------------------------------------------------------------------ 批量计算 Edge - Core 差值代码如下
library(stringr)

# 设置输入输出路径
input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗"
output_dir <- "D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗"

# 设置区域名称
regions <- c("EA", "NE", "NW", "NO", "SO", "SW")
types   <- c("Hum", "Nat")

# 批量处理
for (region in regions) {
  for (type in types) {
    
    # 构造文件名
    core_file <- file.path(input_dir, paste0(type, "Core_", region, "_clean.csv"))
    edge_file <- file.path(input_dir, paste0(type, "Edge_", region, "_clean.csv"))
    
    # 读取 CSV
    core_data <- read.csv(core_file)
    edge_data <- read.csv(edge_file)
    
    # 检查维度一致
    stopifnot(nrow(core_data) == nrow(edge_data))
    stopifnot(ncol(core_data) == ncol(edge_data))
    
    # 计算差值
    coord_cols <- edge_data[, 1:2]
    diff_cols <- edge_data[, 3:ncol(edge_data)] - core_data[, 3:ncol(core_data)]
    diff_data <- cbind(coord_cols, diff_cols)
    
    # 写入结果
    output_file <- file.path(output_dir, paste0(type, "Edge_minus_Core_AET_", region, "_9km.csv"))
    write.csv(diff_data, output_file, row.names = FALSE)
    
    cat("✅ 已处理：", output_file, "\n")
  }
}
# ------------------------------------------------------------------------------差值作图优化版
library(ggplot2)

# 1. 读取清理后无NA的两个CSV文件
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗/HumEdge_minus_Core_AET_NW_9km.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗/NatEdge_minus_Core_AET_NW_9km.csv")

hum_mean <- hum_clean$mean
nat_mean <- nat_clean$mean

# 配对 t 检验
t_test_result <- t.test(hum_mean, nat_mean, paired = TRUE)

#  查看结果
print(t_test_result)

# 2. 取mean列，计算插值（差值）
interpolation <- hum_clean$mean - nat_clean$mean

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
            aes(x = x, y = y), fill = "skyblue", alpha = 0.4) +
  # 右边填充 (x >= 0, 更深)
  geom_area(data = subset(df_dens, x >= 0),
            aes(x = x, y = y), fill = "skyblue", alpha = 0.8) +
  # 黑色虚线 x=0
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 0.2)) +   # 限制横纵坐标范围
  #coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, max(df_dens$y) * 1.05)) +
  #labs(x = "∆AET(℃)", y = "Density") +
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
ggsave("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/∆AET_day_edge&core_NW_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")

# ------------------------------------------------------------------------------箱线图分区
library(ggplot2)
library(ggpubr)
library(dplyr)

# 1. 读取四个文件
hum_core <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗/HumCore_NE_clean.csv")
hum_edge <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗/HumEdge_NE_clean.csv")
nat_core <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗/NatCore_NE_clean.csv")
nat_edge <- read.csv("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/region清洗/NatEdge_NE_clean.csv")

# 2. 构造数据框
df_hum_core  <- data.frame(mean = hum_core$mean, group = "Hum-Core")
df_nat_core  <- data.frame(mean = nat_core$mean, group = "Nat-Core")
df_hum_edge  <- data.frame(mean = hum_edge$mean, group = "Hum-Edge")
df_nat_edge  <- data.frame(mean = nat_edge$mean, group = "Nat-Edge")
df_all <- rbind(df_hum_core, df_nat_core, df_hum_edge, df_nat_edge)

df_all %>%
  group_by(group) %>%
  summarise(median_value = format(round(median(mean, na.rm = TRUE), 2), nsmall = 2))

# 3. 设置比较组
my_comparisons <- list(
  c("Hum-Core", "Nat-Core"),
  c("Hum-Edge", "Nat-Edge"),
  c("Hum-Core", "Hum-Edge"),
  c("Nat-Core", "Nat-Edge")
)

# 4. 绘制箱线图 + 显著性比较
p <- ggplot(df_all, aes(x = group, y = mean, fill = group)) +
  geom_boxplot(width = 0.6,outlier.size = 0.75, linewidth = 1) +
  scale_fill_manual(values = c(
    "Hum-Core"  = "#E41A1C",
    "Nat-Core"  = "#377EB8",
    "Hum-Edge"  = "#cc4c02",
    "Nat-Edge"  = "#01665e"
  )) +
  labs(x = NULL, y = expression(AET~(mm))) +
  coord_cartesian(ylim = c(0, 165)) +   # 限制纵坐标最大值为40
  scale_y_continuous(breaks = seq(0, 165, by = 40)) +  # 每隔20一个刻度
  theme_minimal(base_size = 32) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    axis.line = element_blank(),  # 取消默认轴线
    axis.ticks = element_line(color = "black", linewidth = 1.0),
    axis.text.x = element_text(angle = 48, hjust = 1),  # 横坐标标签倾斜
    legend.position = "none"
  ) +
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test", 
                     label = "p.signif",
                     step.increase = 0.1,
                     size = 10,  # 修改显著性符号 * 的字体大小
                     tip.length = 0.01)

# 5. 显示图
print(p)

ggsave("D:/Forest_Fragmentation/人为和自然归因/AET_edge&core/AET_NE_2020.png",
       plot = p, width = 10, height = 10, dpi = 600 ,bg = "white")
