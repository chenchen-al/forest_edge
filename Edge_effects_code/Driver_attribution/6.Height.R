# ------------------------------------------------------------------------------Hum/Nat edge掩膜提取
rm(list = ls())
gc(reset = TRUE)

library(terra)

# 1. 读取掩膜栅格（Hum Edge）
mask_path <- "D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Nat_Edge_core_0_1km/Nat_edge_core_0_1km_2020.tif"
mask <- rast(mask_path)

# 2. 读取 Height 栅格（2020 年均值）
Height_path <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_raw/CanopyHeight_China.tif"
Height <- rast(Height_path)

# 3. 检查 CRS 是否一致，不一致则重投影 Height 到 mask 的 CRS
if (!compareGeom(Height, mask, crs = TRUE, stopOnError = FALSE)) {
  message("CRS 不一致，进行重投影...")
  Height <- project(Height, mask, method = "near")
} else {
  message("CRS 一致，无需重投影")
}

# 4. 重采样对齐（确保栅格分辨率和范围匹配）
Height_aligned <- resample(Height, mask, method = "near")

# 5. 掩膜操作（保留 mask == 1 区域，其他设 NA）
Height_masked <- mask(Height_aligned, mask, maskvalues = 0, updatevalue = NA)

# 6. 输出路径
output_path <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_raw/Height_Nat_2020.tif"

# 7. 写出结果，保存为浮点型
writeRaster(Height_masked, output_path, datatype = "FLT4S", NAflag = -9999, overwrite = TRUE)

cat("✅ 掩膜后的 Height 数据已保存至：", output_path, "\n")

# ------------------------------------------------------------------------------9km聚合提取 (Height)
library(terra)

# 1. 读取你的 1km 栅格数据（Height 人为边缘）
r <- rast("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_raw/Height_Hum_2020.tif")

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
output_csv <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Hum_2020_9km.csv"
write.csv(df, output_csv, row.names = FALSE, na = "NA")

cat("✅ 9km 聚合统计已保存至：", output_csv, "\n")


# ------------------------------------------------------------------------------9km聚合提取类型合并，去除NA (Height)
# 1. 读取两个 CSV 文件
hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Hum_2020_9km.csv")
nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Nat_2020_9km.csv")

# 2. 找出两个文件中都没有 NA 的行
valid_rows <- complete.cases(hum) & complete.cases(nat)

# 3. 筛选出无NA的行
hum_clean <- hum[valid_rows, ]
nat_clean <- nat[valid_rows, ]

# 4. 可选：确保行数一致
stopifnot(nrow(hum_clean) == nrow(nat_clean))

# 5. 写出为新的 CSV 文件
write.csv(hum_clean, "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Hum_2020_9km_clean.csv", row.names = FALSE)
write.csv(nat_clean, "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Nat_2020_9km_clean.csv", row.names = FALSE)

cat("✅ 清洗后的 Height 数据已保存。\n")

# ------------------------------------------------------------------------------差值作图优化版
library(ggplot2)

# 1. 读取清理后的两个 CSV 文件
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Hum_2020_9km_clean.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/Height_Nat_2020_9km_clean.csv")

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
            aes(x = x, y = y), fill = "forestgreen", alpha = 0.4) +
  # 右边填充 (x >= 0, 更深)
  geom_area(data = subset(df_dens, x >= 0),
            aes(x = x, y = y), fill = "forestgreen", alpha = 0.8) +
  # 黑色虚线 x=0
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  # coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, max(df_dens$y) * 1.05)) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 0.25)) +   # 限制横纵坐标范围
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
ggsave("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/data.summary/∆Height_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")
# ------------------------------------------------------------------------------分区裁剪
library(terra)

# 1. 输入路径
shp_dir <- "D:/Forest_Fragmentation/中国标准地图-审图号GS(2020)4619号-shp格式/六大地理分区"
tif_path <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_raw/Height_Nat_2020.tif"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/NatHeight_2020_region"

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
  out_file <- file.path(out_dir, paste0("NatHeight_2020_", region_name, ".tif"))
  
  # 保存
  writeRaster(r_crop, out_file, overwrite = TRUE)
  
  cat("已保存:", out_file, "\n")
}

cat("✅ 所有6个分区裁剪完成！\n")
# ------------------------------------------------------------------------------批量聚合
library(terra)

# 1. 输入目录（换成 Height 栅格的六大分区）
tif_dir <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/NatHeight_2020_region"
out_dir <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/summary_csv"

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

# 1. 文件夹路径（改为 Height 汇总文件夹）
csv_dir <- "D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/summary_csv"
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
hum_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/summary_csv/clean/Hum_NE_clean.csv")
nat_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/summary_csv/clean/Nat_NE_clean.csv")

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
            aes(x = x, y = y), fill = "forestgreen", alpha = 0.4) +
  # 右边填充 (x >= 0, 更深)
  geom_area(data = subset(df_dens, x >= 0),
            aes(x = x, y = y), fill = "forestgreen", alpha = 0.8) +
  # 黑色虚线 x=0
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 0.25)) +   # 限制横纵坐标范围
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
ggsave("D:/Forest_Fragmentation/人为和自然归因/Tree_Height/Height_2020_six_region/summary_csv/clean/∆Height_2020_NE_优化版.png",
       plot = p, width = 10, height = 9, dpi = 600, bg = "white")
