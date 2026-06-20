# ------------------------------------------------------------------------------
library(terra)

# 设定基础路径
folder_path <- "D:/Forest_Fragmentation/NPP/CASAPKU/NPP"

# 定义函数：按年份计算年总 NPP（单位仍为 gC/m²）
calc_annual_sum <- function(year) {
  # 匹配文件名，如 NPP_2020_1.tif 到 NPP_2020_12.tif
  pattern <- paste0("^NPP_", year, "_(1[0-2]|[1-9])\\.tif$")
  npp_files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  # 读取并堆叠
  npp_stack <- rast(npp_files)
  
  # 逐像元求和（即全年总 NPP）
  npp_annual_sum <- sum(npp_stack, na.rm = TRUE)
  
  # 保存输出
  output_path <- file.path(folder_path, paste0("NPP_", year, "_annual_sum.tif"))
  writeRaster(npp_annual_sum, output_path, overwrite = TRUE)
  
  cat("✅ 年总 NPP 栅格已保存到：", output_path, "\n")
}

# 分别计算 2022 和 2021 年
calc_annual_sum(2020)
calc_annual_sum(2021)

# ================================================================
library(terra)

# 输入路径
shp_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/shp"
input_folder <- "D:/Forest_Fragmentation/NPP/CASA_China/YZ_Feng"
output_base <- "D:/Forest_Fragmentation/NPP/CASA_China/YZ_Feng/1"

# 六大地理分区
regions <- c("NW", "NO", "NE", "EA", "SO", "SW")

# 获取所有栅格文件路径
raster_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# 循环每个栅格文件（即每年）
for (raster_path in raster_files) {
  # 提取年份
  file_name <- basename(raster_path)
  year <- gsub(".*_(\\d{4})_.*", "\\1", file_name)
  
  cat("\n📦 当前处理年份：", year, "\n")
  
  # 加载栅格
  r <- rast(raster_path)
  
  # 循环每个区域
  for (region in regions) {
    cat("📍 正在处理区域：", region, "\n")
    
    # 加载对应 shapefile
    shp_path <- file.path(shp_dir, paste0(region, ".shp"))
    region_shp <- vect(shp_path)
    
    # 匹配坐标系
    region_shp_match <- project(region_shp, crs(r))
    
    # 裁剪并掩膜
    r_crop <- crop(r, region_shp_match)
    r_mask <- mask(r_crop, region_shp_match)
    
    # 重投影为 EPSG:4326
    r_out <- project(r_mask, "EPSG:4326", method = "bilinear")
    
    # 输出目录和文件名
    out_dir <- file.path(output_base, region)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    out_file <- file.path(out_dir, paste0(region, "_", year, ".tif"))
    writeRaster(r_out, out_file, overwrite = TRUE)
    
    cat("✅ 已保存：", out_file, "\n")
  }
}

# ==============================================================================
# 获取当前 R 使用的临时目录路径
r_tempdir <- tempdir()
# 打印结果
cat("📂 当前 R 临时目录：", r_tempdir, "\n")



# ==============================================================================

library(terra)

# 地理分区代号
regions <- c("EA", "NE", "NO", "NW", "SO", "SW")

# 主目录
base_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/YZ_Feng/1"

# 遍历每个分区
for (region in regions) {
  message("📍 正在处理区域: ", region)
  
  # 子文件夹路径
  region_dir <- file.path(base_dir, region)
  
  # 参考 500m 栅格
  ref_raster_path <- file.path(base_dir, "NPP_500m_new", paste0("NPP_500m_", region, ".tif"))
  ref_raster <- rast(ref_raster_path)
  
  # 匹配区域下所有 .tif 文件（假设每个文件都要处理）
  tif_files <- list.files(region_dir, pattern = "\\.tif$", full.names = TRUE)
  
  for (tif_path in tif_files) {
    # 加载原始栅格
    src <- rast(tif_path)
    
    # 重采样
    resampled <- resample(src, ref_raster, method = "bilinear")
    
    # 裁剪
    cropped <- crop(resampled, ref_raster)
    clipped <- mask(cropped, ref_raster)
    
    # 构建输出路径
    fname <- tools::file_path_sans_ext(basename(tif_path))
    out_path <- file.path(region_dir, paste0(fname, "_resampled_clipped.tif"))
    
    # 保存
    writeRaster(clipped, out_path, overwrite = TRUE)
    message("✅ 已完成: ", basename(out_path))
  }
}

message("🎉 所有分区处理完成。")


# ------------------------------------------------------------------------------
library(terra)

# 计算 NPP 总碳量的函数
calc_total_npp_TgC <- function(npp_path) {
  npp <- rast(npp_path)
  area_raster <- cellSize(npp, unit = "m")
  total_carbon <- npp * area_raster
  total_gC <- global(total_carbon, "sum", na.rm = TRUE)[1, 1]
  total_TgC <- total_gC / 1e12
  return(total_TgC)
}

# 设置区域和主目录
regions <- c("EA", "NE", "NO", "NW", "SO", "SW")
base_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/YZ_Feng/1"

# 初始化结果表
results <- data.frame(
  Region = character(),
  File = character(),
  Total_TgC = numeric(),
  stringsAsFactors = FALSE
)

# 遍历每个区域文件夹
for (region in regions) {
  region_dir <- file.path(base_dir, region)
  
  # 找出裁剪后的文件（匹配 *_resampled_clipped.tif）
  tif_files <- list.files(region_dir, pattern = "_resampled_clipped\\.tif$", full.names = TRUE)
  
  for (file in tif_files) {
    total_TgC <- calc_total_npp_TgC(file)
    results <- rbind(results, data.frame(
      Region = region,
      File = basename(file),
      Total_TgC = total_TgC
    ))
    message("✅ 已完成: ", basename(file), " -> ", round(total_TgC, 3), " TgC")
  }
}

# 打印和保存
print(results)
write.csv(results, file.path(base_dir, "NPP_Total_TgC_by_File.csv"), row.names = FALSE)
message("📄 已保存总NPP统计结果到 CSV。")


# ------------------------------------------------------------------------------验证相关性
# 输出图像
tiff(
  "D:/Forest_Fragmentation/NPP/CASA_China/YZ_Feng/1//Regional_NPP_comparison.tif",
  width = 100,      # mm
  height = 90,
  units = "mm",
  res = 900,
  compression = "lzw"
)

# 数据输入
dat <- data.frame(
  Region = c("NW","NE","NO","SW","SO","EA"),
  Opt2022 = c(83.085,171.977,113.019,429.884,351.207,217.890),
  Feng = c(95.252,188.742,114.004,633.591,504.826,263.233)
)

# Pearson相关
cor.test(dat$Opt2022, dat$Feng, method = "pearson")

# 线性回归
fit <- lm(Feng ~ Opt2022, data = dat)

R2 <- summary(fit)$r.squared

# 设置边距
par(
  mar = c(5, 5, 1, 1),   # 下、左、上、右
  lwd = 2                    # 外框加粗
)

plot(
  dat$Opt2022,
  dat$Feng,
  pch = 19,
  cex = 0.5,
  axes = FALSE,
  frame.plot = TRUE,
  xlab = "NPP (This study, Tg C)",
  ylab = "NPP (Feng et al., Tg C)",
  main = "",
  cex.lab = 1.2,
  font.lab = 1   # 坐标轴标题不加粗
)

# 自定义坐标轴
axis(
  side = 1,
  lwd = 1.5,
  lwd.ticks = 1.5,
  cex.axis = 1.1,
  font = 1,      # 正常字体
  tcl = -0.25
)

axis(
  side = 2,
  lwd = 1.5,
  lwd.ticks = 1.5,
  cex.axis = 1.1,
  font = 1,
  las = 1,
  tcl = -0.25
)

# 边框
box(lwd = 1.0)

# 区域名称
text(
  dat$Opt2022,
  dat$Feng,
  labels = dat$Region,
  pos = c(4,4,4,2,2,4),
  cex = 1.0,      # 或1.1
  font = 1        # 不加粗
)

# 回归线
abline(
  fit,
  lwd = 2
)

# 1:1线
abline(
  0,
  1,
  lty = 2,
  lwd = 1.5
)

# 图例
legend(
  "topleft",
  legend = c(
    paste0("R² = ", round(R2, 3)),
    "P < 0.001"
  ),
  bty = "n",
  cex = 1.0,
  text.font = 1
)

dev.off()
