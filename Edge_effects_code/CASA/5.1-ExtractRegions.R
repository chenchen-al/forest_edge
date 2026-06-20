library(terra)

# 设置路径
shp_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/shp"
input_folder <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China"
output_base <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part"

# 目标区域名称
regions <- c("NW", "NO", "NE", "EA", "SO", "SW")

# 获取所有 .tif 输入文件
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# 开始批量处理
for (region in regions) {
  cat("\n处理区域：", region, "\n")
  
  # 读取 shapefile（只读取一次）
  shp_path <- file.path(shp_dir, paste0(region, ".shp"))
  region_shp <- vect(shp_path)
  
  for (tif_file in tif_files) {
    r <- rast(tif_file)
    
    # 将 shapefile 重投影为 tif 的 CRS
    region_shp_match <- project(region_shp, crs(r))
    
    # 裁剪 + 掩膜
    r_crop <- crop(r, region_shp_match)
    r_mask <- mask(r_crop, region_shp_match)
    
    # 输出路径
    out_dir <- file.path(output_base, region)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    out_file <- file.path(out_dir, basename(tif_file))
    writeRaster(r_mask, out_file, overwrite = TRUE)
    
    cat("  已裁剪：", basename(tif_file), "\n")
  }
}

# -----------------------------------------------------------------------------
# 单个文件提取

library(terra)

# 设置路径
shp_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/shp"
input_raster <- "D:/Forest_Fragmentation/NPP/CASA_China/OptTEM/China/OptTEM_2023.tif"
output_base <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part"

# 六大地理分区
regions <- c("NW", "NO", "NE", "EA", "SO", "SW")

# 加载目标栅格
r <- rast(input_raster)

# 循环每个区域
for (region in regions) {
  cat("\n处理区域：", region, "\n")
  
  # 读取 shp 并重投影到栅格的 CRS
  shp_path <- file.path(shp_dir, paste0(region, ".shp"))
  region_shp <- vect(shp_path)
  region_shp_match <- project(region_shp, crs(r))
  
  # 裁剪 + 掩膜
  r_crop <- crop(r, region_shp_match)
  r_mask <- mask(r_crop, region_shp_match)
  
  # 输出路径
  out_dir <- file.path(output_base, region)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, basename(input_raster))  # 保持文件名一致
  
  # 保存
  writeRaster(r_mask, out_file, overwrite = TRUE)
  cat("  已保存至：", out_file, "\n")
}
