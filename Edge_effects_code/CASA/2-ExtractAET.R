# ------------------------------------------------------------------------------
# 实际蒸散发（AET） 的 每年逐月数据和年均值
# 清理环境
rm(list = ls())
gc(reset = TRUE)
library(terra)

# 路径设置
aet_nc_path <- "D:/Forest_Fragmentation/NPP/CASA_China/AET/Raw/TerraClimate_aet_2018.nc"
china_shp <- "D:/Forest_Fragmentation/NPP/CASA_China/shengjie/shengjie.shp"
output_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/AET"

# 读取数据
china_boundary <- vect(china_shp)
aet_rasters <- rast(aet_nc_path)

# ⭐ 重投影 shapefile 到 AET 栅格的 CRS
china_boundary_proj <- project(china_boundary, crs(aet_rasters))

# 初始化保存栅格
aet_monthly_list <- list()

for (month in 1:12) {
  aet_month <- aet_rasters[[month]]
  
  # 裁剪并掩膜（使用重投影后的矢量）
  aet_clipped <- crop(aet_month, china_boundary_proj) |> mask(china_boundary_proj)
  
  # 保存文件
  output_month_path <- file.path(output_dir, paste0("AET_2018_", month, ".tif"))
  writeRaster(aet_clipped, output_month_path, overwrite = TRUE)
  
  aet_monthly_list[[month]] <- aet_clipped
}

# 计算年均值
aet_2018_mean <- mean(rast(aet_monthly_list))

# 保存年均值
output_mean_path <- file.path(output_dir, "AET_2018_annual_mean.tif")
writeRaster(aet_2018_mean, output_mean_path, overwrite = TRUE)

cat("✔️ 2018年 AET 月值与年均值提取完成！\n")
