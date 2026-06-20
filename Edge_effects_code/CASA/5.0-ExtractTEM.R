# 2m平均气温（TEM） 的 每年逐月数据和年均值
rm(list = ls())
gc(reset = TRUE)
library(terra)

# 路径设置
tem_nc_path <- "D:/Forest_Fragmentation/NPP/CASA_China/TEM/Raw/TEM_2018_2023.nc"
china_shp <- "D:/Forest_Fragmentation/NPP/CASA_China/shengjie/shengjie.shp"
output_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/TEM"

# 读取数据
china_boundary <- vect(china_shp)
tem_rasters <- rast(tem_nc_path)  # 假设波段是时间顺序排列（月）

# 重投影 shapefile 到 TEM 栅格的 CRS
china_boundary_proj <- project(china_boundary, crs(tem_rasters))

# 时间范围和年份设置
start_year <- 2018
end_year <- 2023
years <- seq(start_year, end_year)
months <- 1:12

# 确认总波段数，理论应该是 (end_year - start_year + 1)*12
total_bands <- nlyr(tem_rasters)
expected_bands <- length(years) * length(months)
if(total_bands != expected_bands){
  stop(paste0("波段数不符合预期，波段数: ", total_bands, "，预期: ", expected_bands))
}

# 循环每年每月提取、裁剪、掩膜和保存
for(y in years){
  monthly_rasters <- list()
  
  for(m in months){
    # 计算波段索引（1基）
    band_index <- (y - start_year) * 12 + m
    
    tem_month <- tem_rasters[[band_index]]
    
    # 裁剪掩膜
    tem_clipped <- crop(tem_month, china_boundary_proj) |> mask(china_boundary_proj)
    
    # 保存单月tif
    output_month_path <- file.path(output_dir, paste0("TEM_", y, "_", sprintf("%02d", m), ".tif"))
    writeRaster(tem_clipped, output_month_path, overwrite = TRUE)
    
    monthly_rasters[[m]] <- tem_clipped
  }
  
  # 计算年均温
  tem_yearly_mean <- mean(rast(monthly_rasters))
  
  # 保存年均温tif
  output_year_path <- file.path(output_dir, paste0("TEM_", y, "_annual_mean.tif"))
  writeRaster(tem_yearly_mean, output_year_path, overwrite = TRUE)
  
  cat(paste0("✔️ 完成 ", y, " 年 TEM 月均温与年均温提取\n"))
}

cat("全部完成！\n")
