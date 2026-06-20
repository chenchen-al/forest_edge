rm(list = ls())
gc(reset = TRUE)
library(terra)

for(i in seq(1982,2018)){
  SRADRasters <- rast(paste0('../NPP/SRAD/Raw/TerraClimate_srad_',as.character(i),'.nc'))
  for(j in seq(1,12)){
    SRADRaster <- SRADRasters[[j]]
    outputFileName <- paste0('../NPP/SRAD/SRAD_',as.character(i),'_',as.character(j),'.tif')
    writeRaster(SRADRaster,outputFileName,overwrite = TRUE)
  }
  print(i)
}

# ------------------------------------------------------------------------------
# 太阳辐射（SRAD） 的 每年逐月数据和年均值
# 清理环境
rm(list = ls())
gc(reset = TRUE)
library(terra)

# 路径设置
srad_nc_path <- "D:/Forest_Fragmentation/NPP/CASA_China/SRAD/Raw/TerraClimate_srad_2018.nc"
china_shp <- "D:/Forest_Fragmentation/NPP/CASA_China/shengjie/shengjie.shp"
output_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/SRAD"

# 读取数据
china_boundary <- vect(china_shp)
srad_rasters <- rast(srad_nc_path)

# ⭐ 重投影 shapefile 到 SRAD 栅格的 CRS
china_boundary_proj <- project(china_boundary, crs(srad_rasters))

# 初始化保存栅格
srad_monthly_list <- list()

for (month in 1:12) {
  srad_month <- srad_rasters[[month]]
  
  # 裁剪并掩膜（使用重投影后的矢量）
  srad_clipped <- crop(srad_month, china_boundary_proj) |> mask(china_boundary_proj)
  
  # 保存文件
  output_month_path <- file.path(output_dir, paste0("SRAD_2018_", month, ".tif"))
  writeRaster(srad_clipped, output_month_path, overwrite = TRUE)
  
  srad_monthly_list[[month]] <- srad_clipped
}

# 计算年均值
srad_2018_mean <- mean(rast(srad_monthly_list))

# 保存年均值
output_mean_path <- file.path(output_dir, "SRAD_2018_annual_mean.tif")
writeRaster(srad_2018_mean, output_mean_path, overwrite = TRUE)

cat("✔️ 2018年 SRAD 月值与年均值提取完成！\n")
