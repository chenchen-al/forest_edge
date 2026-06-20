rm(list = ls())
gc(reset = TRUE)
library(terra)

for(i in seq(1981,2018)){
  PETRasters <- rast(paste0('../NPP/PET/Raw/TerraClimate_pet_',as.character(i),'.nc'))
  for(j in seq(1,12)){
    PETRaster <- PETRasters[[j]]
    outputFileName <- paste0('../NPP/PET/PET_',as.character(i),'_',as.character(j),'.tif')
    writeRaster(PETRaster,outputFileName,overwrite = TRUE)
  }
  print(i)
}


# ------------------------------------------------------------------------------
# 潜在蒸散发（PET） 的 每年逐月数据和年均值
# 清理环境
rm(list = ls())
gc(reset = TRUE)
library(terra)

# 路径设置
pet_nc_path <- "D:/Forest_Fragmentation/NPP/CASA_China/PET/Raw/TerraClimate_pet_2018.nc"
china_shp <- "D:/Forest_Fragmentation/NPP/CASA_China/shengjie/shengjie.shp"
output_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/PET"

# 读取数据
china_boundary <- vect(china_shp)
pet_rasters <- rast(pet_nc_path)

# ⭐ 重投影 shapefile 到 PET 栅格的 CRS
china_boundary_proj <- project(china_boundary, crs(pet_rasters))

# 初始化保存栅格
pet_monthly_list <- list()

for (month in 1:12) {
  pet_month <- pet_rasters[[month]]
  
  # 裁剪并掩膜（使用重投影后的矢量）
  pet_clipped <- crop(pet_month, china_boundary_proj) |> mask(china_boundary_proj)
  
  # 保存文件
  output_month_path <- file.path(output_dir, paste0("PET_2018_", month, ".tif"))
  writeRaster(pet_clipped, output_month_path, overwrite = TRUE)
  
  pet_monthly_list[[month]] <- pet_clipped
}

# 计算年均值
pet_2018_mean <- mean(rast(pet_monthly_list))

# 保存年均值
output_mean_path <- file.path(output_dir, "PET_2018_annual_mean.tif")
writeRaster(pet_2018_mean, output_mean_path, overwrite = TRUE)

cat("✔️ 2018年 PET 月值与年均值提取完成！\n")
