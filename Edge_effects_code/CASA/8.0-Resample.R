library(terra)

terraOptions(progress = 1, memfrac = 0.9)

# 设置中国 Albers 投影（单位：米）
china_albers <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 设置近似的目标分辨率（经纬度 ~500m）
target_res_deg <- 0.0045

# 定义输入输出路径对（列表形式）
regions <- list(
  NO = list(
    input = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NO/NO.tif",
    output = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NO/NO_500m.tif"
  ),
  EA = list(
    input = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/EA/EA.tif",
    output = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/EA/EA_500m.tif"
  ),
  SO = list(
    input = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SO/SO.tif",
    output = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SO/SO_500m.tif"
  ),
  SW = list(
    input = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW.tif",
    output = "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW_500m.tif"
  )
)

# 批量处理函数
for (region in names(regions)) {
  input_path <- regions[[region]]$input
  output_path <- regions[[region]]$output
  
  cat("🔄 正在处理区域：", region, "\n")
  
  # 读取 NDVI 原始 10m 栅格
  ndvi_raw <- rast(input_path)
  
  # 投影到 Albers（单位为米）
  ndvi_albers <- project(ndvi_raw, china_albers)
  
  # 聚合为 500m
  ndvi_500m_albers <- aggregate(ndvi_albers, fact = 50, fun = mean, na.rm = TRUE)
  
  # 回投影到 WGS84（设置目标分辨率）
  ndvi_500m_wgs84 <- project(ndvi_500m_albers, "EPSG:4326", res = target_res_deg)
  
  # 保存输出
  writeRaster(ndvi_500m_wgs84, output_path, overwrite = TRUE)
  
  cat("✅ 成功输出：", output_path, "\n\n")
  
  # 清理内存
  rm(ndvi_raw, ndvi_albers, ndvi_500m_albers, ndvi_500m_wgs84)
  gc()
}

# ------------------------------------------------------------------------------


library(terra)

##==== 设置参考 NDVI 栅格 ====##
ref_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NW/NW_500m.tif"
refRaster <- rast(ref_path)
crs_target <- crs(refRaster)  # 用作统一 CRS

##==== 重采样 OptTEM ====##
opt_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/OptTEM.tif"
OptTEM <- rast(opt_path)

OptTEM_resampled <- resample(OptTEM, refRaster, method = "near")
crs(OptTEM_resampled) <- crs_target
writeRaster(
  OptTEM_resampled,
  "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/OptTEM_resampled_500m.tif",
  overwrite = TRUE
)
rm(OptTEM, OptTEM_resampled); gc()
print("✅ OptTEM 重采样并保存完成。")

##==== 重采样 SRAD ====##
srad_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRAD_2023_annual_mean.tif"
SRAD <- rast(srad_path)

SRAD_resampled <- resample(SRAD, refRaster, method = "cubic")
crs(SRAD_resampled) <- crs_target
writeRaster(
  SRAD_resampled,
  "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRAD_2023_resampled_500m.tif",
  overwrite = TRUE
)
rm(SRAD, SRAD_resampled); gc()
print("✅ SRAD 重采样并保存完成。")

##==== 重采样 SRMax ====##
srmax_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRMax.tif"
SRMax <- rast(srmax_path)

SRMax_resampled <- resample(SRMax, refRaster, method = "near")
crs(SRMax_resampled) <- crs_target
writeRaster(
  SRMax_resampled,
  "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRMax_resampled_500m.tif",
  overwrite = TRUE
)
rm(SRMax, SRMax_resampled); gc()
print("✅ SRMax 重采样并保存完成。")
