# ------------------------------------------------------------------------------计算NPP
library(terra)
##==== 设置 terra 临时目录 ====##
temp_dir <- "D:/temp_terra"
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
terraOptions(tempdir = temp_dir)

##==== 验证设置是否成功 ====##
cat("✅ terra 临时目录设置为：", terraOptions()$tempdir, "\n")

# 定义 CASA 计算函数
CalCASANPP <- function(X){
  if(any(is.na(X))){
    return(NaN)
  }
  NDVI <- X[1]
  TEM <- X[2]
  OptTEM <- X[3]
  SRAD <- X[4]
  SRMax <- X[5]
  WStress <- X[6]
  
  # 计算 SR 和 FPAR
  SR <- (1 + NDVI)/(1 - NDVI)
  FPAR <- (SR - 1.08)/(SRMax - 1.08)
  FPAR <- min(FPAR, 0.95)
  
  # 年均 SRAD → MJ/m²/month
  SRAD <- SRAD * 3600 * 24 * 365 / 1e6
  SRAD <- SRAD / 12
  APAR <- SRAD * FPAR * 0.5
  
  # 温度胁迫
  TStress1 <- 0.8 + 0.02 * OptTEM - 0.0005 * OptTEM^2
  if (TEM < -10 || TStress1 < 0) TStress1 <- 0
  
  coeff1 <- 1 + exp(0.2 * (OptTEM - 10 - TEM))
  coeff2 <- 1 + exp(0.3 * (TEM - 10 - OptTEM))
  TStress2 <- 1.1814 / coeff1 / coeff2
  
  OptCoeff1 <- 1 + exp(-2)
  OptCoeff2 <- 1 + exp(-3)
  OptTStress2 <- 1.1814 / OptCoeff1 / OptCoeff2
  if (TEM > OptTEM + 10 || TEM < OptTEM - 13) {
    TStress2 <- OptTStress2 / 2
  }
  if (TStress2 < 0) TStress2 <- 0
  
  # 计算 NPP
  NPP <- APAR * TStress1 * TStress2 * WStress * 0.405
  if (NPP < 0) NPP <- 0
  
  return(NPP)
}

# ----------------- 1. NE -----------------
cat("\n🚀 正在处理 NE 区域...\n")
NDVI     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NE/NE.tif")
TEM      <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/WaterStress_10m/TEM_2023_resampled_10m.tif")
OptTEM   <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/OptTEM_2023_resampled_10m.tif")
SRAD     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/SRAD_2023_resampled_10m.tif")
SRMax    <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/SRMax_resampled_10m.tif")
WStress  <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/WaterStress_10m/WStress_2023_annual_10m.tif")

# 合并为一个栅格集合
stacked <- c(NDVI, TEM, OptTEM, SRAD, SRMax, WStress)

# 计算 NPP
NPPRaster <- app(stacked, fun = CalCASANPP, cores = 12)

# 输出到目标路径
writeRaster(
  NPPRaster,
  filename = "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_10m.tif",
  overwrite = TRUE
)

# 打印统计信息
print(global(NPPRaster, fun = c("min", "max", "mean"), na.rm = TRUE))
cat("✅ NE 区域 NPP 计算完成！\n")

# ----------------- 2. SO -----------------
cat("\n🚀 正在处理 SO 区域...\n")
NDVI <- rast("D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SO/SO.tif")
TEM <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SO/WaterStress_10m/TEM_2023_resampled_10m.tif")
OptTEM <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SO/OptTEM_2023_resampled_10m.tif")
SRAD <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SO/SRAD_2023_resampled_10m.tif")
SRMax <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SO/SRMax_resampled_10m.tif")
WStress <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SO/WaterStress_10m/WStress_2023_annual_10m.tif")

stacked <- c(NDVI, TEM, OptTEM, SRAD, SRMax, WStress)
NPPRaster <- app(stacked, fun = CalCASANPP, cores = 12)

writeRaster(NPPRaster,
            filename = "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SO/NPP_10m.tif",
            overwrite = TRUE)
print(global(NPPRaster, fun = c("min", "max", "mean"), na.rm = TRUE))
cat("✅ SO 区域 NPP 计算完成！\n")

# ----------------- 3. SW -----------------
cat("\n🚀 正在处理 SW 区域...\n")
NDVI <- rast("D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW.tif")
TEM <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_10m/TEM_2023_resampled_10m.tif")
OptTEM <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/OptTEM_2023_resampled_10m.tif")
SRAD <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRAD_2023_resampled_10m.tif")
SRMax <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRMax_resampled_10m.tif")
WStress <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_10m/WStress_2023_annual_10m.tif")

stacked <- c(NDVI, TEM, OptTEM, SRAD, SRMax, WStress)
NPPRaster <- app(stacked, fun = CalCASANPP, cores = 12)

writeRaster(NPPRaster,
            filename = "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/NPP_10m.tif",
            overwrite = TRUE)
print(global(NPPRaster, fun = c("min", "max", "mean"), na.rm = TRUE))
cat("✅ SW 区域 NPP 计算完成！\n")

# ==== 全部完成 ====
cat("\n🎉 所有区域 NPP 计算已完成并保存！\n")