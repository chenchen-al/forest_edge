library(terra)
##==== 设置 terra 临时目录 ====##
temp_dir <- "D:/temp_terra"
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
terraOptions(tempdir = temp_dir)

##==== 验证设置是否成功 ====##
cat("✅ terra 临时目录设置为：", terraOptions()$tempdir, "\n")

##==== 设置参考 NDVI 栅格 ====##
ref_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW.tif"
refRaster <- rast(ref_path)
crs_target <- crs(refRaster)  # 用作统一 CRS

##==== 重采样 OptTEM ====##
opt_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/OptTEM_2023_resampled_500m.tif"
OptTEM <- rast(opt_path)

OptTEM_resampled <- resample(OptTEM, refRaster, method = "near")
crs(OptTEM_resampled) <- crs_target
writeRaster(
  OptTEM_resampled,
  "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/OptTEM_2023_resampled_10m.tif",
  overwrite = TRUE
)
rm(OptTEM, OptTEM_resampled); gc()
print("✅ OptTEM 重采样并保存完成。")

##==== 重采样 SRAD ====##
srad_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRAD_2023_resampled_500m.tif"
SRAD <- rast(srad_path)

SRAD_resampled <- resample(SRAD, refRaster, method = "cubic")
crs(SRAD_resampled) <- crs_target
writeRaster(
  SRAD_resampled,
  "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRAD_2023_resampled_10m.tif",
  overwrite = TRUE
)
rm(SRAD, SRAD_resampled); gc()
print("✅ SRAD 重采样并保存完成。")

##==== 重采样 SRMax ====##
srmax_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRMax_resampled_500m.tif"
SRMax <- rast(srmax_path)

SRMax_resampled <- resample(SRMax, refRaster, method = "near")
crs(SRMax_resampled) <- crs_target
writeRaster(
  SRMax_resampled,
  "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRMax_resampled_10m.tif",
  overwrite = TRUE
)
rm(SRMax, SRMax_resampled); gc()
print("✅ SRMax 重采样并保存完成。")




# -------------------------------------------------------------------------------
# 清理环境
rm(list = ls())
gc(reset = TRUE)
library(terra)
##==== 设置 terra 临时目录 ====##
temp_dir <- "D:/temp_terra"
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
terraOptions(tempdir = temp_dir)

##==== 验证设置是否成功 ====##
cat("✅ terra 临时目录设置为：", terraOptions()$tempdir, "\n")

##==== 定义 Water Stress 计算函数 ====##
CalWaterStress <- function(X) {
  TEM <- X[1]
  AET <- X[2]
  PET <- X[3]
  WStressBe <- X[4]
  
  if (any(is.na(c(TEM, AET, PET, WStressBe)))) {
    return(NaN)
  }
  if (TEM <= 0) {
    return(WStressBe)
  }
  if (PET == 0) {
    return(1)
  } else {
    Y <- 0.5 + 0.5 * AET / PET
    return(ifelse(Y > 1, 1, Y))
  }
}

##==== 设置参考 NDVI 栅格 ====##
ndvi_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW_500m.tif"
refRaster <- rast(ndvi_path)
crs_target <- crs(refRaster)  # 统一参考 CRS

##==== 年份 & 输出路径 ====##
year <- 2023
out_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_500m"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

##==== 读取原始栅格 ====##
AET <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/AET_", year, "_annual_mean.tif"))
PET <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/PET_", year, "_annual_mean.tif"))
TEM_K <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/TEM_", year, "_annual_mean.tif"))  # 用 TEM_K 保留原始开尔文

##==== 重采样并对齐到 NDVI 分辨率、范围和 CRS ====##
AET <- resample(AET, refRaster, method = "cubic")
PET <- resample(PET, refRaster, method = "cubic")
TEM_K <- resample(TEM_K, refRaster, method = "cubic")  # 还是开尔文单位

# 强制统一 CRS（防止警告）
crs(AET) <- crs_target
crs(PET) <- crs_target
crs(TEM_K) <- crs_target

##==== 输出 TEM（开尔文）重采样结果 ====##
tem_out_path <- file.path(out_dir, paste0("TEM_", year, "_resampled_500m.tif"))
writeRaster(TEM_K, tem_out_path, overwrite = TRUE)

##==== 修正负值 ====##
AET[AET < 0] <- 0
PET[PET < 0] <- 0

##==== 初始化上一年水分胁迫栅格 ====##
WStressBe <- refRaster
values(WStressBe) <- 1
crs(WStressBe) <- crs_target

##==== 临时转为摄氏度用于计算 ====##
TEM_C <- TEM_K - 273.15

##==== 应用函数计算 Water Stress ====##
rasters <- c(TEM_C, AET, PET, WStressBe)
WStress <- app(rasters, fun = CalWaterStress, cores = 12)

##==== 输出结果 ====##
out_path <- file.path(out_dir, paste0("WStress_", year, "_annual_500m.tif"))
writeRaster(WStress, out_path, overwrite = TRUE)

print("✅ Water Stress calculation complete.")



# -------------------------------------------------------------------------------NPP500m
library(terra)
##==== 设置 terra 临时目录 ====##
temp_dir <- "D:/temp_terra"
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
terraOptions(tempdir = temp_dir)

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

# 加载 6 个输入栅格
NDVI     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NO/NO_500m.tif")
TEM      <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/WaterStress_500m/TEM_2023_resampled_500m.tif")
OptTEM   <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/OptTEM_2023_resampled_500m.tif")
SRAD     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/SRAD_2023_resampled_500m.tif")
SRMax    <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/SRMax_resampled_500m.tif")
WStress  <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/WaterStress_500m/WStress_2023_annual_500m.tif")

# 合并为一个栅格集合
stacked <- c(NDVI, TEM, OptTEM, SRAD, SRMax, WStress)

# 计算 NPP
NPPRaster <- app(stacked, fun = CalCASANPP, cores = 12)

# 输出到目标路径
writeRaster(
  NPPRaster,
  filename = "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/NPP_500m_new.tif",
  overwrite = TRUE
)

# 打印统计信息
print(global(NPPRaster, fun = c("min", "max", "mean"), na.rm = TRUE))

# ✅ 最后一行：成功提示
print("NPP计算完成并成功保存！")

# ------------------------------------------------------------------------------算总C/500m
library(terra)

# 读取 NPP 栅格（单位：gC/m²）
npp_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NO/NPP_500m_new.tif"
npp <- rast(npp_path)

# 计算每个像元的真实面积（单位：m²）
area_raster <- cellSize(npp, unit = "m")

# 逐像元计算碳总量（gC）= NPP × 面积
total_carbon <- npp * area_raster

# 汇总所有像元碳量（gC），再转为 TgC（1e12 gC）
total_gC <- global(total_carbon, "sum", na.rm = TRUE)[1, 1]
total_TgC <- total_gC*12 / 1e12

# 输出结果
cat("NO区域2023年总NPP", round(total_TgC, 3), "TgC\n")


# ------------------------------------------------------------------------------计算NPP/10m
# 清理环境
rm(list = ls())
gc(reset = TRUE)
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

# 加载 6 个输入栅格
NDVI     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW.tif")
TEM      <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_10m/TEM_2023_resampled_10m.tif")
OptTEM   <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/OptTEM_2023_resampled_10m.tif")
SRAD     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRAD_2023_resampled_10m.tif")
SRMax    <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/SRMax_resampled_10m.tif")
WStress  <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_10m/WStress_2023_annual_10m.tif")

# 合并为一个栅格集合
stacked <- c(NDVI, TEM, OptTEM, SRAD, SRMax, WStress)

# 计算 NPP
NPPRaster <- app(stacked, fun = CalCASANPP, cores = 12)

# 输出到目标路径
writeRaster(
  NPPRaster,
  filename = "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/NPP_10m_SW.tif",
  overwrite = TRUE
)

# 打印统计信息
print(global(NPPRaster, fun = c("min", "max", "mean"), na.rm = TRUE))

# ✅ 最后一行：成功提示
print("NPP计算完成并成功保存！")

# ------------------------------------------------------------------------------算总C/10m
library(terra)

# 读取 NPP 栅格（单位：gC/m²）
npp_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/NPP_10m_SW.tif"
npp <- rast(npp_path)

# 计算每个像元的真实面积（单位：m²）
area_raster <- cellSize(npp, unit = "m")

# 逐像元计算碳总量（gC）= NPP × 面积
total_carbon <- npp * area_raster

# 汇总所有像元碳量（gC），再转为 TgC（1e12 gC）
total_gC <- global(total_carbon, "sum", na.rm = TRUE)[1, 1]
total_TgC <- total_gC*12 / 1e12

# 输出结果
cat("SW区域2023年总NPP", round(total_TgC, 3), "TgC\n")


