# ------------------------------------------------------------------------------
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
ndvi_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NE/NE.tif"
refRaster <- rast(ndvi_path)
crs_target <- crs(refRaster)  # 统一参考 CRS

##==== 年份 & 输出路径 ====##
year <- 2023
out_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/WaterStress_10m"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

##==== 读取原始栅格 ====##
AET <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/AET_", year, "_annual_mean.tif"))
PET <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/PET_", year, "_annual_mean.tif"))
TEM_K <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/TEM_", year, "_annual_mean.tif"))  # 用 TEM_K 保留原始开尔文

##==== 重采样并对齐到 NDVI 分辨率、范围和 CRS ====##
AET <- resample(AET, refRaster, method = "cubic")
PET <- resample(PET, refRaster, method = "cubic")
TEM_K <- resample(TEM_K, refRaster, method = "cubic")  # 还是开尔文单位

# 强制统一 CRS（防止警告）
crs(AET) <- crs_target
crs(PET) <- crs_target
crs(TEM_K) <- crs_target

##==== 输出 TEM（开尔文）重采样结果 ====##
tem_out_path <- file.path(out_dir, paste0("TEM_", year, "_resampled_10m.tif"))
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
out_path <- file.path(out_dir, paste0("WStress_", year, "_annual_10m.tif"))
writeRaster(WStress, out_path, overwrite = TRUE)

print("✅ Water Stress calculation complete.")

# ------------------------------------------------------------------------------
library(terra)

##==== 设置 terra 临时目录 ====##
temp_dir <- "D:/temp_terra"
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
terraOptions(tempdir = temp_dir)

cat("✅ terra 临时目录设置为：", terraOptions()$tempdir, "\n")

##==== 设置输出目录并创建（如果不存在）====##
out_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_10m"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
cat("✅ 输出目录已创建或已存在：", out_dir, "\n")

##==== 设置参考 NDVI 栅格 ====##
ref_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/SW/SW.tif"
refRaster <- rast(ref_path)
crs_target <- crs(refRaster)

##==== 重采样 TEM ====##
tem_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_500m/TEM_2023_resampled_500m.tif"
TEM <- rast(tem_path)
TEM_resampled <- resample(TEM, refRaster, method = "near")
crs(TEM_resampled) <- crs_target
writeRaster(
  TEM_resampled,
  file.path(out_dir, "TEM_2023_resampled_10m.tif"),
  overwrite = TRUE
)
rm(TEM, TEM_resampled); gc()
print("✅ TEM 重采样并保存完成。")