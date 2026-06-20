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





# ------------------------------------------------------------------------------验证_冯
library(terra)

# 设置路径
folder_path <- "D:/Forest_Fragmentation/NPP/CASAPKU/NPP"

# 自动匹配 NPP_2022_1 到 NPP_2022_12
npp_files <- list.files(folder_path, pattern = "^NPP_2022_(1[0-2]|[1-9])\\.tif$", full.names = TRUE)

# 读取并堆叠所有月度 NPP
npp_stack <- rast(npp_files)

# 计算逐像元年平均（单位仍为 gC/m²）
npp_annual_mean <- mean(npp_stack, na.rm = TRUE)

# 输出路径（可根据需要修改）
output_path <- file.path(folder_path, "NPP_2022_annual_mean.tif")
writeRaster(npp_annual_mean, output_path, overwrite = TRUE)

# 打印确认
cat("✅ 年平均 NPP 栅格已保存到：", output_path, "\n")

# ================================================================
library(terra)

# 设置路径
shp_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/shp"
input_raster <- "D:/Forest_Fragmentation/NPP/CASAPKU/NPP/NPP_2022_annual_mean.tif"
output_base <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part"

# 六大地理分区
regions <- c("NW", "NO", "NE", "EA", "SO", "SW")

# 加载栅格
r <- rast(input_raster)

# 循环每个区域
for (region in regions) {
  cat("\n📍 正在处理区域：", region, "\n")
  
  # 加载对应 shapefile
  shp_path <- file.path(shp_dir, paste0(region, ".shp"))
  region_shp <- vect(shp_path)
  
  # 匹配 CRS
  region_shp_match <- project(region_shp, crs(r))
  
  # 裁剪和掩膜
  r_crop <- crop(r, region_shp_match)
  r_mask <- mask(r_crop, region_shp_match)
  
  # 重投影为 EPSG:4326（WGS84）
  r_out <- project(r_mask, "EPSG:4326", method = "bilinear")
  
  # 输出路径
  out_dir <- file.path(output_base, region)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_file <- file.path(out_dir, basename(input_raster))
  writeRaster(r_out, out_file, overwrite = TRUE)
  
  cat("✅ 已保存至：", out_file, "\n")
}


# ==============================================================================
# 获取当前 R 使用的临时目录路径
r_tempdir <- tempdir()
# 打印结果
cat("📂 当前 R 临时目录：", r_tempdir, "\n")



# ==============================================================================
library(terra)

# 读取9km NPP
src <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_2022_annual_mean.tif")

# 读取目标分辨率（500m）栅格作为参考
target <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_500m.tif")

# 重采样：从9km → 500m，用 bilinear 插值
resampled <- resample(src, target, method = "bilinear")

# 保存结果
writeRaster(resampled,
            "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_2022_resampled_to_500m.tif",
            overwrite = TRUE)

print("✅ 9km NPP 已成功重采样到 500m 分辨率。")


# ==============================================================================
library(terra)

# 读取两幅图像
r_npp_9km <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_2022_resampled_to_500m.tif")
r_npp_500m <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_500m.tif")

# 保证两者坐标系一致（你说已处理，这里只是保险）
crs(r_npp_9km) <- crs(r_npp_500m)

# 重采样到相同分辨率（如果还没做）
r_npp_9km_resampled <- resample(r_npp_9km, r_npp_500m, method = "bilinear")

# 裁剪到目标区域
r_npp_9km_cropped <- crop(r_npp_9km_resampled, r_npp_500m)

# 掩膜掉非重叠区域（即只有两图都存在值的地方保留）
r_npp_9km_clipped <- mask(r_npp_9km_cropped, r_npp_500m)

# 保存裁剪后的栅格
writeRaster(r_npp_9km_clipped,
            "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/NPP_2022_clipped_to_500m.tif",
            overwrite = TRUE)

print("✅ 已裁剪 NPP_2022 到与 NPP_500m 完全重合区域。")

