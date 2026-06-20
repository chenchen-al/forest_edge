rm(list = rm())
gc(reset = TRUE)
library(terra)

##==== Function ====##
CalWaterStress <- function(X){
  TEM <- X[1]
  AET <- X[2]
  PET <- X[3]
  WStressBe <- X[4]
  if(any(is.na(c(TEM,AET,PET,WStressBe)))){
    Y <- NaN
    return(Y)
  }
  if(TEM <= 0){
    Y <- WStressBe
    return(Y)
  }
  if(PET == 0){
    Y <- 1
  }else{
    Y <- 0.5 + 0.5*AET/PET
  }
  if(Y > 1){
    Y <- 1
  }
  return(Y)
}
##==== Function ====##

refRaster <- rast(nrows = 2160,ncols = 4320,nlyrs = 1,xmin = -180,xmax = 180,ymin = -90,ymax = 90,vals = NaN,crs = crs('EPSG:4326'))
for(i in seq(1981,2022)){
  for(j in seq(1,12)){
    if((i == 1981) & (j == 1)){
      AETRaster <- rast(paste0('../NPP/AET/AET_',as.character(i),'_',as.character(j),'.tif'))
      AETRaster <- resample(AETRaster,refRaster,method = 'cubic')
      AETRaster[AETRaster < 0] <- 0
      PETRaster <- rast(paste0('../NPP/PET/PET_',as.character(i),'_',as.character(j),'.tif'))
      PETRaster <- resample(PETRaster,refRaster,method = 'cubic')
      PETRaster[PETRaster < 0] <- 0
      WStressRaster <- refRaster
      WStressRaster[PETRaster == 0] <- 1
      WStressRaster[PETRaster != 0] <- 0.5 + 0.5*(AETRaster[PETRaster != 0]/PETRaster[PETRaster != 0])
      WStressRaster[WStressRaster > 1] <- 1
      outputFileName <- paste0('../NPP/WaterStress/WStress_',as.character(i),'_',as.character(j),'.tif')
      writeRaster(WStressRaster,outputFileName,overwrite = TRUE)
    }else if(j == 1){
      TEMRaster <- rast(paste0('../NPP/TEM/TEM_',as.character(i),'_',as.character(j),'.tif'))
      TEMRaster <- TEMRaster - 273.15
      TEMRaster <- resample(TEMRaster,refRaster,method = 'cubic')
      AETRaster <- rast(paste0('../NPP/AET/AET_',as.character(i),'_',as.character(j),'.tif'))
      AETRaster <- resample(AETRaster,refRaster,method = 'cubic')
      AETRaster[AETRaster < 0] <- 0
      PETRaster <- rast(paste0('../NPP/PET/PET_',as.character(i),'_',as.character(j),'.tif'))
      PETRaster <- resample(PETRaster,refRaster,method = 'cubic')
      PETRaster[PETRaster < 0] <- 0
      WStressRasterBe <- rast(paste0('../NPP/WaterStress/WStress_',as.character(i-1),'_12.tif'))
      rasters <- c(TEMRaster,AETRaster,PETRaster,WStressRasterBe)
      WStressRaster <- app(rasters,fun = CalWaterStress,cores = 24)
      outputFileName <- paste0('../NPP/WaterStress/WStress_',as.character(i),'_',as.character(j),'.tif')
      writeRaster(WStressRaster,outputFileName,overwrite = TRUE)
    }else{
      TEMRaster <- rast(paste0('../NPP/TEM/TEM_',as.character(i),'_',as.character(j),'.tif'))
      TEMRaster <- TEMRaster - 273.15
      TEMRaster <- resample(TEMRaster,refRaster,method = 'cubic')
      AETRaster <- rast(paste0('../NPP/AET/AET_',as.character(i),'_',as.character(j),'.tif'))
      AETRaster <- resample(AETRaster,refRaster,method = 'cubic')
      AETRaster[AETRaster < 0] <- 0
      PETRaster <- rast(paste0('../NPP/PET/PET_',as.character(i),'_',as.character(j),'.tif'))
      PETRaster <- resample(PETRaster,refRaster,method = 'cubic')
      PETRaster[PETRaster < 0] <- 0
      WStressRasterBe <- rast(paste0('../NPP/WaterStress/WStress_',as.character(i),'_',as.character(j-1),'.tif'))
      rasters <- c(TEMRaster,AETRaster,PETRaster,WStressRasterBe)
      WStressRaster <- app(rasters,fun = CalWaterStress,cores = 24)
      outputFileName <- paste0('../NPP/WaterStress/WStress_',as.character(i),'_',as.character(j),'.tif')
      writeRaster(WStressRaster,outputFileName,overwrite = TRUE)
    }
  }
}

# ------------------------------------------------------------------------------版本1（500m是这样算的。10m消耗量太大）
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

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------版本二，10m先得到了TEM，但是内存消耗过大，特别慢
# 清理环境
rm(list = ls())
gc(reset = TRUE)
library(terra)

# 先创建目录，再设置 terra 临时目录
dir.create("D:/temp_terra", showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir = "D:/temp_terra")
terraOptions()$tempdir


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

##==== 直接读取已重采样的 TEM 栅格（开尔文） ====##
TEM_K <- rast(file.path(out_dir, paste0("TEM_", year, "_resampled_10m.tif")))

##==== 读取并重采样 AET 和 PET（与 refRaster 对齐）====##
AET <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/AET_", year, "_annual_mean.tif"))
PET <- rast(paste0("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/PET_", year, "_annual_mean.tif"))

AET <- resample(AET, refRaster, method = "cubic")
PET <- resample(PET, refRaster, method = "cubic")

crs(AET) <- crs_target
crs(PET) <- crs_target
crs(TEM_K) <- crs_target  # 这行只是保险


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



# ------------------------------------------------------------------------------版本3，基于500m结果，插值，因为，气候数据是9km，并不会造成太多变化
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
tem_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NE/WaterStress_500m/TEM_2023_resampled_500m.tif"
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

##==== 重采样 Water Stress ====##
ws_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/SW/WaterStress_500m/WStress_2023_annual_500m.tif"
WStress <- rast(ws_path)
WStress_resampled <- resample(WStress, refRaster, method = "near")
crs(WStress_resampled) <- crs_target
writeRaster(
  WStress_resampled,
  file.path(out_dir, "WStress_2023_annual_10m.tif"),
  overwrite = TRUE
)
rm(WStress, WStress_resampled); gc()
print("✅ WStress 重采样并保存完成。")

# ------------------------------------------------------------------------------版本4，基于500m结果，插值，swTEM
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