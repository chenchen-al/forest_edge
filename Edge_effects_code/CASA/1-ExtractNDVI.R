rm(list = ls())
gc(reset = TRUE)
library(terra)

CalNDVI <- function(X){
  if(any(is.na(X))){
    Y <- NaN
    return(Y)
  }
  Y <- max(X)/1000
  if(Y > 0.99){
    Y <- NaN
  }
  return(Y)
}

years <- seq(1982,2022)
months <- seq(1,12)
refRaster <- rast(nrows = 2160,ncols = 4320,xmin = -180,xmax = 180,ymin = -90,ymax = 90,crs = 'EPSG:4326')
for(i in seq(1,length(years))){
  for(j in seq(1,length(months))){
    tempYear <- years[i]
    tempMonth <- months[j]
    if(tempMonth < 10){
      fileName1 <- paste0('../NPP/NDVI/Raw/PKU_GIMMS_NDVI_V1.2_',as.character(tempYear),'0',as.character(tempMonth),'01.tif')
      fileName2 <- paste0('../NPP/NDVI/Raw/PKU_GIMMS_NDVI_V1.2_',as.character(tempYear),'0',as.character(tempMonth),'02.tif')
    }else{
      fileName1 <- paste0('../NPP/NDVI/Raw/PKU_GIMMS_NDVI_V1.2_',as.character(tempYear),as.character(tempMonth),'01.tif')
      fileName2 <- paste0('../NPP/NDVI/Raw/PKU_GIMMS_NDVI_V1.2_',as.character(tempYear),as.character(tempMonth),'02.tif')
    }
    tempRaser1 <- rast(fileName1)
    tempRaser1 <- tempRaser1[[1]]
    tempRaser2 <- rast(fileName2)
    tempRaser2 <- tempRaser2[[1]]
    rasters <- c(tempRaser1,tempRaser2)
    NDVIRaster <- app(rasters,fun = CalNDVI,cores = 24)
    outputFileName <- paste0('../NPP/NDVI/NDVI_',as.character(tempYear),'_',as.character(tempMonth),'.tif')
    writeRaster(NDVIRaster,outputFileName,overwrite = TRUE)
  }
  print(tempYear)
}
# ------------------------------------------------------------------------------
# 图像拼接
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


# 设置文件夹路径
folder_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/NPP_10m"

# 获取所有 .tif 文件的完整路径
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# 加载所有栅格图像
rasters <- lapply(tif_files, rast)

# 拼接图像（自动拼接相邻栅格）
mosaic_raster <- do.call(mosaic, rasters)

# 输出路径，命名为 .tif
output_path <- file.path(folder_path, "China_NPP_10m.tif")
writeRaster(mosaic_raster, output_path, overwrite = TRUE)

cat("拼接完成，结果保存为：", output_path, "\n")

# ------------------------------------------------------------------------------
library(terra)

terraOptions(progress = 1, memfrac = 0.9)

# 路径设置
input_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NO/NO.tif"
output_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NO/NO_500m.tif"

# 读取 NDVI 原图（10m, EPSG:4326）
ndvi_raw <- rast(input_path)

# 中国 Albers 投影（以米为单位）
china_albers <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 投影到 Albers（单位为米）
ndvi_albers <- project(ndvi_raw, china_albers)

# 聚合成 500m 分辨率（10m → 500m）
ndvi_500m_albers <- aggregate(ndvi_albers, fact = 50, fun = mean, na.rm = TRUE)

# 近似地将 500m 对应的经纬度分辨率设置为 ~0.0045°
# 这个值可以调整，更精确的方法是反过来从 Albers 算出对应度数
target_res_deg <- 0.0045

# 回投影到 WGS84（并设置目标解析度）
ndvi_500m_wgs84 <- project(ndvi_500m_albers, "EPSG:4326", res = target_res_deg)

# 保存为新的 500m 栅格（WGS84坐标）
writeRaster(ndvi_500m_wgs84, output_path, overwrite = TRUE)

cat("✅ 成功输出 500m 分辨率 NDVI 栅格至：", output_path, "\n")



