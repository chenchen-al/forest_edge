rm(list = ls())
gc(reset = TRUE)
library(terra)

refRaster <- rast(nrows = 2160,ncols = 4320,nlyrs = 1,xmin = -180,xmax = 180,ymin = -90,ymax = 90,vals = 0,crs = crs('EPSG:4326'))
NDVIRaster <- rast('../NPP/NDVI/NDVI_1982_8.tif')
VEGRaster <- rast('../NPP/VEG/VegetationType.tif')
VEGRaster <- resample(VEGRaster,refRaster,method = 'near')
VEGRaster[VEGRaster == 0] <- NaN
VEGRaster[VEGRaster > 12] <- NaN
rasters <- c(NDVIRaster,VEGRaster)
rastersCoords <- crds(rasters,na.rm = FALSE)
rastersValues <- values(rasters)
selectIDs_1 <- which((!is.na(rastersValues[,1])) & (is.na(rastersValues[,2])))
selectIDs_2 <- which(!is.na(rastersValues[,2]))
for(i in seq(1,length(selectIDs_1))){
  tempX <- as.numeric(rastersCoords[selectIDs_1[i],1])
  tempY <- as.numeric(rastersCoords[selectIDs_1[i],2])
  valueXs <- as.numeric(rastersCoords[selectIDs_2,1])
  valueYs <- as.numeric(rastersCoords[selectIDs_2,2])
  diffXs <- abs(tempX - valueXs)
  diffYs <- abs(tempY - valueYs)
  diffCoords <- diffXs + diffYs
  selectID <- as.numeric(which.min(diffCoords))
  if(length(selectID) > 1){
    selectID <- selectID[1]
  }
  rastersValues[selectIDs_1[i],2] <- as.numeric(rastersValues[selectIDs_2[selectID],2])
  print(i)
}
VEGRasterMatrix <- data.frame(X = rastersCoords[,1],Y = rastersCoords[,2],Value = rastersValues[,2])
VEGRaster <- rast(VEGRasterMatrix,type = 'xyz',crs = crs('EPSG:4326'))
VEGRaster[VEGRaster %in% c(1,6)] <- 4.14
VEGRaster[VEGRaster %in% c(2,3)] <- 6.17
VEGRaster[VEGRaster %in% c(4,5)] <- 5.43
VEGRaster[VEGRaster %in% c(7,8,9,10,11,12)] <- 5.13
outputFileName <- '../NPP/VEG/SRMax.tif'
writeRaster(VEGRaster,outputFileName,overwrite = TRUE)


# ------------------------------------------------------------------------------
# landcover数据裁剪至China区域
# 清空环境
rm(list = ls())
gc(reset = TRUE)
library(terra)

# 设置路径
lc_path <- "D:/Forest_Fragmentation/NPP/CASA_China/LandCover/LandCover_2023.tif"
shp_path <- "D:/Forest_Fragmentation/NPP/CASA_China/shengjie/shengjie.shp"
out_path <- "D:/Forest_Fragmentation/NPP/CASA_China/LandCover/LandCover_2023_China.tif"

# 读取数据
landcover <- rast(lc_path)
china_shp <- vect(shp_path)

# 获取并统一投影（非常重要）
china_shp <- project(china_shp, crs(landcover))

# 裁剪并掩膜
landcover_crop <- crop(landcover, china_shp)
landcover_china <- mask(landcover_crop, china_shp)

# 保存输出
writeRaster(landcover_china, out_path, overwrite = TRUE)

# ------------------------------------------------------------------------------
# ------------------ 清空内存 ------------------
rm(list = ls())          # 清空工作空间变量
gc(reset = TRUE)         # 强制进行垃圾回收
terra::terraOptions(progress = 1, memfrac = 0.9)  # 限制terra内存使用
# 构建可用于 CASA 模型的 SRMax 输入栅格
# 使用 terra 包 + focal() 方法进行高效最近邻填补
# ------------------------------------------------------------------------------
library(terra)

# 设置路径
ndvi_path <- "D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NW/NW_500m.tif"
veg_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/LandCover_2023_China.tif"
output_path <- "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRMax_500m.tif"

# 读取栅格
NDVI <- rast(ndvi_path)
VEG <- rast(veg_path)

# 确保投影一致并重采样
if (!identical(crs(NDVI), crs(VEG))) {
  VEG <- project(VEG, crs(NDVI))
}
VEG <- resample(VEG, NDVI, method = "near")

# 设置无效值为 NA
VEG[VEG == 0 | VEG > 12] <- NA

# 使用 focal 填补缺失值：3x3 滑动窗口找最近邻（近似）
fill_na <- function(x, ...) {
  center <- x[5]  # 中心像元
  if (!is.na(center)) return(center)
  non_na <- x[!is.na(x)]
  if (length(non_na) == 0) return(NA)
  return(non_na[1])  # 取第一个非 NA 值
}

VEG_filled <- focal(VEG, w = matrix(1, 3, 3), fun = fill_na,
                    na.policy = "only", expand = TRUE)

# 映射为 SRMax 栅格
SRMax <- VEG_filled
SRMax[SRMax %in% c(1,6)]  <- 4.14
SRMax[SRMax %in% c(2,3)]  <- 6.17
SRMax[SRMax %in% c(4,5)]  <- 5.43
SRMax[SRMax %in% c(7:12)] <- 5.13

# 写入结果
writeRaster(SRMax, output_path, overwrite = TRUE)

cat("\u2705 SRMax 栅格成功生成：", output_path, "\n")

