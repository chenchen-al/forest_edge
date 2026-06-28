# ------------------------------------------------------------------------------裁剪
library(terra)

# 设置路径
input_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/OptTEM/World"
output_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/OptTEM/China"
shapefile <- "D:/Forest_Fragmentation/NPP/CASA_China/shengjie/shengjie.shp"

# 创建输出文件夹
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 读取矢量文件
china_shape <- vect(shapefile)

# 获取所有tif文件路径
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# 处理第一个 tif 获取坐标系
example_r <- rast(tif_files[1])
china_shape_proj <- project(china_shape, crs(example_r))

# 批量裁剪
for (tif_path in tif_files) {
  r <- rast(tif_path)
  r_crop <- crop(r, china_shape_proj)
  r_mask <- mask(r_crop, china_shape_proj)
  out_path <- file.path(output_dir, basename(tif_path))
  writeRaster(r_mask, out_path, overwrite = TRUE)
  cat("已处理:", basename(tif_path), "\n")
}
# ------------------------------------------------------------------------------opttem
rm(list = ls())
gc(reset = TRUE)
library(terra)

#===== 最优温度计算函数 =====#
CalOptTEM <- function(X){
  NDVISeries <- X[seq(1, (length(X) - 1), 2)]
  TEMSeries  <- X[seq(2, length(X), 2)]
  if (all(is.na(NDVISeries)) || any(is.na(TEMSeries))) {
    return(NaN)
  }
  selectID <- which.max(NDVISeries)
  return(TEMSeries[selectID[1]])  # 防止多值
}

#===== 设置路径 =====#
data_dir <- "D:/Forest_Fragmentation/NPP/CASA_China/OptTEM/China"
output_file <- file.path(data_dir, "OptTEM_2023.tif")

#===== 读取并组合 NDVI2022 和 TEM2023 栅格 =====#
rasters <- list()

for (j in 1:12) {
  ndvi_path <- file.path(data_dir, paste0("NDVI_2022_", j, ".tif"))
  tem_path  <- file.path(data_dir, paste0("TEM_2023_", j, ".tif"))
  
  ndvi_ras <- rast(ndvi_path)
  tem_ras  <- rast(tem_path) - 273.15  # 转为摄氏度
  
  tem_ras  <- resample(tem_ras, ndvi_ras, method = "cubic")  # 匹配分辨率
  rasters  <- c(rasters, ndvi_ras, tem_ras)
}

#===== 合并 list 为 SpatRaster =====#
rasters <- rast(rasters)

#===== 执行像元级计算（并行可选）=====#
optTEM <- app(rasters, fun = CalOptTEM, cores = 24)

#===== 保存结果 =====#
writeRaster(optTEM, output_file, overwrite = TRUE)
cat("OptTEM_2023 已保存到：", output_file, "\n")
