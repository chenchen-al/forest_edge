# ------------------------------------------------------------------------------图像拼接
library(terra)

# 设置文件夹路径
folder_path <- "D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Hum_Edge_raw_new/2020_100m"

# 获取所有 .tif 文件的完整路径
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# 加载所有栅格图像
rasters <- lapply(tif_files, rast)

# 拼接图像（自动拼接相邻栅格）
mosaic_raster <- do.call(mosaic, rasters)

# 输出路径，命名为 Nat_Edge_mosaic.tif
output_path <- file.path(folder_path, "Hum_Edge_China_2020_100m.tif")

# 保存拼接结果
writeRaster(mosaic_raster, output_path, overwrite = TRUE)

cat("拼接完成，结果保存为：", output_path, "\n")

# ------------------------------------------------------------------------------重采样至1km
library(terra)

r <- rast("D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Nat_Edge_raw/2020/Nat_Edge_China_2020.tif")
cat("输入投影:\n"); print(crs(r, proj=TRUE))

agg_fun <- function(x, na.rm=TRUE) {
  count_nonzero <- sum(!is.na(x) & x != 0)
  if (count_nonzero >= 10) {
    return(max(x, na.rm=TRUE))
  } else {
    return(0)
  }
}

r_agg <- aggregate(r, fact=10, fun=agg_fun)

# 强制赋投影
crs(r_agg) <- crs(r)
cat("输出投影:\n"); print(crs(r_agg, proj=TRUE))

writeRaster(r_agg, 
            "D:/Forest_Fragmentation/人为和自然归因/Edge_raster/Nat_Edge_raw/2020/Nat_Edge_China_2020_1km.tif",
            overwrite=TRUE)


