rm(list = rm())
gc(reset = TRUE)
library(terra)

##==== Function ====##
CalCASANPP <- function(X){
  if(any(is.na(X))){
    Y <- NaN
    return(Y)
  }
  NDVI <- X[1]
  TEM <- X[2]
  OptTEM <- X[3]
  SRAD <- X[4]
  SRMax <- X[5]
  WStress <- X[6]
  # Calculate APAR
  SR <- (1 + NDVI)/(1 - NDVI)
  FPAR <- (SR - 1.08)/(SRMax - 1.08)
  FPAR <- min(FPAR,0.95)
  SRAD <- SRAD*3600*24*30/1000000
  APAR <- SRAD*FPAR*0.5
  # Calculate temperature stress 1
  TStress1 <- 0.8 + 0.02*OptTEM - 0.0005*OptTEM*OptTEM
  if(TEM < -10){
    TStress1 <- 0
  }
  if(TStress1 < 0){
    TStress1 <- 0
  }
  # Calculate temperature stress 2
  coeff1 <- 1 + exp(0.2*(OptTEM - 10 - TEM))
  coeff2 <- 1 + exp(0.3*(TEM - 10 - OptTEM))
  TStress2 <- 1.1814/coeff1/coeff2
  coeff1 <- 1 + exp(-2)
  coeff1 <- 1 + exp(-3)
  OptTStress2 <- 1.1814/coeff1/coeff2
  if((TEM > OptTEM + 10) | (TEM < OptTEM - 13)){
    TStress2 <- OptTStress2/2
  }
  if(TStress2 < 0){
    TStress2 <- 0
  }
  # Calculate NPP
  Y <- APAR*TStress1*TStress2*WStress*0.405
  if(Y < 0){
    Y <- 0
  }
  return(Y)
}
##==== Function ====##

refRaster <- rast(nrows = 2160,ncols = 4320,nlyrs = 1,xmin = -180,xmax = 180,ymin = -90,ymax = 90,vals = NaN,crs = crs('EPSG:4326'))
for(i in seq(2008,2022)){
  OptTEMRaster <- rast(paste0('../NPP/OptTEM/OptTEM_',as.character(i),'.tif'))
  OptTEMRaster <- resample(OptTEMRaster,refRaster,method = 'near')
  for(j in seq(1,12)){
    NDVIRaster <- rast(paste0('../NPP/NDVI/NDVI_',as.character(i),'_',as.character(j),'.tif'))
    NDVIRaster <- resample(NDVIRaster,refRaster,method = 'near')
    TEMRaster <- rast(paste0('../NPP/TEM/TEM_',as.character(i),'_',as.character(j),'.tif'))
    TEMRaster <- TEMRaster - 273.15
    TEMRaster <- resample(TEMRaster,refRaster,method = 'cubic')
    SRADRaster <- rast(paste0('../NPP/SRAD/SRAD_',as.character(i),'_',as.character(j),'.tif'))
    SRADRaster <- resample(SRADRaster,refRaster,method = 'cubic')
    SRMaxRaster <- rast('../NPP/VEG/SRMax.tif')
    SRMaxRaster <- resample(SRMaxRaster,refRaster,method = 'near')
    WStressRaster <- rast(paste0('../NPP/WaterStress/WStress_',as.character(i),'_',as.character(j),'.tif'))
    WStressRaster <- resample(WStressRaster,refRaster,method = 'near')
    rasters <- c(NDVIRaster,TEMRaster,OptTEMRaster,SRADRaster,SRMaxRaster,WStressRaster)
    NPPRaster <- app(rasters,fun = CalCASANPP,cores = 24)
    outputFileName <- paste0('../NPP/NPP_',as.character(i),'_',as.character(j),'.tif')
    writeRaster(NPPRaster,outputFileName,overwrite = TRUE)
  }
}


# ------------------------------------------------------------------------------
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
NDVI     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/NDVI/Raw/NW/NW_500m.tif")
TEM      <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/WaterStress_500m/TEM_2023_resampled_500m.tif")
OptTEM   <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/OptTEM_resampled_500m.tif")
SRAD     <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRAD_2023_resampled_500m.tif")
SRMax    <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/SRMax_resampled_500m.tif")
WStress  <- rast("D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/WaterStress_500m/WStress_2023_annual_500m.tif")

# 合并为一个栅格集合
stacked <- c(NDVI, TEM, OptTEM, SRAD, SRMax, WStress)

# 计算 NPP
NPPRaster <- app(stacked, fun = CalCASANPP, cores = 12)

# 输出到目标路径
writeRaster(
  NPPRaster,
  filename = "D:/Forest_Fragmentation/NPP/CASA_China/regions/China_part/NW/NPP_500m.tif",
  overwrite = TRUE
)

# 打印统计信息
print(global(NPPRaster, fun = c("min", "max", "mean"), na.rm = TRUE))

# ✅ 最后一行：成功提示
print("NPP计算完成并成功保存！")

