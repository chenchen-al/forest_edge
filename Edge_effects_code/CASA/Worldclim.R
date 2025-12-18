data <- read_excel("D:\\Nonlinear\\1——几千多行的数据\\data2.xlsx")
data$Latitude <- round(data$Latitude, 2) #提取维度，并仅保留两位小数
data$Longitude <- round(data$Longitude, 2) #提取纬度，并仅保留两位小数
coords <- data.frame(Longitude=data$Longitude, Latitude=data$Latitude)  #将提取的经纬度化成数据框
coords <- as.matrix(coords)  #将其化为matrix的数据类型

##############################worldclima data#########################
#####从遥感数据里面提取降雨数据(提取每一个月份的)，并对每一个月的降雨求和（年降雨量）#####
setwd("D:\\组会汇报\\准备工作\\温度和降水生态系统的的数据包")  
p.mean.files <- list.files("wc2.1_30s_prec", ".tif", full.names=TRUE)     
p.mean <- stack(p.mean.files) 
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(p.mean) <- month 
coords <- na.omit(coords)
p.points <- SpatialPoints(coords, proj4string = p.mean@crs)
prec.data <- raster::extract(p.mean,p.points)
df.rain <- cbind.data.frame(coordinates(p.points),prec.data)
df.annual.rain <- df.rain %>% 
  rowwise() %>%
  mutate(annual.rain = sum(c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))) %>%
  dplyr::select(Longitude, Latitude, annual.rain)

#################从遥感数据里面提取大气温度数据(提取每一个月份)###############计算平均温度的代码在后面
t.mean.files <- list.files("wc2.1_30s_tavg", ".tif", full.names=TRUE)     
t.mean <- stack(t.mean.files)  #镶嵌这些文件
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(t.mean) <- month
t.points <- SpatialPoints(coords, proj4string = t.mean@crs)
temp.data <- raster::extract(t.mean, t.points)
nrow(temp.data)
df.temp<- cbind.data.frame(coordinates(t.points),temp.data)