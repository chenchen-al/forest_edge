# ------------------------------------------------------------------------------9km聚合提取
library(terra)

# 1. 读取你的 1km 栅格数据
r <- rast("D:/Forest_Fragmentation/人为和自然归因/相关性分析/NDVI/NDVI_NaturalEdge_2020.tif")

# 2. 设置聚合因子（假设原图是1km）
fact <- 9

# 3. 聚合栅格，分别计算四个统计量
r_max    <- aggregate(r, fact=fact, fun=max,    na.rm=TRUE)
r_min    <- aggregate(r, fact=fact, fun=min,    na.rm=TRUE)
r_mean   <- aggregate(r, fact=fact, fun=mean,   na.rm=TRUE)
r_median <- aggregate(r, fact=fact, fun=median, na.rm=TRUE)

# 4. 合并成一个多波段栅格
r_summary <- c(r_max, r_min, r_mean, r_median)
names(r_summary) <- c("max", "min", "mean", "median")

# 5. 提取所有格网的中心坐标和统计值（保留NA格）
df <- as.data.frame(r_summary, xy=TRUE, na.rm=FALSE)

# 6. 保存为 CSV 文件
output_csv <- "D:/Forest_Fragmentation/人为和自然归因/相关性分析/NDVI/NDVI_NaturalEdge_2020_9km.csv"
write.csv(df, output_csv, row.names = FALSE, na = "NA")

cat("✅ 9km 聚合统计已保存至：", output_csv, "\n")


# 批量清洗文件夹下的所有 CSV 文件（去除 NA，保证所有文件行数一致）

# ------------------------------------------------------------------------------数据清洗
input_dir  <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/dh-dn"
output_dir <- file.path(input_dir, "cleaned")

# 如果输出文件夹不存在，就创建
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2. 获取所有 CSV 文件路径
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# 3. 读取所有 CSV 文件，存入列表
data_list <- lapply(files, read.csv)

# 4. 找出所有文件中都没有 NA 的行
valid_rows <- Reduce(`&`, lapply(data_list, complete.cases))

# 5. 对每个数据框进行清洗（只保留 valid_rows），并保存到新文件夹
for (i in seq_along(data_list)) {
  clean_data <- data_list[[i]][valid_rows, ]
  
  # 构造输出路径
  filename <- basename(files[i])
  out_path <- file.path(output_dir, sub("\\.csv$", "_clean.csv", filename))
  
  # 写出 CSV
  write.csv(clean_data, out_path, row.names = FALSE)
}

cat("✅ 清洗完成，文件已输出到:", output_dir, "\n")
# ------------------------------------------------------------------------------ 环境因子逐列差值（Edge - Core）
# 1. 读取两个清洗后的 CSV 文件
hum_core_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/dh-dn/cleaned/dLST_Nat_core_2020_9km_clean.csv")
hum_edge_clean <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/dh-dn/cleaned/dLST_Nat_edge_2020_9km_clean.csv")

# 2. 检查行列是否匹配
stopifnot(nrow(hum_core_clean) == nrow(hum_edge_clean))
stopifnot(ncol(hum_core_clean) == ncol(hum_edge_clean))

# 3. 前两列为坐标，直接保留；从第3列开始做差值
coord_cols <- hum_edge_clean[, 1:2]
diff_cols <- hum_edge_clean[, 3:ncol(hum_edge_clean)] - hum_core_clean[, 3:ncol(hum_core_clean)]

# 4. 合并坐标列和差值列
hum_diff_all <- cbind(coord_cols, diff_cols)

# 5. 输出结果到新文件
write.csv(hum_diff_all, "D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/dh-dn/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv", row.names = FALSE)
rm(list = ls())
gc(reset = TRUE)

# ------------------------------------------------------------------------------
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]
ppt_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PPT_NatEdge_2020_9km_clean.csv")[,5]
panda_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PANDA_NatEdge_2020_9km_clean.csv")[,5]
win_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Win_NatEdge_2020_9km_clean.csv")[,5]

dlst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv")[,5]
daet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dAET_NatEdge_minus_Core_2020_9km.csv")[,5]
dssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dSSM_NatEdge_minus_Core_2020_9km.csv")[,5]
dvpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dVPD_NatEdge_minus_Core_2020_9km.csv")[,5]
dppt_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPPT_NatEdge_minus_Core_2020_9km.csv")[,5]
dpanda_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPANDA_NatEdge_minus_Core_2020_9km.csv")[,5]
dwin_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dWin_NatEdge_minus_Core_2020_9km.csv")[,5]


height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
dem_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/DEM_Nat_2020_9km_clean.csv")[,5]

# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]
ppt_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PPT_HumEdge_2020_9km_clean.csv")[,5]
panda_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PANDA_HumEdge_2020_9km_clean.csv")[,5]
win_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Win_HumEdge_2020_9km_clean.csv")[,5]

dlst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dLST_HumEdge_minus_Core_2020_9km.csv")[,5]
daet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dAET_HumEdge_minus_Core_2020_9km.csv")[,5]
dssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv")[,5]
dvpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv")[,5]
dppt_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPPT_HumEdge_minus_Core_2020_9km.csv")[,5]
dpanda_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPANDA_HumEdge_minus_Core_2020_9km.csv")[,5]
dwin_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dWin_HumEdge_minus_Core_2020_9km.csv")[,5]

height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]
dem_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/DEM_Hum_2020_9km_clean.csv")[,5]

# 森林类型
forest_tape <- read.csv(
   "D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/森林类型/ForestType_2020_9km_clean.csv")[, 3]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  PPT = ppt_nat,
  ALAN =  panda_nat,
  Win =  win_nat,
  
  dLST = dlst_nat,
  dAET = daet_nat,
  dSSM = dssm_nat,
  dVPD = dvpd_nat,
  dPPT = dppt_nat,
  dALAN =  dpanda_nat,
  dWin =  dwin_nat,
  
  Height = height_nat,
  Age = age_nat,
  DEM =  dem_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  PPT = ppt_hum,
  ALAN =  panda_hum,
  Win =  win_hum,
  
  dLST = dlst_hum,
  dAET = daet_hum,
  dSSM = dssm_hum,
  dVPD = dvpd_hum,
  dPPT = dppt_hum,
  dALAN =  dpanda_hum,
  dWin =  dwin_hum,
  
  Height = height_hum,
  Age = age_hum,
  DEM =  dem_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:19]
hum_values <- edge_df[edge_df$type == "Hum", 2:19]

# 检查行数是否一致
if (nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 生成差值数据框
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  PPT_diff = diff_values$PPT,
  ALAN_diff = diff_values$ALAN,
   Win_diff = diff_values$Win, 
  
  dLST_diff = diff_values$dLST,
  dAET_diff = diff_values$dAET,
  dSSM_diff = diff_values$dSSM,
  dVPD_diff = diff_values$dVPD,
  dPPT_diff = diff_values$dPPT,
  dALAN_diff = diff_values$dALAN,
   dWin_diff = diff_values$dWin, 
  
  Height_diff = diff_values$Height,
  Age_diff = diff_values$Age,
  # Elevation_diff = diff_values$DEM,
   forest_type = forest_tape
)

# ------------------------------------------------------------------
# 1️⃣ 标准化差值数据
diff_std <- as.data.frame(scale(diff_df))

# 2️⃣ 构建全模型（包含新变量）
lm_full <- lm(NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff +PPT_diff +ALAN_diff +Win_diff+
                dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +dPPT_diff +dALAN_diff +dWin_diff+
                Height_diff + Age_diff  +forest_type, data = diff_std)

# 3️⃣ 多重共线性（在全模型上检查）
vif_values <- vif(lm_full)
print("全模型 VIF：")
print(vif_values)

# 4️⃣ 逐步回归（AIC 准则，按 AIC 自动选变量）
lm_step <- step(lm_full, direction = "both", trace = FALSE)

# 5️⃣ 最终模型结果
summary(lm_step)

# 6️⃣ 最终模型 VIF（若 lm_step 仅含截距，vif 会报错；为稳健起见先判断）
if (length(coef(lm_step)) > 1) {
  vif_final <- vif(lm_step)
  print("最终模型 VIF：")
  print(vif_final)
} else {
  print("最终模型仅含截距，无法计算 VIF。")
}

# 7️⃣ 标准化回归系数（lm_step 已基于标准化数据拟合）
coef_final <- coef(lm_step)
print("标准化系数：")
print(coef_final)

# 8️⃣ 计算原始差值数据的相关性矩阵
cor_matrix <- cor(diff_df, use = "complete.obs")
print("差值变量相关性矩阵：")
print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------相关性矩阵作图Elevation
library(corrplot)
png("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/correlation.png",
    width = 1200, height = 1200, res = 150)

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 1.0,
         tl.srt = 90,
         cl.cex = 1.0,
         mar = c(0,0,1,0))

dev.off()

# 查看全模型和逐步回归模型的解释力
cat("全模型 R-squared:", summary(lm_full)$r.squared, "\n")
cat("逐步回归最终模型 R-squared:", summary(lm_step)$r.squared, "\n")

# ------------------------------------------------------------------------------RF随机森林
# 读取人为边缘 NDVI 和气候指标
library(car)
# ------------------------------------------------------------------
# 自然边缘
ndvi_nat <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/NDVI_Nat_2020_9km_clean.csv")[,5]
lst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/LST_NatEdge_2020_9km_clean.csv")[,5]
aet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/AET_NatEdge_2020_9km_clean.csv")[,5]
ssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/SSM_NatEdge_2020_9km_clean.csv")[,5]
vpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/VPD_NatEdge_2020_9km_clean.csv")[,5]
ppt_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PPT_NatEdge_2020_9km_clean.csv")[,5]
panda_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PANDA_NatEdge_2020_9km_clean.csv")[,5]
win_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Win_NatEdge_2020_9km_clean.csv")[,5]

dlst_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dLST_NatEdge_minus_Core_2020_9km.csv")[,5]
daet_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dAET_NatEdge_minus_Core_2020_9km.csv")[,5]
dssm_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dSSM_NatEdge_minus_Core_2020_9km.csv")[,5]
dvpd_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dVPD_NatEdge_minus_Core_2020_9km.csv")[,5]
dppt_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPPT_NatEdge_minus_Core_2020_9km.csv")[,5]
dpanda_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPANDA_NatEdge_minus_Core_2020_9km.csv")[,5]
dwin_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dWin_NatEdge_minus_Core_2020_9km.csv")[,5]


height_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Height_Nat_2020_9km_clean.csv")[,5]
age_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Age_Nat_2020_9km_clean.csv")[,5]
dem_nat  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/DEM_Nat_2020_9km_clean.csv")[,5]

# 人为边缘
ndvi_hum <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/NDVI_Hum_2020_9km_clean.csv")[,5]
lst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/LST_HumEdge_2020_9km_clean.csv")[,5]
aet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/AET_HumEdge_2020_9km_clean.csv")[,5]
ssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/SSM_HumEdge_2020_9km_clean.csv")[,5]
vpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/VPD_HumEdge_2020_9km_clean.csv")[,5]
ppt_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PPT_HumEdge_2020_9km_clean.csv")[,5]
panda_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/PANDA_HumEdge_2020_9km_clean.csv")[,5]
win_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Win_HumEdge_2020_9km_clean.csv")[,5]

dlst_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dLST_HumEdge_minus_Core_2020_9km.csv")[,5]
daet_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dAET_HumEdge_minus_Core_2020_9km.csv")[,5]
dssm_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dSSM_HumEdge_minus_Core_2020_9km.csv")[,5]
dvpd_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dVPD_HumEdge_minus_Core_2020_9km.csv")[,5]
dppt_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPPT_HumEdge_minus_Core_2020_9km.csv")[,5]
dpanda_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dPANDA_HumEdge_minus_Core_2020_9km.csv")[,5]
dwin_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/dWin_HumEdge_minus_Core_2020_9km.csv")[,5]

height_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Height_Hum_2020_9km_clean.csv")[,5]
age_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/Age_Hum_2020_9km_clean.csv")[,5]
dem_hum  <- read.csv("D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/cleaned/DEM_Hum_2020_9km_clean.csv")[,5]

# 森林类型
forest_tape <- read.csv(
  "D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/森林类型/ForestType_2020_9km_clean.csv")[, 3]

# ------------------------------------------------------------------
# 合并自然边缘数据
nat_df <- data.frame(
  type = "Nat",
  NDVI = ndvi_nat,
  LST  = lst_nat,
  AET  = aet_nat,
  SSM  = ssm_nat,
  VPD  = vpd_nat,
  PPT = ppt_nat,
  PANDA =  panda_nat,
  Win =  win_nat,
  
  dLST = dlst_nat,
  dAET = daet_nat,
  dSSM = dssm_nat,
  dVPD = dvpd_nat,
  dPPT = dppt_nat,
  dPANDA =  dpanda_nat,
  dWin =  dwin_nat,
  
  Height = height_nat,
  Age = age_nat
  # DEM =  dem_nat
)

# 合并人为边缘数据
hum_df <- data.frame(
  type = "Hum",
  NDVI = ndvi_hum,
  LST  = lst_hum,
  AET  = aet_hum,
  SSM  = ssm_hum,
  VPD  = vpd_hum,
  PPT = ppt_hum,
  PANDA =  panda_hum,
  Win =  win_hum,
  
  dLST = dlst_hum,
  dAET = daet_hum,
  dSSM = dssm_hum,
  dVPD = dvpd_hum,
  dPPT = dppt_hum,
  dPANDA =  dpanda_hum,
  dWin =  dwin_hum,
  
  Height = height_hum,
  Age = age_hum
  # DEM =  dem_hum
)

# ------------------------------------------------------------------
# 合并成一个总表
edge_df <- rbind(nat_df, hum_df)

# 确保顺序正确：先 Nat，再 Hum
nat_values <- edge_df[edge_df$type == "Nat", 2:18]
hum_values <- edge_df[edge_df$type == "Hum", 2:18]

# 检查行数是否一致
if (nrow(nat_values) != nrow(hum_values)) stop("Nat 与 Hum 的行数不一致！")

# 按元素相减（Hum - Nat）
diff_values <- hum_values - nat_values

# 生成差值数据框
diff_df <- data.frame(
  NDVI_diff = diff_values$NDVI,
  LST_diff  = diff_values$LST,
  AET_diff  = diff_values$AET,
  SSM_diff  = diff_values$SSM,
  VPD_diff  = diff_values$VPD,
  PPT_diff = diff_values$PPT,
  ALAN_diff = diff_values$PANDA,
  Win_diff = diff_values$Win, 
  
  dLST_diff = diff_values$dLST,
  dAET_diff = diff_values$dAET,
  dSSM_diff = diff_values$dSSM,
  dVPD_diff = diff_values$dVPD,
  dPPT_diff = diff_values$dPPT,
  dALAN_diff = diff_values$dPANDA,
  dWin_diff = diff_values$dWin, 
  
  Height_diff = diff_values$Height,
  Age_diff = diff_values$Age,
  #DEM_diff = diff_values$DEM,
   forest_type = forest_tape
)

library(randomForest)
library(ggplot2)
library(iml)


set.seed(1234)

# 1️⃣ 构建随机森林模型
rf_model <- randomForest(
  NDVI_diff ~ LST_diff + AET_diff + SSM_diff + VPD_diff +PPT_diff +ALAN_diff +Win_diff +
    dLST_diff + dAET_diff + dSSM_diff + dVPD_diff +dPPT_diff +dALAN_diff +dWin_diff +
    Height_diff + Age_diff+ forest_type,
  data = diff_df,
  ntree = 1000,
  importance = TRUE
)

# =========================
# 2️⃣ OOB模型表现
# =========================
y <- diff_df$NDVI_diff
y_pred <- rf_model$predicted

model_R2 <- 1 - mean((y - y_pred)^2) / var(y)
RMSE <- sqrt(mean((y - y_pred)^2))

cat("OOB R² =", model_R2, "\n")
cat("RMSE =", RMSE, "\n")


# =========================
# 3️⃣ 变量重要性
# =========================
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# 🔥 按 %IncMSE 从高到低排序（关键）
importance_df <- importance_df[
  order(importance_df$`%IncMSE`, decreasing = TRUE),
]


# =========================
# 4️⃣ 固定顺序（防 ggplot 乱序核心步骤）
# =========================
importance_df$Variable <- factor(
  importance_df$Variable,
  levels = importance_df$Variable
)


# =========================
# 5️⃣ 只保留论文常用列
# =========================
var_imp <- importance_df[, c("Variable", "%IncMSE", "IncNodePurity")]
print(var_imp)


# =========================
# 6️⃣ 作图
# =========================
output_path <- "D:/Forest_Fragmentation/人为和自然归因/d相关性分析8.0(All叠加风速降水夜间灯光)/RF_importance_R.png"

p <- ggplot(importance_df,
            aes(x = Variable,
                y = `%IncMSE`)) +
  
  geom_bar(stat = "identity",
           fill = "#4C9F70",
           width = 0.6) +
  
  scale_y_continuous(expand = c(0, 5)) +
  
  theme_bw(base_size = 30) +
  
  labs(
    x = "Variables",
    y = "%IncMSE",
    title = "Variable Contributions to NDVI Differences"
  ) +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 1),
    
    axis.line = element_line(color = "black",
                             linewidth = 0.8),
    
    axis.ticks = element_line(color = "black",
                              linewidth = 0.8),
    
    axis.ticks.length = unit(0.25, "cm"),
    
    panel.grid = element_blank(),
    
    axis.text.x = element_text(angle = 45,
                               hjust = 1,
                               color = "black"),
    axis.text.y = element_text(color = "black")
  )

print(p)

ggsave(output_path,
       plot = p,
       width = 20,
       height = 7,
       dpi = 600)


cat("✅ RF变量重要性图已保存：", output_path, "\n")
