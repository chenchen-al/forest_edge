library(ggplot2)
library(dplyr)
library(readr)

# 读取数据，不把第一行当列名（确保第一行是数据）
file_path <- "D:/Forest_Fragmentation/Picture/FE_area/df_af/percent_df.csv"
df <- read_csv(file_path, col_names = FALSE)

# 指定列名
colnames(df) <- c("Category", "Area")

# 保持类别顺序和文件一致
df$Category <- factor(df$Category, levels = df$Category)

# 计算百分比和标签
df <- df %>%
  mutate(Percent = Area / sum(Area) * 100,
         Label = paste0(Category, " (", round(Percent, 1), "%)"))

# 按照文件顺序指定颜色
my_colors <- c(
  "Forest"             = "#228B22",
  "Anthropogenic land" = "darkgrey",
  "Natural land"       = "#FFFFFF",
  "Remained"           = "#ADFF2F",
  "Natural Edge"       = "#00008B"
)

# 绘制饼图
p <- ggplot(df, aes(x = "", y = Percent, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "black") + # 扇形外围黑色边线
  coord_polar(theta = "y") +
  scale_fill_manual(values = my_colors, labels = df$Label) + # 自定义颜色，图例显示类别+比例
  theme_void() +
  theme(
    legend.position = c(0.90, 0.15), # 图例右下角
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# 显示图形
print(p)

# 保存高分辨率图像
ggsave("D:/Forest_Fragmentation/Picture/FE_area/df_af/percent_df_pie_custom.png",
       p, width = 10, height = 10, dpi = 600)

# ==============================================================================合并后

library(ggplot2)
library(dplyr)
library(readr)

# ==== 1. 读取数据 ====
file_path <- "D:/Forest_Fragmentation/Picture/FE_area/df_af/percent_af.csv"
df <- read_csv(file_path, col_names = FALSE)
colnames(df) <- c("Category", "Area")

# ==== 2. 合并类别 ====
df <- df %>%
  mutate(NewCategory = case_when(
    Category == "Forest" ~ "Forest",
    Category %in% c("Anthropogenic land", "Natural land") ~ "land",
    Category %in% c("Natural Edge", "Remained") ~ "edge",
    TRUE ~ Category
  )) %>%
  group_by(NewCategory) %>%
  summarise(Area = sum(Area), .groups = "drop") %>%
  mutate(
    Percent = Area / sum(Area) * 100,
    Label = paste0(NewCategory, " (", round(Percent, 1), "%)")
  )

# ==== 3. 自定义颜色（按新类别）====
my_colors <- c(
  "Forest" = "#228B22",  # 森林
  "land"   = "darkgrey", # 土地
  "edge"   = "#ADFF2F"   # 边缘
)

# ==== 4. 绘制饼图 ====
p <- ggplot(df, aes(x = "", y = Percent, fill = NewCategory)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = my_colors, labels = df$Label) +
  theme_void() +
  theme(
    legend.position = c(0.90, 0.15),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# ==== 5. 显示与保存 ====
print(p)

ggsave("D:/Forest_Fragmentation/Picture/FE_area/df_af/percent_af_pie_2.0.png",
       p, width = 10, height = 10, dpi = 600)

