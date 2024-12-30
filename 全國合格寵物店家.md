# 載入 tidyverse 套件
library(tidyverse)

# 讀取 CSV 檔案
file_path <- "List of Licensed Pet Businesses.csv"
data <- read_csv(file_path)

# 檢視數據結構
glimpse(data)

# 假設地址欄名稱為 "address"，從中提取縣與市
data <- data %>%
  mutate(
    County = str_extract(address, "^[^市縣]+[市縣]"),  # 提取縣市部分
    City = str_extract(address, "[^區]+區")            # 提取市區部分（假設需要市區）
  )

# 檢視結果
head(data)

# 保存修改後的數據回原檔案
write_csv(data, file_path)

# 載入 tidyverse 套件
library(tidyverse)

# 設定檔案路徑
file_path <- "List of Licensed Pet Businesses.csv"

# 讀取原始檔案
data <- read_csv(file_path)

# 確認欄位名稱
print(colnames(data))

# 確保 "County" 欄位存在，若不存在請先新增
# 提取地址中的縣市部分 (假設地址欄為 "Address")
if (!"County" %in% colnames(data)) {
  data <- data %>%
    mutate(
      County = str_extract(Address, "^[^市縣]+[市縣]")  # 提取縣市部分
    )
}

# 按 "County" 欄位排序
sorted_data <- data %>%
  arrange(County)

# 查看排序後數據
head(sorted_data)

# 覆蓋保存到原始檔案
write_csv(sorted_data, file_path)

# 載入必要套件
library(tidyverse)

# 設定檔案路徑
file_path <- "List of Licensed Pet Businesses.csv"

# 讀取數據
data <- read_csv(file_path)

# 確保存在 "County" 欄位，否則從地址提取
if (!"County" %in% colnames(data)) {
  data <- data %>%
    mutate(
      County = str_extract(Address, "^[^市縣]+[市縣]")  # 提取縣市部分
    )
}

# 按縣市進行統計
county_summary <- data %>%
  group_by(County) %>%
  summarise(Total = n()) %>%
  mutate(Percentage = Total / sum(Total) * 100)

# 繪製平面柱狀圖
plot <- ggplot(county_summary, aes(x = reorder(County, -Percentage), y = Percentage, fill = County)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5) +
  labs(
    title = "Proportion of Businesses by County",
    x = "County",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

# 查看圖形
print(plot)

# 保存圖形
ggsave("Proportion_of_Businesses_by_County_BarChart.png", plot = plot, width = 10, height = 6)

# 載入 tidyverse 套件
library(tidyverse)

# 設定檔案路徑
file_path <- "List of Licensed Pet Businesses.csv"

# 讀取數據
data <- read_csv(file_path)

# 確保存在 "County" 欄位，否則從地址提取
if (!"County" %in% colnames(data)) {
  data <- data %>%
    mutate(
      County = str_extract(Address, "^[^市縣]+[市縣]")  # 提取縣市部分
    )
}

# 確保存在 "animaltype" 欄位
if (!"animaltype" %in% colnames(data)) {
  stop("The 'animaltype' column is missing in the data.")
}

# 統計各縣市中不同 animaltype 的比率
animaltype_summary <- data %>%
  group_by(County, animaltype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(County) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# 查看統計結果
head(animaltype_summary)

# 保存統計結果回原檔案
write_csv(animaltype_summary, file_path)
