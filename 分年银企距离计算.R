# 安装所需的包
install.packages("RANN")
install.packages("sf")

# 加载库
library("RANN")
library("sf")

# 创建一个空数据框用于存储结果
merged_result <- data.frame()
# 读取企业数据
firm_data <- read.csv("D:\\Stata14\\examples\\地级市宏观数据\\0942 【高级会员】金融机构数据库2007-2022（已更新）\\上市公司经纬度.csv", head=TRUE, sep=",")

# 读取银行数据
bank_data <- read.csv("D:\\Stata14\\examples\\地级市宏观数据\\0942 【高级会员】金融机构数据库2007-2022（已更新）\\金融许可证_面板(剔除退出).csv", head=TRUE, sep=",")


# 循环处理每一年的数据
for (i in 2000:2022) {
  
  # 根据年份筛选数据
  firm_data_year <- subset(firm_data, year == i)
  bank_data_year <- subset(bank_data, year == i)
  
  # 提取经纬度数据和企业ID
  firm_pos_year <- firm_data_year[, c("sid", "lng", "lat")]
  bank_pos_year <- bank_data_year[, c("经度", "纬度")]
  
  # 删除包含缺失值的行
  firm_pos_year <- firm_pos_year[complete.cases(firm_pos_year$lng, firm_pos_year$lat), ]
  bank_pos_year <- bank_pos_year[complete.cases(bank_pos_year$经度, bank_pos_year$纬度), ]
  
  # 检查数据是否为空
  if (nrow(firm_pos_year) == 0 || nrow(bank_pos_year) == 0) {
    cat("No data for year", i, "\n")
    next  # 继续下一次循环
  }
  
  # 创建 sf 对象
  firm_sf <- st_as_sf(firm_pos_year, coords = c("lng", "lat"), crs = 4326)  # WGS84
  bank_sf <- st_as_sf(bank_pos_year, coords = c("经度", "纬度"), crs = 4326)  # WGS84
  
  # 转换坐标系为墨卡托投影
  firm_sf_merc <- st_transform(firm_sf, crs = 3857)  # Web Mercator
  bank_sf_merc <- st_transform(bank_sf, crs = 3857)  # Web Mercator
  
  # 检查是否有足够的数据点
  if (nrow(st_coordinates(firm_sf_merc)) == 0 || nrow(st_coordinates(bank_sf_merc)) == 0) {
    cat("No coordinates for year", i, "\n")
    next  # 继续下一次循环
  }
  
  # 计算最近的3个邻居
  nearest_indices <- RANN::nn2(st_coordinates(bank_sf_merc), st_coordinates(firm_sf_merc), k = 3)$nn.idx
  nearest_dists <- RANN::nn2(st_coordinates(bank_sf_merc), st_coordinates(firm_sf_merc), k = 3)$nn.dists
  
  # 获取最近银行的信息
  nearest_bank_info <- bank_data_year[nearest_indices, ]
  
  # 创建结果数据框
  result_data <- data.frame(
    Year = rep(i, nrow(nearest_indices)),
    Firm_ID = firm_pos_year$sid,
    Nearest_Distance_1 = nearest_dists[, 1],
    Nearest_Distance_2 = nearest_dists[, 2],
    Nearest_Distance_3 = nearest_dists[, 3],
    Nearest_Distance = rowMeans(nearest_dists, na.rm = TRUE)  # 计算平均距离
  )
  
  # 将结果追加到整个数据框
  merged_result <- rbind(merged_result, result_data)
}

# 将整个数据框写入 CSV 文件
write.csv(merged_result, file="F:\\Users\\zhang\\Desktop\\merged_result.csv", row.names=FALSE)
