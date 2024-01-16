# csv文件太大转换sf对象，速度非常慢，我们可以先转换成shp，然后在转换成sf对象 #

# 安装和加载需要的包
install.packages("sf")
library("sf")

# 创建一个空的数据框用于存储结果
result_data <- data.frame()

# 读取企业数据
firm_data <- read.csv("D:\\Stata14\\examples\\地级市宏观数据\\金融机构数据库2007-2022（已更新）\\上市公司经纬度.csv", head=TRUE, sep=",")

# 读取银行 shapefile 数据，指定坐标系为 EPSG:4326
bank_sf <- st_read("F:/Users/zhang/Desktop/论文/拟写论文/银企距离对数字化转型/企业和银行shp/bank.shp", crs = 4326)


# 循环处理每一年的数据
for (i in 2000:2022) {
  # 根据年份筛选数据
  firm_data_year <- subset(firm_data, year == i)
  bank_sf_year <- subset(bank_sf, year == i)
  
  # 提取经纬度数据和企业ID
  firm_pos_year <- firm_data_year[, c("sid", "lng", "lat")]
  
  # 移除包含缺失值的行
  firm_pos_year <- firm_pos_year[complete.cases(firm_pos_year[, c("lng", "lat")]), ]
  
  # 转换成sf对象 
  firm_sf <- st_as_sf(firm_pos_year, coords = c("lng", "lat"), crs = 4326)  # WGS84
  
  # 使用 st_transform() 将企业 sf 对象转换为墨卡托投影
  firm_sf_mercator <- st_transform(firm_sf, crs = 3857)
  
  # 将银行数据框转换为 sf 对象
  bank_sf_mercator <- st_as_sf(bank_sf_year, coords = c("经度", "纬度"), crs = 4326)
  bank_sf_mercator <- st_transform(bank_sf_mercator, crs = 3857)
  
  # 使用 st_intersection 找到每个企业点半径5KM内的银行
  nearby_banks <- st_intersection(bank_sf_mercator, st_buffer(firm_sf_mercator, dist = 5000))
  
  # 过滤掉包含空值的行
  nearby_banks <- nearby_banks[sapply(nearby_banks, Negate(anyNA)), ]
  
  # 选择保留的列
  selected_columns <- c("sid", "id", "year", "机构名")
  
  # Use subset to select columns and exclude geometry column
  selected_data <- nearby_banks[, selected_columns, drop = FALSE]
  
  # 将结果追加到整个数据框
  result_data <- rbind(result_data, selected_data %>% st_drop_geometry())
}

# 将结果数据框写入 CSV 文件
write.csv(result_data, file = "F:/Users/zhang/Desktop/result_data.csv", row.names = FALSE)
