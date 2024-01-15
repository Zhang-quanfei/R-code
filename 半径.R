# 安装和加载需要的包
install.packages("sf")
library("sf")

# 创建一个空的数据框用于存储结果
result_data <- data.frame()

# 读取企业数据
firm_data <- read.csv("D:\\Stata14\\examples\\地级市宏观数据\\0942 【高级会员】金融机构数据库2007-2022（已更新）\\上市公司经纬度.csv", head=TRUE, sep=",")

# 读取银行数据
bank_data <- read.csv("D:\\Stata14\\examples\\地级市宏观数据\\0942 【高级会员】金融机构数据库2007-2022（已更新）\\金融许可证_面板(剔除退出).csv", head=TRUE, sep=",")

# 循环处理每一年的数据
for (i in 2001:2001) {
  # 根据年份筛选数据
  firm_data_year <- subset(firm_data, year == i)
  bank_data_year <- subset(bank_data, year == i)
  
  # 创建企业和银行的 sf 对象
  firm_sf_year <- st_as_sf(firm_data_year, coords = c("lng", "lat"), crs = 4326)
  bank_sf_year <- st_as_sf(bank_data_year, coords = c("经度", "纬度"), crs = 4326)
  
  # 使用 st_intersection 找到每个企业点半径5KM内的银行
  nearby_banks <- st_intersection(bank_sf_year, st_buffer(firm_sf_year, dist = 5000))
  
  # 计算在半径5KM内的银行数量
  banks_within_5km <- nrow(nearby_banks)
  
  # 创建结果数据框
  result_data_year <- data.frame(
    Year = rep(i, nrow(firm_data_year)),
    Firm_ID = firm_data_year$sid,
    Banks_Within_5KM = banks_within_5km
  )
  
  # 将结果追加到整个数据框
  result_data <- rbind(result_data, result_data_year)
}

# 将结果数据框写入 CSV 文件
write.csv(result_data, file = "F:\\Users\\zhang\\Desktop\\result_data.csv", row.names = FALSE)
