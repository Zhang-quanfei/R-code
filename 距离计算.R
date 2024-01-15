install.packages("RANN")
install.packages('kknn')
install.packages('proj4')

library("nabor")

library("RANN")

library("kknn")

library("proj4")


##### 

datamanu <- read.csv("/Users/zhangyepeng/Documents/高速可达性与出口产品质量/firmLoc/csv/2013.csv",head=TRUE,sep=",")

datamanu <- format(datamanu, digits = 20)



datafina <- read.csv("/Users/zhangyepeng/Documents/高速可达性与出口产品质量/stationLoc/csv/2013年份高速收费站点.csv",head=TRUE,sep=",")

datafina <- format(datafina, digits = 20)


manupos2 <- cbind(datamanu$longitude, datamanu$latitude)

manuposj2 <- project(manupos2, "+proj=merc")

manuposj2 <- format(manuposj2, digits = 20)



finapos2 <- cbind(datafina$lng, datafina$lat)

finaposj2 <- project(finapos2, "+proj=merc")

finaposj2 <- format(finaposj2, digits = 20)


manudata <- manuposj2

finadata <- finaposj2 



#######


### 最近的3个邻居


nearest2 <- knn(data=finadata, query=manudata, k=1)
str(nearest2)

summary(nearest2)


write.table(nearest2$nn.idx ,file="/Users/zhangyepeng/Documents/高速可达性与出口产品质量/distance/nearestList/nearest2013.csv",sep=",",quote=F,col.name=T,row.names=T)
write.table(nearest2$nn.dists ,file="/Users/zhangyepeng/Documents/高速可达性与出口产品质量/distance/nearestdistList/nearestdist2013.csv",sep=",",quote=F,col.name=T,row.names=T)


#############半径5公里

fulllimit = nrow(finadata)

nlimit= nrow(finadata)/10

nearestcount <- nn2(finadata ,query = manudata, k= nlimit  ,searchtype = "radius" ,radius = 5000)



countnb <- rowSums(nearestcount$nn.idx >0)

write.table(countnb  ,file="D:/data/countnb2012_5km.csv",sep=",",quote=F,col.name=T,row.names=T)



#############半径3公里

fulllimit = nrow(finadata)

nlimit= nrow(finadata)/10
nearestcount <- nn2(finadata ,query = manudata, k= nlimit  ,searchtype = "radius" ,radius = 3000)

countnb <- rowSums(nearestcount$nn.idx >0)

write.table(countnb  ,file="D:/data/countnb2012_3km.csv",sep=",",quote=F,col.name=T,row.names=T)






#############半径1公里

fulllimit = nrow(finadata)

nlimit= nrow(finadata)/10

nearestcount <- nn2(finadata ,query = manudata, k= nlimit  ,searchtype = "radius" ,radius = 1000)

countnb <- rowSums(nearestcount$nn.idx >0)

write.table(countnb  ,file="D:/data/countnb2012_1km.csv",sep=",",quote=F,col.name=T,row.names=T)

