
library(sp) # 地图
library(spdep) ## 空间自相关计算

############################# OLS ###############################
names(citydata_10)[6] <- 'GDP_per'
names(citydata_10)[1] <- 'city'
names(citydata_10)[8:10] <- c('GDP_1','GDP_2','GDP_3')
names(citydata_10)[13] <- 'workers'
names(citydata_10)[15] <- 'workers_private'

citydata_10[,2:(ncol(citydata_10))] <- sapply(citydata_10[,2:(ncol(citydata_10))],as.numeric)

citydata_10$market <- (citydata_10$workers_private/(citydata_10$workers*10000))

citydata <- left_join(city_D_Ds,citydata_10[,c('city','GDP_1','GDP_per','GDP_2','GDP_3',"market")],by='city')
citydata$GDP_2_3 <- (citydata$GDP_2 +citydata$GDP_3)

mod.lm_1 <- lm(D ~ log(GDP_per)+, data=citydata)
summary(mod.lm_1)
mod.lm_2 <- lm(Ds ~ log(GDP_per)+, data=citydata)
summary(mod.lm_2)

AIC(mod.lm)# AIC信息量

######################### weight matrix #########################
city_distance <- readRDS("C:/Users/WXY/Documents/R_learning/map/data/city_df.rds")
city_distance <- city_distance[,c("city.x","city.y","drive_time")]
city_distance$drive_time <- str_locate(city_distance$drive_time,'小')[,1]%>%
  str_sub(city_distance$drive_time,2,.)%>%str_remove(.,'小')%>%as.numeric(.)
city_distance <- city_distance[city_distance$city.x%in%dijishi_2004$NAME2004&
                               city_distance$city.y%in%dijishi_2004$NAME2004,]
names(city_distance)
city_distance_mat <- tidyr::spread(city_distance,key='city.y',value=3)
city_distance_mat <- reshape2::acast(city_distance,city.x~city.y,value.var="drive_time")

dijishi_2004_test <- dijishi_2004[dijishi_2004$NAME2004%in%citydata$city,]
plot(dijishi_2004_test)
names(dijishi_2004_test)[6] <- 'city'
dijishi_2004_test@data <- left_join(dijishi_2004_test@data,citydata,by='city') # D & Ds

W_cont_el <- poly2nb(dijishi_2004_test, queen=T)
W_cont_el <- poly2nb(dijishi_2004_test[-c(21,93,192,306307 311 ),], queen=T)

#再将邻接list转换为矩阵形式

#style参数为矩阵的标准化方式

W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)

## 对应变量y检验空间自相关性
moran.test(dijishi_2004_test$Ds, listw=W_cont_el_mat, zero.policy=T)

## 局部自相关性检验: Local Moran's I

lm1 <- localmoran(dijishi_2004_test$D, listw=W_cont_el_mat,zero.policy=T)

election$lm1 <- lm1[,4]## 提取 z-scores

########
## OLS模型
########
mod.lm <- lm(D ~ log(GDP_per)+幼儿抚养比+market, data=dijishi_2004_test)
summary(mod.lm)
AIC(mod.lm)# AIC信息量
mod.lm_2 <- lm(D ~ log(GDP_per)+幼儿抚养比+market+f_ratio, data=dijishi_2004_test)
summary(mod.lm_2)
AIC(mod.lm_2)
## 模型残差空间自相关性检验
moran.test(res, listw=W_cont_el_mat, zero.policy=T)

mod.sar <-lagsarlm(D ~ log(GDP_per)+f_ratio, data= dijishi_2004_test, listw=W_cont_el_mat,zero.policy=T, tol.solve=1e-12)
summary(mod.sar)
AIC(mod.sar)

res <- mod.sar$residuals
## 模型残差空间自相关性检验
moran.test(res,listw=W_cont_el_mat,zero.policy=T)

########### plot moran i ###########

dijishi_2004_test <- dijishi_2004[dijishi_2004$NAME2004%in%city_D$地区,]
plot(dijishi_2004_test)
names(dijishi_2004_test)[6] <- 'city'
dijishi_2004_test@data <- left_join(dijishi_2004_test@data, city_D, by = c("city" = "地区"))
nbq <- poly2nb(dijishi_2004_test, queen=T)
nbq_w <- nb2listw(nbq, zero.policy = T)
moran_d <- moran.mc(dijishi_2004_test$D, listw = nbq_w, nsim = 999)

range(dijishi_2004_test$D)
moran.plot(dijishi_2004_test$D, nbq_w,
           xlab = "D", ylab = "Lag of D")
