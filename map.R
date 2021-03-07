pacman::p_load(geosphere, maps, maptools, sf, sp, spdep)
pacman::p_load(ggplot2, dplyr, magrittr, showtext, RColorBrewer, shadowtext, ggrepel,igraph)

############################ 1. read data in ############################
# list layers
st_layers("C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", do_count = TRUE)
border <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "BOUL国界线")
city <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "地级市")
city_adm <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "BOUA地级行政区域")
city_location <- readRDS("C:/Users/WXY/Documents/data/DATA_R/city_location.rds")
# add font
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

city$name <- as.character(city$name)

########################### missing area in map ############################
city_location$city[!city_location$city%in%city$name]
city[city$name==" ", "地级行政区代码"] %>% plot()
## 西藏
city[str_sub(city$地级行政区代码, 1, 2)==65, c("name", "地级行政区代码")] #%>% plot()
# 山南市 
# 2016年2月确认撤销山南地区和乃东县，设立地级山南市
city[city$地级行政区代码==5422, 1] %>% plot()
city$name[city$地级行政区代码==5422] <- "山南市"
# 那曲市 
# 2017年设为地级市 行政代码改为: 540600
city[city$地级行政区代码==5424, 1] %>% plot()
city$name[city$地级行政区代码==5424] <- "那曲市"
# 阿里地区
city[city$地级行政区代码==5425, 1] %>% plot()
city$name[city$地级行政区代码==5425] <- "阿里地区"
# 香港
city[city$地级行政区代码==8100, 1] %>% plot()
city$name[city$地级行政区代码==8100] <- "香港特别行政区"
# 澳门
city[city$地级行政区代码==8200, 1] %>% plot()
city$name[city$地级行政区代码==8200] <- "澳门特别行政区"
# 台湾
city[city$地级行政区代码==7100, 1] %>% plot()
city$name[city$地级行政区代码==7100] <- "台湾岛"
# 
city[city$地级行政区代码==6590, 1] %>% plot()
city$Shape[city$地级行政区代码==6590]


############################ 1. adjcent matrix ############################

city_sp <- as(city, "Spatial")

nb_queen <- poly2nb(city_sp, queen = TRUE)
nbmat <- nb2mat(nb_queen, zero.policy=TRUE)
nbmat[nbmat!=0] <- 1
rownames(nbmat) <- as.character(city_sp$name)
colnames(nbmat) <- as.character(city_sp$name)
dim(nbmat)
nbmat <- nbmat[rownames(nbmat)!=" ", colnames(nbmat)!=" "] 
nbmat <- nbmat[order(rownames(nbmat)), order(colnames(nbmat))]
xlsx::write.xlsx(nbmat, "data/地级市距离矩阵_综合.xlsx", sheetName = "相邻矩阵(0-1)", append = TRUE)


# distance weight mean
nb_df <- reshape2::melt(nbmat)
nb_df <- nb_df[nb_df$Var1!=nb_df$Var2&nb_df$value!=0, ] # delete same citys
nb_df[, 1:2] <- apply(nb_df[, 1:2], 2, as.character)
nb_df <- left_join(nb_df, city_distance[,  c("city.x", "city.y", "drive_time2")],
                            by=c('Var2'='city.x', 'Var1'='city.y'))
nb_df$drive_time2.x[is.na(nb_df$drive_time2.x)] <- nb_df$drive_time2.y[is.na(nb_df$drive_time2.x)]

nb_time_ave <- nb_df %>% 
  select(Var1, Var2, drive_time2.x) %>%
  group_by(Var1) %>%
  summarise(ave_time = mean(drive_time2.x, na.rm = T))
mean(nb_time_ave$ave_time, na.rm = T)


############################ plot adjcent region ############################
nb_city <- nb_df[nb_df$Var1=="北京市"|nb_df$Var2=="北京市"&nb_df$drive_time2.x<4.3, c("Var1", "Var2")] %>% 
  unlist() %>%
  unique()

par(mar = seq(0.1, 4))
showtext_auto()
ggplot() + 
  geom_sf(data = city[1:20, "name"], 
          colour = "grey15", fill = "grey85", size = 0.3, alpha = 1, linetype = "solid") +
  geom_sf(data = city[city$name%in%nb_city[-6], "name"],
          colour = "grey25", fill = "#9ECAE1", size = 0.1, alpha = 1, linetype = "solid") +
  geom_sf_label(data = city[city$name%in%nb_city[-6], "name"], # add labels
                  aes(label = name),
                  color="grey15", size= 3.5) +
  theme_minimal()

ggsave("export/adj_sample2.pdf")

# reference
# https://github.com/r-spatial/sf/issues/234
