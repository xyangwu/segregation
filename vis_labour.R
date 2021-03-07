# load in packages
pacman::p_load("rgdal", "rgeos", "maptools", "dplyr", "stringr",
               "ggplot2", "RColorBrewer", "viridis", "showtext", "sf")

# add font 
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

# read map ----------------------------------------------------------------------------

# list map layers
st_layers("C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", do_count = TRUE)

# load in city and national border
# province <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "BOUA省级行政区域")
# city <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "地级市")
nation <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "BOUL国界线")

# st_layers("C:/Users/WXY/Documents/R_learning/spatial/data/国家1-400", do_count = TRUE)
# nation2 <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/国家1-400", layer = "国界线")
# province2 <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/国家1-400", layer = "省级行政区")
# city2 <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/国家1-400", layer = "中国地州界")

# city2 = left_join(city2, adm_cn[, c("county", "city")], by = c("NAME" = "county"))
# city2$NAME = gsub("市辖区\\(|\\)", "", city2$NAME)
# city2 = left_join(city2, unique(city_name00[, c("name_cn15", "name_harmonized")]), by = c("NAME" = "name_cn15"))

# city2$name_harmonized[city2$NAME == "济源市"] = "焦作市, 济源市"
# city2$name_harmonized[city2$NAME %in% c("天门市", "潜江市", "仙桃市", "随州市")] = "湖北省直辖市(天门,潜江,仙桃,随州)"
# city2$name_harmonized[city2$NAME %in% c("恩施市")] = "恩施土家族苗族自治州"
# city2$name_harmonized[city2$NAME %in% c("林芝县")] = "林芝市"
# city2$name_harmonized[city2$NAME %in% c("株州市")] = "株洲市"
# city2$name_harmonized[city2$NAME %in% c("阿勒泰市")] = "阿勒泰地区"
# city2$name_harmonized[city2$NAME %in% c("思茅市")] = "普洱市"
# city2$name_harmonized[city2$NAME %in% c("楚雄市")] = "楚雄彝族自治州"
# city2$name_harmonized[city2$NAME %in% c("揭阳县")] = "揭阳市"
# city2$name_harmonized[city2$NAME %in% c("和田市")] = "和田地区"
# city2$name_harmonized[city2$NAME %in% c("怒江傈傈族自治州")] = "怒江傈僳族自治州"
# city2$name_harmonized[city2$NAME %in% c("固原县")] = "吴忠市, 固原市, 中卫市"
# city2$name_harmonized[city2$NAME %in% c("淮阴市")] = "淮安市, 宿迁市"
# city2$name_harmonized[city2$NAME %in% c("莱芜市")] = "泰安市, 莱芜市"

#city2 = left_join(city2, unique(city_name00[, c("name_harmonized", "province", "region.x", "region.y")]), by = c("name_harmonized" = "name_harmonized"))

# city2[city2$province == "西藏", ]
# 
# unmatched = which(is.na(city2$name_harmonized)) %>% city2$NAME[.]
# which(lfp_10$city %in% unmatched) %>% lfp_10$city[.]
# which(lfp_10$city %in% c("天门市"))
# unmatched[7]
# 
# city2_sp = as(city2, "Spatial")
# 
# city2_sp <- rgeos::gUnaryUnion(spgeom = city2_sp, id = city2_sp$name_harmonized)
# plot(city2_sp) 

# aggregate area according to harmonized administration name
#city2_hmnz = city2 %>%
#  group_by(name_harmonized) %>% 
#  summarise(name_harmonized = unique(name_harmonized), do_union = TRUE)

national_ae = st_transform(nation,
                           crs = "+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")

plot(st_geometry(national_ae))

# Refrence
# https://stackoverflow.com/questions/13757771/relocating-alaska-and-hawaii-on-thematic-map-of-the-usa-with-ggplot2
# https://revolutionarygis.wordpress.com/2017/04/11/china-shapefile-data/

# read map data in 2004 ------------------------------------------------------------

city_name00 = read.xlsx("data/cityname_00.xlsx", sheet = 1)

# 2004 shapefile
dijishi_2004 <- readOGR("C:/Users/WXY/Documents/R_learning/spatial/data/xingzhengquhua_shp/dijishi_2004.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)

dijishi_2004@data$AD2004 = substr(dijishi_2004@data$AD2004, 1, 4)
# adm_cn$countycode = as.character(adm_cn$countycode)
# dijishi_2004@data = left_join(dijishi_2004@data, unique(adm_cn[, c("citycode", "city")]), by = c("AD2004"="citycode"))

dijishi_2004@data = dijishi_2004@data %>%
  mutate(NAME2004 = case_when(
    is.na(NAME2004) ~ "无名",
    grepl('北京市', NAME2004) ~ "北京市",
    grepl('上海市', NAME2004) ~ "上海市",
    grepl('天津市', NAME2004) ~ "天津市",
    grepl('重庆市', NAME2004) ~ "重庆市",
    grepl('思茅市', NAME2004) ~ "普洱市",
    grepl("来宾市", NAME2004) ~ "柳州市",
    grepl("中卫市", NAME2004) ~ "固原市",
    grepl("崇左市", NAME2004) ~ "南宁市",
    grepl("荷泽市", NAME2004) ~ "菏泽市",
    substr(AD2004, 1, 2) == 42 & grepl("省直辖|随州市", NAME2004) ~ "湖北省直辖县级行政区划",
    substr(AD2004, 1, 2) == 65 & grepl("省直辖", NAME2004) ~ "新疆自治区直辖县级行政区划",
    substr(AD2004, 1, 2) == 46 & grepl("省直辖", NAME2004) ~ "海南省直辖县级行政区划",
    substr(AD2004, 1, 2) == 50  ~ "重庆市",
    TRUE ~ as.character(NAME2004)
  ))

dijishi_2004@data = left_join(dijishi_2004@data, unique(city_name00[, c("name_cn10", "name_harmonized")]), by = c("NAME2004"="name_cn10"), match)
# View(dijishi_2004@data)
which(is.na(dijishi_2004@data$name_harmonized)) %>% dijishi_2004@data[.,]
dijishi_2004@data$name_harmonized[which(is.na(dijishi_2004@data$name_harmonized))] = dijishi_2004@data$NAME2004[which(is.na(dijishi_2004@data$name_harmonized))]

# dissolve citys into harmonized geo-units
mapdata <- gUnaryUnion(spgeom = dijishi_2004, id = dijishi_2004$name_harmonized)
mapdata <- SpatialPolygonsDataFrame(Sr = mapdata, data = data.frame(id = row.names(mapdata), row.names = row.names(mapdata), name_harmonized = row.names(mapdata)))
names(mapdata)

# transform to sf
mapdata = st_as_sf(mapdata)

equalarea = CRS("+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs")
# equalarea2 = CRS("+proj=tmerc +lat_0=0 +lon_0=108 +k=1 +x_0=36500000 +y_0=0 +ellps=krass +towgs84=15.8,-154.4,-82.3,0,0,0,0 +units=m +no_defs")
mapdata = st_transform(mapdata, crs = equalarea)
plot(mapdata$geometry)

# dissolve citys into province
dijishi_2004$provincecode <- substr(dijishi_2004$AD2004, 1, 2)
province04 <- rgeos::gUnaryUnion(spgeom = dijishi_2004, id = dijishi_2004$provincecode)

# nation border
nation_border = st_transform(nation, crs = equalarea)

# south sea --------------------------------------------------------------------

mapdata = left_join(mapdata, unique(city_name00[, c("name_harmonized", "province", "region.x", "region.y")]))

plot(mapdata$geometry[mapdata$province == "海南"])
st_bbox(mapdata$geometry[mapdata$province == "海南"])

box = c(xmin = 60000, xmax = 5000000, ymin = -3400000, ymax = -800000)

southsea = st_crop(mapdata, box)
southsea = as(southsea, "Spatial") 

southline = st_crop(nation_border, box)
southline = as(southline, "Spatial") 

southsea = st_as_sf(southsea)
southline = st_as_sf(southline)

# another solution
# fix <- function(object,params){
#   r=params[1];scale=params[2];shift=params[3:4]
#   object = elide(object,rotate=r)
#   size = max(apply(bbox(object),1,diff))/scale
#   object = elide(object,scale=size)
#   object = elide(object,shift=shift)
#   object
# }
# 
# southsea2 = fix(southsea, c(0, 1.7, 125, 17)) 
# proj4string(southsea2) = proj4string(southsea)
# southline2 = fix(southline, c(0, 1.7, 125, 17)) 
# proj4string(southline2) = proj4string(southline) 

# plot data --------------------------------------------------------------------

showtext_auto() # show Chinese characters when plotting
display.brewer.pal("RdYlGn", n = 11) # RdYlBu

# plot lfpr
mapdata = left_join(mapdata, unique(lfpr_tfr[, c("name_harmonized", "province", "region.x", "region.y")]))

main <- ggplot() + 
  geom_sf(data = mapdata, aes(fill = tfr),
          colour = "white", size = 0.05, alpha = 0.7, linetype = "solid") +
  geom_sf(data = st_as_sf(province04), 
          colour = "grey60", fill = NA, size = 0.3, alpha = 0.7, linetype = "solid") +
  geom_sf(data = nation_border,
          colour = "grey50", fill = NA, size = 0.5) +
  coord_sf(ylim = c(-1900000, 2174178), xlim = c(-2641806, 3000000), crs = st_crs(mapdata)) +
  scale_fill_distiller(palette = "RdYlGn", name = "Female Labour Force Participate Rate (%)",
                       guide = guide_colourbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
main

# natural breaks
# library(classInt)
# classes <- classIntervals(mapdata$lfpr, n = 5, style = "jenks")
# classes$brks
# mapdata <- mapdata %>%
#   mutate(percent_class = cut(lfpr, classes$brks, include.lowest = T))

south = ggplot() + 
  geom_sf(data = southsea, 
          colour = "grey40", fill = "grey70", size = 0.05, alpha = 0.8, linetype = "solid") +
  geom_sf(data = southline, 
          colour = "grey30", size = 0.3, alpha = 1, linetype = "solid") +
  coord_sf(datum = NA, ylim = c(-3400000, -800000), expand = FALSE) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "grey30", fill = NA),
        plot.background = element_blank())
south

tfr_map = main +
  annotation_custom(
    grob = ggplotGrob(south),
    xmin = 1200000,
    xmax = 1200000 + (box[2] - box[1])/2,
    ymin = -2100000,
    ymax = -2100000 + (box[4] - box[3])/2
  )

ggsave(tfr_map, filename = "fig/tfr_map.pdf", width = 8, height = 8, dpi = 300)

# plot D ------------------------------------------------------------------------

names(mapdata); dim(mapdata)
mapdata = mapdata[, -c(6)]
# a10_class2 
mapdata = left_join(mapdata, datamodel10, by = c("name_harmonized" = "unit")) 
names(mapdata) 

mapdata[mapdata$ind_prof > 0.1, 1:3]
mapdata$ind_prof[mapdata$province %in% c("北京")]
mapdata$ind_prof[mapdata$province %in% c("山东", "重庆")] = NA

main <- ggplot() + 
  geom_sf(data = mapdata, aes(fill = a), 
          colour = "white", size = 0.05, alpha = 0.7, linetype = "solid") + 
  # aes(fill = d10,text = paste(name_harmonized, "\n",
  #              "D: ", round(d10, 3), "\n",
  #              "Ds: ", round(ds10, 3), "\n",
  #              sep = "")), 
  geom_sf(data = st_as_sf(province04), 
          colour = "grey60", fill = NA, size = 0.3, alpha = 0.7, linetype = "solid") +
  geom_sf(data = nation_border,
          colour = "grey50", fill = NA, size = 0.5) + 
  coord_sf(ylim = c(-1900000, 2174178), xlim = c(-2641806, 3000000), crs = st_crs(mapdata)) +
  scale_fill_distiller(palette = "RdYlGn", name = "城市化",
                       guide = guide_colourbar(direction = "horizontal",
                                               barheight = unit(2, units = "mm"),
                                               barwidth = unit(50, units = "mm"),
                                               draw.ulim = F,
                                               title.position = 'top',
                                               title.hjust = 0.5,
                                               label.hjust = 0.5)) + 
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) 
main

# p = ggplotly(main, tooltip = "text")
# htmlwidgets::saveWidget(as_widget(p), "C:/Users/WXY/Documents/R_learning/segregation/fig/d_map10_plotly.html")

# natural breaks
# library(classInt)
# classes <- classIntervals(mapdata$lfpr, n = 5, style = "jenks")
# classes$brks
# mapdata <- mapdata %>%
#   mutate(percent_class = cut(lfpr, classes$brks, include.lowest = T))

south = ggplot() + 
  geom_sf(data = southsea, 
          colour = "grey40", fill = "grey70", size = 0.05, alpha = 0.8, linetype = "solid") +
  geom_sf(data = southline, 
          colour = "grey30", size = 0.3, alpha = 1, linetype = "solid") +
  coord_sf(datum = NA, ylim = c(-3400000, -800000), expand = FALSE) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "grey30", fill = NA),
        plot.background = element_blank())

ds_map = main +
  annotation_custom(
    grob = ggplotGrob(south),
    xmin = 1200000,
    xmax = 1200000 + (box[2] - box[1])/2,
    ymin = -2100000,
    ymax = -2100000 + (box[4] - box[3])/2
  )

ggsave(ds_map, filename = "fig/map10_class2_ds.pdf", width = 8, height = 8, dpi = 300)

