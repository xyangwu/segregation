# floating population in beijing&shanghai 2017 -----------------------------------
bj_sh <- openxlsx::read.xlsx('data/北京上海2017外来人口.xlsx', sheet = 1, startRow = 2)
bj_sh$城市 <- zoo::na.locf(bj_sh$城市)
bj_sh <- bj_sh[-which(bj_sh$城市=='合计'),]
range(bj_sh$d指数)

bj_sh$行政区划 <- gsub('区|新区', '', bj_sh$行政区划) %>% trimws(.)
bj_sh$行政区划[19] <- "徐汇"

# read in polygon data -----------------------------------------------------------
pacman::p_load("rgdal", "rgeos", "maptools", "dplyr")

sh_street <- readOGR('data/Geopandas/上海街镇/上海街镇.shp', use_iconv = T,encoding = 'utf-8',stringsAsFactors = F)
sh_district <- rgeos::gUnaryUnion(spgeom = sh_street, id = sh_street$区县)

cn_district <- readOGR('data/share01/县市界.shp', use_iconv = T,encoding = 'utf-8',stringsAsFactors = F)
names(cn_district)
head(cn_district@data)
bj_district <- cn_district[which(cn_district$省 == "北京市"), ]
pdf("export/sh_s1.pdf")
plot(sh_street)
dev.off()

# join segregation index with polygon data----------------------------------------

pacman::p_load("ggplot2", "ggmap", "RColorBrewer")

sh_district_df <- fortify(sh_district)
sh_district$id <- row.names(sh_district) # allocate an id variable to the sp data
sh_district_df <- left_join(sh_district_df, sh_district@data) # join the data
sh_district_df <- left_join(sh_district_df, bj_sh[which(bj_sh$城市=="上海"), 1:10], by = c('id' = '行政区划'))
# test
which(is.na(sh_district_df$外来人口[which(sh_district_df$id=="闵行")]))

bj_district <- spTransform(bj_district, CRS(proj4string(sh_district)))
bj_district_df <- fortify(bj_district)
bj_district$id <- row.names(bj_district) # allocate an id variable to the sp data
bj_district_df <- left_join(bj_district_df, bj_district@data) # join the data
bj_district_df$district <- sub('区|县', '', bj_district_df$district)
bj_district_df <- left_join(bj_district_df, bj_sh[which(bj_sh$城市=="北京"), 1:10], by = c('district' = '行政区划'))
# test
which(is.na(bj_district_df$外来人口[which(bj_district_df$district=="昌平")]))

# district center for adding label later
for (i in which(bj_sh$城市=='北京')) {
  
  bj_sh[i, c('long', 'lat')] <- coordinates (rgeos::gCentroid (bj_district[grep(bj_sh$行政区划[i],bj_district$district),]))
  
}

for (i in which(bj_sh$城市=='上海')) {
  
  bj_sh[i, c('long', 'lat')] <- coordinates (rgeos::gCentroid (sh_district[grep(bj_sh$行政区划[i],sh_district$id),]))

}

# set plot theme ---------------------------
# add font family
library(showtext)
windowsFonts(
  hanyi = windowsFont("HanYiChangSongJian-1.ttf"),
  courier = windowsFont("cour.ttf"))
#font <- font_files()
font_add(family = 'hanyi', regular = 'HanYiChangSongJian-1.ttf')
font_add('courier', 'cour.ttf', 'courbd.ttf')
font_families()
showtext_auto()

# shanghai  ------------------------------------
sh_map1 <- ggplot() + #ggmap(bg) +
  geom_polygon(data = sh_district_df, 
               aes(x = long, y = lat, group = group, fill = d指数),
               alpha = 0.5) +
  
  geom_path(data = sh_district_df, aes(x = long, y = lat, group = group), 
            colour='white', alpha=0.8, lwd= 0.8) +
  
  #scale_fill_gradient(high = '#D53E4F' , low = '#FFFFBF', name="d指数") +

  geom_point(data = bj_sh[which(bj_sh$城市=='上海'),], aes(x = long, y = lat, size = X9),
             color = "lightcoral", alpha = 0.8) +
  
  labs(title = "上海外来人口隔离指数",
       subtitle = "N = 17",
       caption = "@data by Luo") +
  theme_light() +
  geom_text_repel(data= bj_sh[which(bj_sh$城市=='上海'),], aes(x = long ,y = lat, label = 行政区划), hjust = 1)

sh_map1

# beijing ------------------------------------
bj_map1 <- ggplot() + #ggmap(bg) +
  geom_polygon(data = bj_district_df, 
               aes(x = long, y = lat, group = group, fill = d指数),
               alpha = 0.5) +
  
  geom_path(data = bj_district_df, aes(x = long, y = lat, group = group), 
            colour='white', alpha=0.8, lwd= 0.8) +
  geom_point(data = bj_sh[which(bj_sh$城市=='北京'),], aes(x = long, y = lat, size = X9),
             color = "lightcoral", alpha = 0.8) +
  labs(title = "北京外来人口隔离指数",
       subtitle = "N = 16",
       caption = "@data by Luo") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text_repel(data= bj_sh[which(bj_sh$城市=='北京'),], aes(x = long ,y = lat, label = 行政区划), hjust = 1)
  
bj_map1

ggsave('export/sh_map1.pdf', sh_map1, width = 7, height = 7)
ggsave('export/bj_map1.pdf', bj_map1, width = 7, height = 7)
