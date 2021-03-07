pacman::p_load("rgdal", "rgeos", "maptools", "dplyr")

setwd('C:/Users/WXY/Documents/R_learning/map/')

# load in data  ###########################
# shanghai population makeup
sh_town_pop <- readxl::read_xls("data/09.xls")
sh_town_pop$nohukou <- (sh_town_pop$`合　计` - sh_town_pop$本地户口)
sh_town_pop$nohukourate <- sh_town_pop$nohukou/sh_town_pop$`合　计`

# polygon at town level
sh_town_shp <- readOGR("data/Geopandas/上海街镇/上海街镇.shp",use_iconv = T,encoding = 'utf-8',stringsAsFactors = F)

# check data
names(sh_town_shp);head(sh_town_shp$NAME, n=10)
unique(sh_town_shp$名称);sum(grep("镇|乡|街道",sh_town_pop$地区))
# new town "南汇新城镇"
which(!sh_town_shp$名称%in%sh_town_pop$地区) %>%sh_town_shp$名称[.]
# "打浦桥街道" "南汇新城镇" 撤销, 2012年4月，原临港新城更名为南汇新城
# 撤销申港街道、芦潮港镇建制，调整老港镇部分行政区划，成立南汇新城镇 
# 下辖申港社区、芦潮港社区和临港综合社区三个社区

plot(sh_town_shp2) # test


# transform shp to dataframe  ###########################
# and left join population data
#sh_town_shp <- spTransform(sh_town_shp, CRS(proj4string(sh_town_shp)))
library(ggplot2)
sh_town_df <- fortify(sh_town_shp) # convert to data.frame (also broom::tidy(sh_town))
head(sh_town_df, n = 2) # peak at the fortified data
sh_town_shp$id <- row.names(sh_town_shp) # allocate an id variable to the sp data
head(sh_town_shp@data, n = 2) # final check before join (requires shared variable name)
sh_town_df <- left_join(sh_town_df, sh_town_shp@data) # join the data
sh_town_df <- left_join(sh_town_df, sh_town_pop[,names(sh_town_pop)], by =c('名称'='地区')) # join the data from sh_town_shp

# district layer -------------------------------------------------------
sh_district = rgeos::gUnaryUnion(spgeom = sh_town_shp, id = sh_town_shp$区县)
sh_district_df <- ggplot2::fortify(sh_district, stringsAsfactors = FALSE)
sh_district_df <- left_join(sh_district_df, sh_town_shp@data[,c("区县","id")])
head(sh_district_df, n=2)

# plot map -------------------------------------------------------------
library(ggplot2)
library(RColorBrewer)
library(ggmap)

# view colour
RColorBrewer::display.brewer.all()
display.brewer.pal(11, 'Spectral')
brewer.pal(11, 'Spectral')[c(1:6, 10)]
rev(brewer.pal(11, 'Spectral')[c(1:6, 10)])

#sh.b1 <- readRDS('R_learning/segregation/data/sh_b1.rds')
#sh.b4 <- get_stamenmap(bbox = c(120.8523+0.1, 30.68147, 121.9740+0.1, 31.86613), zoom = 11, maptype = "watercolor")
#bb <- attr(sh.b4, "bb")
#bb_df  <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1, 3)]))

sh_town_map <- ggplot() +
  geom_polygon(data = sh_town_df, 
               aes(x = long, y = lat, group = group, fill = nohukou),
               alpha = 0.7) +
  
  geom_path(data = sh_town_df, aes(x = long, y = lat, group = group), 
            colour='deepskyblue4', alpha=0.7, lwd= 0.2) +
  
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')[c(1:6, 10)]))(212), name="非本地户籍人口（人）") +
  
  geom_path(data = sh_district_df, aes(x = long, y = lat, group = group),
            colour = 'black', alpha = 0.7, lwd = 0.7) +
  theme_void()+
  
  labs(title = "上海非本地户籍人口分布",
       subtitle = "212个街道、乡镇|总人口：230191965人|非本地户籍人口：12877144人(44%)",
       caption = "Datasource:第六次人口普查 vis with {ggmap}")

# save map to .pdf filetype
ggsave(paste0('export/sh_town_map.pdf'), sh_town_map, width = 7, height = 7)

############ compute indices ################
# too slow!
library("seg")
# dataframe to polygons
sp <- lapply(split(mydf[, -1], mydf[, 1]), Polygon)
sp <- SpatialPolygons(lapply(seq_along(sp), function(i) { 
  Polygons(list(sp[[i]]), ID = row.names(mydf[!duplicated(mydf[, 1]), ])[i] )
  })
)
# or join data to polygon
sh_town_shp@data <- left_join(sh_town_shp@data,sh_town_pop,by=c('名称'='地区'))

data = sh_town_shp@data[!is.na(sh_town_shp@data$本地户口), c("本地户口", "nohukou")]
dissim(data=data)$d

sp.seg <- spseg(sh_town_shp@polygons[!is.na(sh_town_shp@data$本地户口),], data, smoothing = "kernel", sigma = 0.7)

dis.kern <- round(as.list(sp.seg)$d, 3)       #dissimilarity 
inf.kern <- round(as.list(sp.seg)$h, 3)       #information theory 
