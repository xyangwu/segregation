
########################## visualization segregation ########################### 
pacman::p_load("rgdal", "rgeos", "maptools")
library(tidyverse)
pacman::p_load(ggplot2, dplyr, magrittr, showtext, RColorBrewer, shadowtext, ggrepel)


########################## 2. read in data ########################### 

city_name00 = read.xlsx("data/cityname_00.xlsx", sheet = 1)

# 2004 shapefile
dijishi_2004 <- readOGR("C:/Users/WXY/Documents/R_learning/spatial/data/xingzhengquhua_shp/dijishi_2004.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)

proj4string(dijishi_2004)
#dijishi_2004 <- spTransform(dijishi_2004, CRSobj = CRS("+proj=longlat +ellps=WGS84"))
dijishi_2004 <- spTransform(dijishi_2004, 
                            CRSobj = CRS("+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84
                           +datum=WGS84 +units=km +no_defs"))

dijishi_2004$AD2004 = substr(dijishi_2004$AD2004, 1, 4)
adm_cn$countycode = as.character(adm_cn$countycode)
dijishi_2004@data = left_join(dijishi_2004@data, adm_cn[, c("citycode", "city")], by = c("AD2004"="citycode"))

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

dijishi_2004@data = left_join(dijishi_2004@data, city_name00[, c("name_cn10", "name_harmonized")], by = c("NAME2004"="name_cn10"))
View(dijishi_2004@data)
which(is.na(dijishi_2004@data$name_harmonized)) %>% dijishi_2004@data[.,]

dijishi_2004$provincecode <- stringr::str_sub(dijishi_2004$AD2004,1,2)
province_04 <- rgeos::gUnaryUnion(spgeom = dijishi_2004, id = dijishi_2004$provincecode)
plot(province_04)

plot(dijishi_2004[dijishi_2004$NAME2004 == "上海市", ]) 

province_04 <- SpatialPolygonsDataFrame(Sr = province_04, 
                                        data = data.frame(id=row.names(province_04),
                                                          row.names = row.names(province_04)))

province_04_df <- ggplot2::fortify(province_04, stringsAsfactors = FALSE)
#province_04_df <- left_join(province_04_df, province_04@data[,c("id")])
head(sh_district_df, n=2)

dijishi_2004@data$id <- rownames(dijishi_2004@data)
dijishi_df <- fortify(dijishi_2004) 
dijishi_df <- full_join(dijishi_df, dijishi_2004@data, by = "id")
dijishi_df$class <- rep("Mainland",nrow(dijishi_df))
names(dijishi_df)[13] <- 'city'

# join D & Ds
dijishi_df <- left_join(dijishi_df, city_D_Ds, by='city') 
sum(!is.na(unique(dijishi_df$D))) # check

sexratio <- openxlsx::read.xlsx('data/按年龄分类.xlsx',sheet=1)
sexratio <- sexratio[sexratio$city%in%city_D_Ds$city,]

dijishi_df <- left_join(dijishi_df, sexratio, by='city') # GDP

dijishi_df <- left_join(dijishi_df, city_ocprate[,c(1,4)], by='city') # female workers

dijishi_df <- left_join(dijishi_df, sexratio[,c(1,4)], by='city') # female workers

names(dijishi_df)

#--------------------- Nanhai Region -----------------------------------
Width<-9;Height<-9
long_Start<-124;lat_Start<-16

Nanhai_df<-dijishi_df[dijishi_df$long>106.55 & dijishi_df$long<123.58,]
Nanhai_df<-Nanhai_df[Nanhai_df$lat>4.61 & Nanhai_df$lat<25.45,]

min_long<-min(Nanhai_df$long, na.rm = TRUE)
min_lat<-min(Nanhai_df$lat, na.rm = TRUE)
max_long<-max(Nanhai_df$long, na.rm = TRUE)
max_lat<-max(Nanhai_df$lat, na.rm = TRUE)

Nanhai_df$long<-(Nanhai_df$long-min_long)/(max_long-min_long)*Width+long_Start
Nanhai_df$lat<-(Nanhai_df$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start
Nanhai_df$class<-rep("NanHai",nrow(Nanhai_df))

dijishi_df<-rbind(dijishi_df,Nanhai_df)

#----------------------------- Nanhai 9 Line -----------------------------
Nanhai_dfLine <- read.csv("C:/Users/WXY/Documents/R_learning/map/data/Original_Data-master/中国南海九段线.csv")  
Encoding(names(Nanhai_dfLine)) <- 'UTF-8'
colnames(Nanhai_dfLine)<-c("long","lat","ID")

Nanhai_dfLine$long<-(Nanhai_dfLine$long-min_long)/(max_long-min_long)*Width+long_Start
Nanhai_dfLine$lat<-(Nanhai_dfLine$lat-min_lat)/(max_lat-min_lat)*Height+lat_Start

#---------------------------  centroids --------------------------- 
centroids.df <- data.frame(long=coordinates(dijishi_2004)[,1],
                           lat=coordinates(dijishi_2004)[,2],stringsAsFactors = F)
centroids.df$id <- dijishi_2004@data$id
centroids.df$NAME2004 <- dijishi_2004@data$NAME2004

########################## 2.2 2000 shapefile ########################### 

cn_2000 <- readOGR("C:/Users/WXY/Documents/R_learning/segregation/data/ipumsi_00001/geo2_cn2000/geo2_cn2000.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)
cn_gadm <- readOGR("C:/Users/WXY/Documents/R_learning/spatial/data/gadm36_CHN_shp/gadm36_CHN_2.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)
head(cn_gadm@data)

city_name = lapply(1:nrow(cn_2000), function(x){
  city = gsub(" city| municipality", "", cn_2000@data[, c("ADMIN_NAME")][x])
  i = grep(city, cn_gadm@data[, c("NAME_2")])
  return(paste0(cn_gadm@data[, c("NL_NAME_2")][i]))
  } )

xlsx::write.xlsx(city_name, "city_name.xlsx")
city_name = xlsx::read.xlsx("city_name.xlsx", sheetIndex = 1, encoding = "UTF-8")
plot(cn_2000)


########################## 3. visualization ###########################

#################### 1990, 2000 & 2010 ##########################
showtext_auto()
Ds_year <- ggplot()+
  geom_histogram(data = Ds_city10[Ds_city10$city != "重庆市", ],
                 aes(Ds), 
                 fill = 'coral',
                 color = 'firebrick3',
                 bins = 40,
                 alpha = .2) +
  geom_histogram(data = Ds_city00,
                 aes(Ds), 
                 fill = 'orange2',
                 color = 'orange',
                 bins = 40,
                 alpha = .2) +
  geom_histogram(data = Ds_city90,
                 aes(Ds),
                 fill = '#C6DBEF',
                 color = 'steelblue',
                 bins = 40,
                 alpha = .2) +
  theme_classic() +
  ylab("频次") +
  xlab("指数值") +
  ggtitle("1990年、2000年与2010年的Ds指数分布")

ggsave("export/Ds_year.pdf", width = 7, height = 6)

#################### gdp per capita ##########################

cor.test(log(Ds_city10$GDP_percap[Ds_city10$city != "重庆市"]), Ds_city10$Ds[Ds_city10$city != "重庆市"])

ggplot(Ds_city10 %>%
         filter(city != "重庆市"),
       aes(log(GDP_percap), Ds))+
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm", se = F, color = "#C6DBEF") +
  #geom_text_repel(aes(label=city))+
  geom_text(aes(x = max(log(Ds_city10$GDP_percap), na.rm = T)*4/5, y = max(Ds_city10$Ds, na.rm = T)*4/5,
                label = paste0('estimated Pearson correlation: \n', 0.23, "~", 0.46, "(CI: 0.95)")), color = "grey50")

plot(log(Ds_city10$GDP_percap[Ds_city10$city != "重庆市"]), Ds_city10$Ds[Ds_city10$city != "重庆市"]) 

#--------------------------- 3.1 map D -------------------------- 

library(ggplot2);library(ggrepel);library(RColorBrewer);library(showtext)

showtext_auto()
Ds_map <- ggplot()+
  geom_polygon(data=dijishi_df, aes(x=long, y=lat, group=interaction(class,group),
                                    fill=D),colour=alpha('white',alpha=0.6),size=0.05)+

  geom_rect(aes(xmin = long_Start, xmax=long_Start+Width+0.3, ymin=lat_Start-0.3, ymax=lat_Start+Height),
            fill=NA, colour="black",size=0.15)+
  geom_line(data=Nanhai_dfLine, aes(x=long, y=lat, group=ID),
            colour="black", size=0.6)+
  scale_fill_distiller(palette="Spectral")+ # PuRd Spectral PiYG
  # scale_fill_manual(name = "D",
   #                   values = c("(-Inf,0.1]" = "#5E4FA2","(0.1,0.2]" = "#88CFA4",
    #                             "(0.2, 0.3]" = "#FFFFBF","(0.3,0.4]" = "#F88D52",
     #                            "(0.4, 0.5]" = "#9E0142"),
      #                labels = c("<= 0.1", "0.1 < D <= 0.2", "0.2 < D <= 0.3",
       #                          "0.3 < D <= 0.4","0.4 < D <= 0.5"))+
  # geom_text(data = centroids.df, aes(label=NAME2004, x = long, y = lat, group = NAME2004),
  #          size = 0.8,alpha=0.7) + 
  theme_bw() +
  coord_map()+
  ylim(15,55)+
  theme(legend.position=c(0.15,0.2),
        legend.background = element_blank(),panel.border = element_blank(),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),axis.text.y = element_blank()
        )+
  labs(title = "",x="", y="",
     caption = "")

# save map to .pdf filetype
ggsave(paste0('export/f_m.pdf'), width = 7, height = 7)

#--------------------------- 3.2 plot D*D_detail at city level ------------------------- 
city_dissim$D_detail <- city_dissim_d$D

cor.test(city_dissim$D, city_dissim$D_detail)
city_dissim$color = ifelse(city_dissim$D < city_dissim$D_detail, "#4A8FC2", "firebrick3")

city_plot <- city_dissim %>% ggplot(aes(D, D_detail)) +
  geom_point(color = city_dissim$color,shape = 21, size = 4, stroke = 1.6,alpha=0.8) +
  geom_text(x=0.3, y=0.2,label='estimated correlation= 0.7722') +
  geom_abline(aes(intercept=0, slope=1),linetype="dotted")+
  #geom_text_repel(aes(label=地区),segment.size = 0.6)+
  ylim(0,0.4)+
  xlim(0,0.4)+
  theme_classic() +
  coord_equal()+
  ylab("细分职业后的D指数") +
  xlab("职业中类分的D指数") +
  ggtitle("地市一级D指数，D指数变大了")
# 种类变多后D指数也变大了，影响隔离指数的可能只是因为，那些拥有较高D指数的市有更多工作种类
# 相对较高的隔离程度说明了在这些新种类的工作里，依然保持了过去存在的男女性别比，从而相加后
# 也就变大了，而不能说明这些地区现实中的隔离程度。

#------------------------- 3.3 plot D at prov level ------------------------- 

plot2 <- prov_dissim %>% ggplot()+
  theme_classic()+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size=rel(1.2),face="bold", lineheight=.5 ),
        plot.subtitle=element_text(size=12,face="bold"),
        plot.caption = element_text(size = rel(1)),
        axis.text.y = element_text(colour = '#08306B')
        #plot.margin = unit(c(1,0,0,0), "cm"),
  )+
  geom_point(aes(x = 0.4, y = reorder(地区, D_detail)), size = 0, col = "white") +
  geom_hline(yintercept=1:31, colour='grey80')+
  expand_limits(y = 33) +
  
  geom_point(aes(x = D, y = 地区),colour = "#4388BE", size = 5, alpha = 0.7) +
  geom_point(aes(x = D_detail, y = 地区), colour = "#C6DBEF", size = 5)+
  # darkmagenta
  geom_text(aes(x = x, y = y, label = label),
            data=data.frame(x=c(0.18615401,0.356), y = c(32,26),
                            label = c("按7个职业中类分", "64个细类(包含了“不便\n分类的其他从业人员”)")),
            color = c("#4388BE","#C6DBEF"), size = 4) +
  
  labs(title = "职业种类划分得更细后，D指数变大了",
       subtitle = 'Analysis Unit: 31个省市区',
       x='D指数',
       caption = "Source: 中国第六次人口普查" )
ggsave( "export/city_plot.pdf", city_plot, width = 7, height = 8)

#------------------------- 3.4 correlation between sex ratio*D ------------------------- 

cor.test(prov_dissim$ratio.f.m., prov_dissim$dissim)

ggplot(prov_dissim, aes(ratio.f.m., dissim))+
  geom_point(fill = "firebrick3", aes(size = dissim))+
  theme_classic()+
  #scale_x_continuous(breaks=c(0.6,0.7,0.8,0.9),limits=c(0.6,0.7,0.8,0.9)) +
  geom_smooth(method = "lm", se = F, color = "darkmagenta") +
  geom_text_repel(aes(label=prov))+
  geom_text(aes(x=0.7, y=0.21,label='estimated correlation= -0.5586'))

# plot dissim
library(ggplot2);library(ggrepel)

ggplot(city_dissim,aes(reorder(city, dissim), dissim))+
  geom_bar(stat="identity", fill = "#C6DBEF")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour = '#08306B'))+
  geom_text(aes(label = round(dissim,4)),
            colour = rep("grey",170), angle = 90, hjust = 1.1)

# correlation between sex ratio and dissimilarity

cor_city <- cor.test(city_dissim$ratio.f.m., city_dissim$dissim)$conf.int

ggplot(city_dissim,aes(ratio.f.m., dissim))+
  geom_point(fill = "firebrick3", aes(size = dissim,alpha=dissim))+
  theme_classic()+
  geom_smooth(method = "glasso", se = F, color = "#C6DBEF") +
  #geom_text_repel(aes(label=city))+
  geom_text(aes(x=max(city_dissim$ratio.f.m.)*4/5, y=max(city_dissim$dissim,na.rm = T)*4/5,
                label=paste0('estimated correlation:',round(cor_city[1],4),"~",round(cor_city[2],4))))

#------------------------- 3.4 correlation between D and Ds ------------------------- 
city_D_Ds <- city_D_Ds[city_D_Ds$city!='重庆市',] # 重庆市缺少中类数据
city_D_Ds_t <- city_D_Ds[-which.max(city_D_Ds$Ds),] # 重庆市缺少中类数据

cor.test(city_D_Ds_t$Ds, city_D_Ds_t$D)

plot_D_Ds <- ggplot(city_D_Ds,aes(D, Ds))+
  geom_point(aes(color=Ds),shape = 21, size = 2, stroke = 1.6,alpha=0.8) +
  geom_text(x=0.45, y=0.4,label='estimated correlation= 0.62') +
  geom_smooth(method = "glm", se = F, color = "firebrick3") +
  geom_abline(aes(intercept=0, slope=1),linetype="dotted")+
  ylim(0,0.5)+
  xlim(0,0.5)+
  theme_classic() +
  coord_equal()+
  ylab("Ds指数") +
  xlab("D指数") +
  ggtitle("D指数与Ds指数d")

density_D_Ds <- ggplot(city_D_Ds)+
  geom_histogram(aes(D),fill='#C6DBEF',color='steelblue',bins = 40,alpha=.7)+
  geom_histogram(aes(Ds),fill='firebrick3',color='coral',bins = 40,alpha=.2)+
  theme_classic() +
  ylab("频次") +
  xlab("指数值") +
  ggtitle("D指数与Ds指数的分布")
# Ds指数更少受到职业规模变化的影响，Ds指数值的分布更为集中，而D指数分布较分散。
library(showtext);showtext_auto()

ggsave( "export/plot_D_Ds.pdf", plot_D_Ds, width = 7, height = 6)


#------------------------- 3.5 correlation sexratio, D and Ds ------------------------- 
cor.test(city_D_Ds$f_ratio,city_D_Ds$Ds,method = "pearson")

plot_sexratio <- ggplot(sexratio)+
  geom_histogram(aes(`0_4_m/f`),fill='#C6DBEF',color='steelblue',bins = 40,alpha=.7)+
  geom_histogram(aes(`0_m/f`),fill='firebrick3',color='coral',bins = 40,alpha=.2)+
  theme_classic() +
  ylab("频次") +
  xlab("指数值") +
  ggtitle("性别比")

plot_D_sex <- ggplot(city_D_Ds,aes(f_ratio,Ds))+
  geom_point(aes(color=Ds),shape = 21, size = 2, stroke = 1.6,alpha=0.8) +
  geom_text(x=0.48, y=0.4,label='pearson correlation= - -0.54') +
  #geom_smooth(method = "glm", se = F, color = "firebrick3") +
  theme_classic() +
  ylab("Ds指数") +
  xlab("就业人口中的女性占比")

ggsave( "export/fratio_Ds.pdf", width = 7, height = 6)


#------------------------- 3.5 correlation morality ------------------------- 
city_D_Ds$`0_4_m/f`
ggplot(city_D_Ds,aes(`0_4_m/f`,f_ratio))+
  geom_point(color='coral',size = 2, stroke = 1,alpha=0.6) +
  #geom_text(x=0.48, y=0.4,label='pearson correlation= - -0.54') +
  geom_smooth(method = "lm", se = F, color = "slategray3") +
  theme_classic() +
  ylab("F/M") +
  xlab("出生性别比")

#------------------------- 3.5 occupation class ------------------------- 

ggplot(city_ocp_sex)+
  geom_histogram(aes(feminine),fill='#C6DBEF',color='steelblue',bins = 40,alpha=.7)+
  geom_histogram(aes(Masculine),fill='firebrick3',color='coral',bins = 40,alpha=.2)+
  geom_histogram(aes(Middle),fill='#FEEDA2',color='#FDBE6F',bins = 40,alpha=.2)+
  theme_classic() +
  ylab("频数") +
  xlab("各地区3类属性的职业数量")
ggsave( "export/sex_ocp.pdf", width = 7, height = 6)


# -------------------------------------------------
ggplot(data = test_vis, aes(x = factor(hour_timetable), y = mean_delayed_pass, group = 1)) + 
  ylim(0, NA) + 
  geom_point(color = 'purple', stat = 'identity') + 
  geom_polygon(color = 'purple', fill=NA) + 
  coord_polar(start = - pi * 1/24)


# boxplot: D in regions --------------------------------------------

dataplot = rbind(datamodel10[, c("unit", "a", "d", "ds", "eduyear_mean", "eduyear_mean_f", "tfr", "region.y")], 
                 datamodel00[, c("unit", "a", "d", "ds", "eduyear_mean", "eduyear_mean_f", "tfr", "region.y")],
                 datamodel90[, c("unit", "a", "d", "ds", "eduyear_mean", "eduyear_mean_f", "tfr", "region.y")])

dataplot$year = rep(c(2010, 2000, 1990), c(nrow(datamodel10), nrow(datamodel00), nrow(datamodel90))) 
dataplot$region.y = factor(dataplot$region.y, levels = c("西部", "中部", "东北", "东部"))

ggplot(dataplot) + 
  geom_boxplot(aes(factor(region.y), ds, group = region.y), show.legend = FALSE) +
  theme_bw() + 
  facet_wrap(year~., nrow = 1, scales = "free_x") + 
  labs(x = NULL,
       y = "职业隔离指数(A)") + 
  theme_bw() + 
  theme(text = element_text(family = "stsong", size = 18),
        panel.grid = element_blank())

# edu & index ----------------------------------------------

dataplot %>% select(year, a, d, ds, tfr) %>%
  pivot_longer(col = 2:4, names_to = "index", values_to = "value") %>%
  mutate(index = factor(index, levels = c("d", "ds", "a"))) %>% 
  ggplot(aes(tfr, value, colour = factor(year), shape = factor(year))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  theme_bw() +
  facet_wrap(.~index, scales = "free_y") +
  theme_bw() + 
  labs(x = "地区的总和生育率",
       y = "职业性别隔离指数") +
  theme(axis.text = element_text(vjust = 0, size = rel(1.2)),
        strip.text = element_text(rel(1.5)),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.spacing = unit(0.5,"cm"),
        legend.direction = "horizontal",
        legend.position = "bottom", 
        legend.key.height = unit(0.3, 'cm'),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", colour = "gray30")) + 
  guides(linetype = guide_legend(keywidth = 3, keyheight = 1)) 

ggsave(filename = "fig/chart_index_tfr2.pdf", plot = last_plot(), width = 13, height = 8)

cor.test(dataplot$a[dataplot$year == "1990"], 
         dataplot$eduyear_mean_f[dataplot$year == "1990"]) 

ggplot(dataplot) + 
  geom_boxplot(aes(factor(year), eduyear_mean_f, colour = factor(year))) + 
  theme_bw()

dataplot %>%
  group_by(year) %>%
  summarise(eduyear = sd(eduyear_mean_f))

ggplot(dataplot) + 
  geom_point(aes(gdp3_prop, a, colour = factor(year))) + 
  geom_smooth(aes(gdp3_prop, a, colour = factor(year)), method = "lm") +
  theme_bw()

cor.test(subset(dataplot, year == 1990)$a - subset(dataplot, year == 2000)$a, 
         c(subset(dataplot, year == 1990)$gdp3_prop - subset(dataplot, year == 2000)$gdp3_prop))

ggplot(dataplot %>% filter(year == 2010, !region.y %in% c("西部")),
       aes(eduyear_mean_f, d, colour = factor(region.y))) + 
  geom_point() + 
  theme_bw()

subset(dataplot, tfr > 4)
ggsave("fig/chart_spatiala.pdf", width = 8, height = 6)




