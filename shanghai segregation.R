# Variable: distance from center city------------------------------------------------------
pacman::p_load(sp, baidumap)
library(tidyverse)

lon  <- as.numeric(garbage[,'lon'])
lat <- as.numeric(garbage[,'lat'])
m1 <- cbind(lon, lat) %>% as.matrix()

sh_coordinate <- getPlace('上海')[, c('lat', 'lon')]
garbage$dis_centr <- spDistsN1(m1, as.numeric(c(121.4704078, 31.2303834)), longlat = TRUE)

summary(garbage$dis_centr)
tail(sort(garbage$dis_centr), 10)

# outlier
garbage[, c("A2community", "A3community", 'A5', "lon", "lat")][which(garbage$dis_centr==max(garbage$dis_centr)), ]
garbage[, "lat"][which(garbage$A5=='江秀苑')] <- jitter(garbage[, "lat"][which(garbage$A5=='堡镇南路217号-369号')], factor = 0.0001)
garbage[, "lon"][which(garbage$A5=='江秀苑')] <- jitter(garbage[, "lon"][which(garbage$A5=='堡镇南路217号-369号')], factor = 0.0001)

# distance level
garbage$dis_lev <- NA
garbage$dis_lev <- ifelse(garbage$dis_centr <= 5.5, garbage$dis_lev <- 1, 
                          ifelse(garbage$dis_centr <= 9.5,garbage$dis_lev <- 2, 
                                 ifelse(garbage$dis_centr <= 18, garbage$dis_lev <- 3, garbage$dis_lev <- 4)))

garbage$dis_lev <- factor(garbage$dis_lev)

garbage$dis_lev2 <- NA
garbage$dis_lev2 <- ifelse(garbage$dis_centr <= 10, garbage$dis_lev <- 1, 
                          ifelse(garbage$dis_centr <= 30,garbage$dis_lev <- 2, 3))

garbage$dis_lev2 <- factor(garbage$dis_lev2)

# Variable: luxurious cars -----------------------------------------------------------
# house type
grep('^C100.*\\d$', names(data13)); names(data13)[grep('^C100.*\\d$', names(data13))]

nrow(data13) ; nrow(garbage)

# which row in garbage is different from data13
which(!rownames(data13)%in%rownames(garbage)) 

garbage <- cbind.data.frame(garbage,data13[-c(530, 663),grep('^C100.*\\d$', names(data13))])

garbage[,36][which(is.na(garbage[,36]))] <- 0 # treat NA as 0
garbage[,30:36] <- garbage[,30:36]/100

garbage$house_tp_sum <- apply(garbage[,30:36], 1, sum)

garbage$house_tp_sum[which(garbage$house_tp_sum != 1)]

# house type
garbage$house_tp <- NA
garbage$house_tp[which(garbage$C1006 > 0.5)] <- 6
garbage$house_tp[which(garbage$C1005 > 0.5)] <- 5
garbage$house_tp[which(garbage$C1004 > 0.5)] <- 4
garbage$house_tp[which(garbage$C1003 > 0.5)] <- 3
garbage$house_tp[which(garbage$C1002 > 0.5)] <- 2
garbage$house_tp[which(garbage$C1001 > 0.5)] <- 1

garbage$house_tp_sum[is.na(garbage$house_tp)]

hist(garbage$house_tp)

sum(which(garbage$C1005 > 0.5)%in%which(garbage$C1006 > 0.5))

# cars rate
options(scipen = 999)

garbage$cars <- data13$C901[-63]
garbage$sh_cars <- data13$C902[-63]/100
which(!is.na(garbage$sh_cars)&is.na(garbage$cars))

hist(garbage$cars, ylab = "", xlab = "", axes = F)
axis(1)
axis(2)

garbage$cars <- round(garbage$cars/100, 4)
garbage$cars[which(garbage$cars >1)] <- 1.01

#-- point Plot: luxurious cars and local cars -----------------------------------------------
cars_base <- ggplot(garbage %>% filter(!is.na(garbage$cars)&!is.na(garbage$sh_cars)),
                    aes(x = area,
                        y = cars)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6),
        axis.text.y = element_text(family="Courier",face="bold",size = 10),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 10,family = "hanyi",face = 'bold',lineheight = 1.2),
        legend.justification = c(1,1),
        panel.grid.major = element_line(colour = alpha('grey60', 0.7)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(colour = alpha('grey60', 0.7)),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white', color = alpha('grey60', 0.7))
  )
community_text <- garbage[garbage$cars > 0.8&!is.na(garbage$cars)&!is.na(garbage$sh_cars)&garbage$house_tp!=1, ]

library(scales)
library(viridis); library("ggrepel")
cars_plot <- ggplot(garbage%>% filter(!is.na(garbage$cars)&!is.na(garbage$sh_cars)),
       aes(x = sh_cars,
           y = cars, color = dis_lev2)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6)) +
  geom_point(position = position_jitter(width = 0.01, height = 0.01),size = 2) +
  stat_smooth() + 
  scale_color_viridis(name="离市政府的直线距离", discrete=TRUE) +
  facet_wrap(.~dis_lev2, nrow = 2) +
  theme_classic()
  ggtitle('豪车率与沪牌比例散点图', subtitle = '（去掉里弄房）')
  #geom_text_repel(aes(label = A5), color = "gray20", fontface = 'bold',
                  #data = community_text, segment.colour = 'lightblue',
                  #alpha = 0.8, force = 10)


export::graph2ppt(cars_plot, 'export/cars_plot.pptx')

save.image('.RData')

# histogram Plot: luxurious cars and distance level --------------------------------
cars_hist <- ggplot(garbage%>% filter(!is.na(garbage$cars)&!is.na(garbage$sh_cars)&garbage$house_tp!=1),
       aes(x = cars)) +
  geom_histogram( alpha= 0.6, bins = 35, colour = 'black', fill = 'grey80') +
  facet_wrap(~dis_lev2, ncol = 2) +
  theme_classic() +
  labs(x = '豪车率',
       y = '频次',
       title = '四个居住区（按距离划分）的豪车率分布')

export::graph2ppt(cars_point, 'cars_point.pptx')
library(ggridges)
  
  ggplot(garbage%>% filter(!is.na(garbage$cars)&!is.na(garbage$sh_cars)&garbage$house_tp!=1),
         aes(x = cars, y= dis_lev, fill = factor(..quantile..))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE, size = 1) +
    scale_fill_viridis(discrete = TRUE, name = "四分位线") +
    scale_x_continuous(breaks = c(0.1888, 0.1500, 0.1373, 0.1019, 0.4, 0.8), labels = percent(c(0.1888, 0.1500, 0.1373, 0.1019, 0.4, 0.8)))+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  

# histogram Plot: bad behaviour and distance level --------------------------------
summary(garbage$badbhv)
dist_badbhv <- garbage %>% filter(!is.na(badbhv))%>% count(dis_lev2, badbhv) %>%
                   group_by(dis_lev2)%>% mutate(sum = sum(n)) %>% mutate(badrate = round(n/sum,2))

ggplot(dist_badbhv,
       aes(x = dis_lev2, y = sum, fill = badbhv)) +
  geom_bar(stat = "identity",  color='grey', width = 0.3) +
  scale_fill_manual(values = c('white', 'grey60'), name = '是否有不文明行为', labels = c('没有', '有')) +
  geom_text(aes(label = scales::percent(badrate, 2)),data = filter(dist_badbhv, badbhv==1),
            vjust = 1.3, size = 5, color='white', fontface = 'bold') +
  geom_text(aes(label = scales::percent(badrate, 2)),data = filter(dist_badbhv, badbhv==0),
            vjust = -0.3, size = 5, color='grey80', fontface = 'bold') +
  ggtitle('距市中心9.5公里到18公里范围的小区内不文明行为发生频率最多', subtitle = '（不文明行为如吐痰和乱认垃圾等）') +
  theme_classic()


# houese type ------------------------------------------------------------------------
house_tp <- commu_obsv_df %>% group_by(dis_lev) %>% count(house_tp) %>% na.omit() %>% mutate(sum = sum(n)) %>%
  mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(house_tp, 'export/spatial_unequal.xlsx', sheetName = 'houese_tp', append = T)


# sports infrastractrue ------------------------------------------------------------------------
excer_equip <- commu_obsv_df %>% group_by(dis_lev2) %>% count(C101) %>% na.omit() %>% mutate(sum = sum(n)) %>%
  mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(excer_equip, 'export/spatial_unequal.xlsx', sheetName = 'excer_equip', append = T)

C103 <- commu_obsv_df %>% group_by(dis_lev) %>% count(C103) %>% na.omit() %>% mutate(sum = sum(n)) %>%
  mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(C103, 'export/spatial_unequal.xlsx', sheetName = 'C103', append = T)

C304 <- commu_obsv_df %>% group_by(dis_lev) %>% count(C304) %>% na.omit() %>% mutate(sum = sum(n)) %>%
  mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(C304, 'export/spatial_unequal.xlsx', sheetName = 'C304', append = T)

# pop structrue ------------------------------------------------------------------------
pop_str <- suns_df %>% count(age2, migrant) %>% group_by(age2) %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum)
xlsx::write.xlsx(C304, 'export/spatial_unequal.xlsx', sheetName = 'C304', append = T)

suns_df %>%
  ggplot(aes(x=age2, fill = factor(migrant))) + 
  geom_bar(stat = 'count', position = 'stack')

suns_df$shhukouplname

# dustbin cleanliness ------------------------------------------------------------------------
C203 <- commu_obsv_df %>% group_by(dis_lev) %>% count(C203) %>% na.omit() %>% mutate(sum = sum(n)) %>%
  mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(C203, 'export/spatial_unequal.xlsx', sheetName = 'C203', append = T)

#
agree1_hs_gender <- suns_df %>% group_by(pp_Gender, housewk) %>% count(agreesort1) %>% na.omit() %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% as.data.frame()
xlsx::write.xlsx(agree3_hs_gender, 'export/wastesort.xlsx', sheetName = 'agree3_hs_gender', append = T)

# housework
a3_hs <- suns_df %>% group_by(housewk) %>% count(agreesort3) %>% na.omit() %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(a3_hs, 'export/wastesort.xlsx', sheetName = 'a3_hs', append = T)

ggplot(agree3_age, aes(x = age2, y = n, fill = factor(agreesort3))) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  geom_text(aes(x = age2, y = sum-n/2, label = percent(percent)), data = agree3_age%>%filter(agreesort3==1), vjust = 0.5) +
  geom_text(aes(x = age2, y = sum, label = sum), data = agree3_age%>%filter(agreesort3==1), vjust = -1)

