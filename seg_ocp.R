library(tidyverse)
library(openxlsx)
library(rgdal)

options(scipen = 999)
setwd("C:/Users/WXY/Documents/R_learning/segregation")

# load in data ------------------------------------------------------------------------------

# population by occupations
data10_city = read.xlsx('data/occupation_city.xlsx', sheet = 1)

# data05 = haven::read_dta("C:/Users/WXY/Documents/R_learning/segregation/data/census05/data_num.dta")
data00_city = readRDS("data/data00_city.rds")
data00_city2 = read.xlsx("data/data00_city2.xlsx", sheet = 1)
data90_city = readRDS("data/data90_city.rds")

malecol <- which(data00[1,]=="男")[-c(1:2, 71)]
for (i in malecol) {
  data00[-1, i] <- as.numeric(data00[-1, i-1]) - as.numeric(data00[-1, i+1])
  print(i)
}

# geographical data
map_04 <- readOGR("C:/Users/WXY/Documents/R_learning/spatial/data/xingzhengquhua_shp/dijishi_2004.shp", use_iconv=TRUE, encoding='UTF-8', stringsAsFactors = F)

city_name2005 <- read.xlsx("data/cityname_05.xlsx", sheet = 1)
cityname_00 <- read.xlsx("data/cityname_00.xlsx", sheet = 1)

# yearbook data 
citydata_10 <- read.xlsx('data/城市统计年鉴2010/0_全市综合.xlsx',sheet = 1)
citydata_00 <- read.xlsx('data/yearbook.xlsx', sheet = 2)
citydata_90 <- read.xlsx('data/yearbook.xlsx', sheet = 3)

# fertility
fertrate_10 = read.xlsx('data/fertilityrate_10.xlsx', sheet=1)


########################## tidy data #########################

## occupation data 2010 --------------------------
data10_city$sex <- NA
data10_city$sex[grep("女|男|总计", data10_city$city)] <- data10_city$city[grep("女|男|总计", data10_city$city)]
data10_city$sex <- zoo::na.locf(data10_city$sex)
data10_city$sex = ifelse(data10_city$sex == '男', 'm', ifelse(data10_city$sex == '女', 'f', data10_city$sex))
# data10_city$city <- str_squish(data10_city$city)
data10_city[, 3:(ncol(data10_city) - 1)] <- sapply(data10_city[, 3:(ncol(data10_city)-1)], as.numeric)

data10_city <- data10_city %>%
  filter(sex %in% c("m", "f") & !data10_city$city %in% c("男", "女")) %>%
  select(c(1:3, ncol(data10_city), 4:(ncol(data10_city) - 1))) %>% 
  gather(., key = "occupation", value = "pop", 5:ncol(data10_city))

## occupation data 2005 --------------------------
# data05_city <- data05 %>%
#   zap_labels(.) %>%
#   filter(age >= 15 & r19 %in% c(1, 2)) %>%
#   left_join(., city_name2005[, c("code", "name")], by = c("dz_code" = "code")) %>%
#   select(name, r21, r3) %>%
#   group_by(name, r21, r3) %>%
#   summarise(n = n())
# 
# data05_city <- left_join(data05_city, city_name2005[, c("code", "name")], by = c("dz_code" = "code"))
# data05_city$dz_code[which(is.na(data05_city$name))] %>% unique(.)
# 
# names(data05_city) = c("city", "occupation", "sex", "pop")
# data05_city$sex = ifelse(data05_city$sex == 1, 'm', ifelse(data05_city$sex == 2, 'f', data05_city$sex))

## occupation data 2000 --------------------------
names(data00_city)[c(3, 2, 4, 1)] = c('sex', 'occupation', 'pop', 'city')
data00_city$sex = ifelse(data00_city$sex == 1, 'm', ifelse(data00_city$sex == 2, 'f', data00_city$sex))

data00_city2$city = trimws(data00_city2$city)
data00_city2[, 4:ncol(data00_city2)] <- sapply(data00_city2[, 4:ncol(data00_city2)], as.numeric)
data00_city2 = data00_city2 %>%
  filter(sex!="total" & city %in% cityname_00$name_cn) %>%
  pivot_longer(cols = 5:74,
               names_to = "occupation",
               values_to = "pop") %>%
  filter(occupation != "七、不便分类的其他从业人员")

# occupation data 1990
names(data90_city)[c(3, 2, 4, 1)] = c('sex', 'occupation', 'pop', 'city')
data90_city$sex = ifelse(data90_city$sex == 1, 'm', ifelse(data90_city$sex == 2, 'f', data90_city$sex))

# yearbook data
citydata_10$prov <- NA
citydata_10$prov[grep("省|自治区|北京市|上海市|天津市|重庆市|城市合计", citydata_10$city)] <- 
  citydata_10$city[grep("省|自治区|北京市|上海市|天津市|重庆市|城市合计", citydata_10$city)]
citydata_10$prov <- zoo::na.locf(citydata_10$prov)
citydata_10$city <- citydata_10$city %>% str_squish(.)
citydata_10 <- citydata_10[,c(1, grep('全市', names(citydata_10)), ncol(citydata_10))]

citysample = citydata_10$city[!grepl("省|自治区|城市合计", citydata_10$city) & !citydata_10$prov %in% c("西藏自治区", "新疆维吾尔自治区")]


############################### compute D ################################

# function: get dissimilarity
get_D <- function(df){
  df <- df[, c('city', 'sex', 'occupation', 'pop')]
  df[, 4] <- as.numeric(df[, 4])
  df <- tidyr::pivot_wider(df, names_from = "sex", values_from  = 4) %>%
    mutate(prop_f = f/(f + m), prop_m = m/(f + m), D = prop_f - prop_m)
  df <- tidyr::spread(df[ , c('city', 'occupation', 'D')], key = "occupation", value = 3)
  df$D <- apply(df[ , 2:ncol(df)], 1, FUN = function(x){
    sum(abs(x), na.rm = T)/2} )
  D_df <- df[, c('city','D')]
}

############################# 1) D at province level ###########################
names(ocp_prov) <- gsub("/r/r/n","", names(ocp_prov))
names(ocp_prov)[1] <- '地区'
ocp_prov$sex <- NA
ocp_prov$sex[grep("女|男|全国",ocp_prov$地区)] <- ocp_prov$地区[grep("全国|女|男",ocp_prov$地区)]
ocp_prov$sex <- zoo::na.locf(ocp_prov$sex)

# dissimilarity of main category 
prov_level <- (ocp_prov$sex%in%c("男","女")) %>%
  ocp_prov[., c(1:2, grep("一|二|三|四|五|六|七|sex",names(ocp_prov)))]
prov_level <- prov_level[!prov_level$地区%in%c("男","女"),c(1,10,2,3:9)]%>%
  tidyr::gather(.,key="occupation",value="pop",4:10)
prov_level <- prov_level[order(prov_level$地区),]
prov_level$prop <- (prov_level$pop/prov_level$合计)

prov_dissim <- get_D(prov_level)
  
# dissimilarity detailed occupation 
prov_level_d <- (ocp_prov$sex%in%c("男","女")&!ocp_prov$地区%in%c("男","女"))%>%ocp_prov[.,c(-grep("一|二|三|四|五|六",names(ocp_prov)))]
prov_level_d <- tidyr::gather(prov_level_d[,c(1,2,67,3:66)],key="occupation",value="pop",4:67)
names(prov_level_d)
prov_level_d <- prov_level_d[order(prov_level_d$地区),]
prov_level_d$prop <- (prov_level_d$pop/prov_level_d$合计)
prov_dissim_d <- get_dissim(prov_level_d)


############################# 2) D at city level ###########################

#data10_city <- data10_city %>% 
#  filter(city %in% map_04$NAME2004)

data10_city <- data10_city[data10_city$city!='重庆市', ] # 重庆市缺少中类数据
data10_city[duplicated(data10_city[, c("city", "sex", "occupation")]), ]

## main category
D_city10 <- data10_city %>%
  select(city, occupation, sex, pop) %>%
  filter(city %in% citysample) %>%
  filter(grepl("一|二|三|四|五|六", occupation) == TRUE) %>%
  get_D(.)

## sub-category occupation 
data10_city[is.na(data10_city)] = 0

D_city10 <- data10_city %>%
  select(city, occupation, sex, pop) %>%
  filter(city %in% citysample) %>%
  filter(grepl("一|二|三|四|五|六|七", occupation) == FALSE) %>%
  get_D(.)

test_10 <- data10_city %>%
  select(city, occupation, sex, pop) %>%
  filter(city %in% citysample) %>%
  filter(grepl("一|二|三|四|五|六|七", occupation) == FALSE)
#data10_city <- data10_city[order(data10_city$省), ]
#data10_city$prop <- (data10_city$pop/data10_city$合计)
D_city10 <- get_D(city_level)

# 2. compute Ds -----------------------------------------

get_Ds <- function(data, x){
  df <- data[data[, 'city'] == x, c('sex','occupation','pop')]
  df$pop = as.numeric(df$pop)
  df$pop[is.na(df$pop)] = 0
  df_long <- pivot_wider(df, names_from = "sex", values_from = "pop")
  df_long$total = df_long$m + df_long$f
  df_long$p_f <- df_long$f/df_long$total
  df_long$p_m <- df_long$m/df_long$total
  Ds <- sum(abs(df_long$p_f/sum(df_long$p_f, na.rm=T) - 
                  df_long$p_m/sum(df_long$p_m, na.rm=T)), na.rm = T)/2
  return(Ds)
}


########################### (1) Ds of class at city level #########################

data10_city1 = data10_city %>% 
  filter(city%in%citysample & grepl("一|二|三|四|五|六", occupation))
Ds_city10_class <- sapply(unique(data10_city1$city), 
                          function(x)get_Ds(data = data10_city1, x = x))

Ds_city10_class <- data.frame(city = names(Ds_city10_class), Ds = Ds_city10_class, stringsAsFactors = F)

########################### (2) Ds of sub-class at the city level #########################

Ds_city90 <- sapply(unique(data90_city$city), function(x)get_Ds(data = data90_city, x = x))
Ds_city90 <- data.frame(city = names(Ds_city90), Ds = Ds_city90, stringsAsFactors = F)

Ds_city00 <- sapply(unique(data00_city$city), function(x)get_Ds(data = data00_city, x = x))
Ds_city00 <- data.frame(city = names(Ds_city00), Ds = Ds_city00, stringsAsFactors = F)

Ds_city00.2 <- sapply(unique(data00_city2$city), 
                      function(x)get_Ds(data = data00_city2, x = x))
Ds_city00.2 <- data.frame(city = names(Ds_city00.2), Ds = Ds_city00.2, stringsAsFactors = F)

#Ds_city05 <- sapply(unique(data05_city$city), function(x)get_Ds(data = data05_city, x = x))
#Ds_city05 <- data.frame(city = names(Ds_city05), Ds = Ds_city05, stringsAsFactors = T)

data10_city2 = data10_city %>% filter(city%in%citysample & !grepl("一|二|三|四|五|六|七", occupation))
Ds_city10 <- sapply(unique(data10_city2$city), function(x)get_Ds(data = data10_city2, x = x))

Ds_city10 <- data.frame(city = names(Ds_city10), Ds = Ds_city10, stringsAsFactors = F)


plot(Ds_city10$Ds[Ds_city10$city != "重庆市"], Ds_city10_class$Ds[Ds_city10_class$city != "重庆市"], main = "", xlab = "", ylab = "")
title(main = "地级市职业大类和中类的Ds指数的相关性", xlab = "职业中类的Ds", ylab = "职业大类的Ds")
cor.test(Ds_city10$Ds[Ds_city10$city != "重庆市"], Ds_city10_class$Ds[Ds_city10_class$city != "重庆市"])

Ds_3year = left_join(Ds_city00, Ds_city90, by = c("city" = "city"))
Ds_3year = left_join(Ds_3year, Ds_city10, by = c("city" = "city"))


################################### fertility ###############################
Ds_city10 = left_join(Ds_city10, fertrate_10 %>% 
                        filter(city %in% citysample), by = c("city" = "city"))
plot(Ds_city10$`15-64岁妇女平均活产子女数`[Ds_city10$city != "重庆市"], Ds_city10$Ds[Ds_city10$city != "重庆市"]) 
Ds_city10 = left_join(Ds_city10, citydata_10[, c("city", "人均地区生产总值_全市(元)")], 
                      by = c("city" = "city"))
names(Ds_city10)[3:4] <- c("fertility_rate", "GDP_percap")
plot(log(Ds_city10$GDP_percap[Ds_city10$city != "重庆市"]), Ds_city10$Ds[Ds_city10$city != "重庆市"]) 

## compute TFR
group_by(age_group) %>%
  summarise(female = n(), birth = sum(child))


################################### marketization ###############################
library(psych)
names(citydata_00)
market_00 <- citydata_00 %>%
  filter(city %in% Ds_city00$city) %>%
    mutate(private_worker = `城镇个体从业人员_全市`/`年末单位从业人员数_全市`,
           industry_local = `规模以上工业产值_内资企业_全市`/`规模以上工业总产值(当年价,.万元)_全市`, 
           avgsalary = `职工平均工资_全市(元)`,
           productquant_3 = `第三产业占GDP的比重_全市`,
           productwork_3 = `第三产业从业人员比重_全市`,
           gdp_percap = `2000年国内生产总值（万元）_全市`/`年末总人口（万人）_全市`,
           edu_goven = `教育支出`/`财政支出`) %>%
  select(city, private_worker, industry_local, edu_goven, avgsalary, productwork_3, productquant_3, gdp_percap)

# 确定因子数量
fa.parallel(market_00[, -1])
fa.parallel(X2)
fa.parallel(X3)

# 提取因子, 设置了因子旋转
fa00 = fa(market_00[, -1], nfactors = 2, rotate = 'varimax') 
fa00 = fa(X2, nfactors = 1, rotate = 'varimax') 
fa00 = fa(X3, nfactors = 3, rotate = 'varimax')

round(fa00$loadings, 3)


################################### Ds & D #################################
names(city_D)[1] <- 'city'
city_Ds <- left_join(city_Ds,city_D,id='city')
cor.test(city_Ds$Ds, city_Ds$D)

################################### sex ratio ###############################

#---------------------------  sex ratio(f/m) at city level --------------------------- 

city_D_Ds <- left_join(city_D_Ds, sexratio, by='city')

city_ocprate <- tidyr::spread(city_level_d[!duplicated(city_level_d[,c(1,3,4)]),c(1,3,4)],key="sex",value=3)
city_ocprate$f_ratio <- (city_ocprate$女/(city_ocprate$男 + city_ocprate$女))
names(city_ocprate)[1] <- 'city'
city_D_Ds <- left_join(city_D_Ds,city_ocprate[,c(1,4)],by='city')

city_ocp_sex <- tidyr::spread(city_level_d[ ,c(1:3,5,6)],key="sex",value=5)
city_ocp_sex$女[is.na(city_ocp_sex$女)] <- 0
city_ocp_sex$f_rate <- (city_ocp_sex$女/(city_ocp_sex$男+city_ocp_sex$女))
city_ocp_sex$f_class <- car::recode(city_ocp_sex$f_rate,
                                    "lo:0.2999999='feminine';0.3:0.7='Middle';
                                    NA=NA;else='Masculine'")
city_ocp_sex <- tidyr::spread(city_ocp_sex[,c(1:3,7)],key="occupation",value=4)
city_ocp_sex$feminine <- sapply(1:nrow(city_ocp_sex),
                                function(x){sum(city_ocp_sex[x,3:66]=='feminine',na.rm = T)})
city_ocp_sex$Masculine <- sapply(1:nrow(city_ocp_sex),
                                function(x){sum(city_ocp_sex[x,3:66]=='Masculine',na.rm = T)})
city_ocp_sex$Middle <- sapply(1:nrow(city_ocp_sex),
                                function(x){sum(city_ocp_sex[x,3:66]=='Middle',na.rm = T)})

xlsx::write.xlsx(city_ocp_sex[,c(1,2,67:69)],'data/occupation_sextype.xlsx')

#---------------------------  sex ratio(f/m) at province level --------------------------- 

sex_ratio <- ocp_prov[ocp_prov$sex!="全国",c(1,2,ncol(ocp_prov))]

sex_ratio <- data.frame(prov=c("全国",unique(sex_ratio$地区)[2:32]),
                        'ratio(f/m)'=sex_ratio[(nrow(sex_ratio)/2+1):nrow(sex_ratio),2]/sex_ratio[1:(nrow(sex_ratio)/2),2],
                        stringsAsFactors = F)

prov_dissim <- left_join(prov_dissim,sex_ratio,by="prov")

######################### regions city in #########################

region <-c('东部','北京','天津','河北','上海','江苏','浙江','福建','山东','广东','海南','中部','山西','安徽','江西','河南','湖北','湖南','西部','内蒙古','广西','重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆','东北','辽宁','吉林','黑龙江')

