library(tidyr) 
library(dplyr) 
library(openxlsx) 
library(magrittr) 
library(ggplot2) 
library(showtext) 
#library(plotly) 
#library(data.table) 

options(scipen = 999) # turn off scientific notation

## add fonts 
font_add("stsong", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/STSONG.TTF")
showtext_auto() 

# read in data --------------------------------------- 

# data at city level 

# 2010
lfpr_tfr_10 = read.csv("data/lfpr_tfr_10.csv", stringsAsFactors = FALSE)

# 2000
lfpr_tfr_00 = read.csv("data/lfpr_tfr_00.csv") 

# 1990
lfpr_tfr_90 = read.csv("data/lfpr_tfr_90.csv") 

# city infomation 
city_list = read.xlsx('data/city_list.xlsx', sheet = 1)
city_list$province_name = gsub("维吾尔|回族|壮族|自治区|省|市", "", city_list$province_name)

city_name00 = read.xlsx("data/cityname_00.xlsx", sheet = 1)

# fertility and labour participate rate in 2005
# Not included in this research because data of 2005 is not available //2005年的人口抽查数据尚未公开，本研究没有分析2005年的数据
# tfr_05 = read.xlsx('data/lfp_tfr.xlsx', sheet = "TFR05")
# lfp_05 = read.xlsx("data/lfp_tfr.xlsx", sheet = "LFPR05")

## tfr in 2005 
# lfpr_05 = lfp_05 %>% 
#   mutate(city = trimws(city)) %>% 
#   filter(city %in% unique(lfpr_10$city), sex %in% c("f", "m")) %>% 
#   mutate(partici_rate = (`正在寻找工作` + `合计`)/(`不在业_合计`+ `合计`))
# 
# tfr_05_2 = tfr_05 %>% 
#   mutate(city = trimws(city), tfr = `总和生育率`/1000) %>%
#   filter(city %in% unique(lfpr_10$city)) %>%
#   select(city, tfr)
# 
# lfpr_tfr_05 = left_join(lfpr_05[, c("city", "sex", "partici_rate")], tfr_05_2, by = c("city" = "city"))
# 
# lfpr_tfr_05 %>% 
#   ggplot(aes(tfr, partici_rate, colour = sex)) +
#   geom_point()

## [!NOT RUN]yearbook data  //年鉴数据[!不运行]
# citydata_10$prov <- NA
# citydata_10$prov[grep("省|自治区|北京市|上海市|天津市|重庆市|城市合计", citydata_10$city)] <- citydata_10$city[grep("省|自治区|北京市|上海市|天津市|重庆市|城市合计", citydata_10$city)]
# citydata_10$prov <- zoo::na.locf(citydata_10$prov)
# citydata_10 <- citydata_10[,c(1, grep('全市', names(citydata_10)), ncol(citydata_10))]
# citysample = citydata_10$city[!grepl("省|自治区|城市合计", citydata_10$city) & !citydata_10$prov %in% c("西藏自治区", "新疆维吾尔自治区")]


# education --------------------------------
# education's effect on female LFPR

edu90 = read.csv("data/edu90.csv", stringsAsFactors = FALSE)
edu00 = read.csv("data/edu00.csv", stringsAsFactors = FALSE)
edu10 = read.csv("data/edu10.csv", stringsAsFactors = FALSE)

# 2010

names(lfpr_tfr_10)
#lfpr_tfr_10 = lfpr_tfr_10[, -grep("edu", names(lfpr_tfr_10))]
lfpr_tfr_10 = lfpr_tfr_10 %>%
  left_join(., edu10, by = c("name_harmonized" = "name_harmonized")) 

lfpr_tfr_10$middlehigh_edu = lfpr_tfr_10$senior_edu + lfpr_tfr_10$high_edu
lfpr_tfr_10$middlehigh_edu_f = lfpr_tfr_10$senior_edu_f + lfpr_tfr_10$high_edu_f
lfpr_tfr_10$compulsary_edu = lfpr_tfr_10$primary_edu + lfpr_tfr_10$junior_edu
lfpr_tfr_10$compulsary_edu_f = lfpr_tfr_10$primary_edu_f + lfpr_tfr_10$junior_edu_f

# 2000

names(lfpr_tfr_00) 
#lfpr_tfr_00 = lfpr_tfr_00[, -grep("edu", names(lfpr_tfr_00))]
lfpr_tfr_00 = lfpr_tfr_00 %>% 
  left_join(., edu00, by = c("name_harmonized" = "name_harmonized")) 

lfpr_tfr_00$middlehigh_edu = lfpr_tfr_00$senior_edu + lfpr_tfr_00$high_edu
lfpr_tfr_00$middlehigh_edu_f = lfpr_tfr_00$senior_edu_f + lfpr_tfr_00$high_edu_f
lfpr_tfr_00$compulsary_edu = lfpr_tfr_00$primary_edu + lfpr_tfr_00$junior_edu
lfpr_tfr_00$compulsary_edu_f = lfpr_tfr_00$primary_edu_f + lfpr_tfr_00$junior_edu_f

# 1990 

lfpr_tfr_90 = lfpr_tfr_90[, -grep("edu", names(lfpr_tfr_90))]
lfpr_tfr_90 = lfpr_tfr_90 %>% 
  left_join(., edu90, by = c("name_harmonized" = "name_harmonized")) 

lfpr_tfr_90$middlehigh_edu = lfpr_tfr_90$senior_edu + lfpr_tfr_90$high_edu
lfpr_tfr_90$middlehigh_edu_f = lfpr_tfr_90$senior_edu_f + lfpr_tfr_90$high_edu_f
lfpr_tfr_90$compulsary_edu = lfpr_tfr_90$primary_edu + lfpr_tfr_90$junior_edu
lfpr_tfr_90$compulsary_edu_f = lfpr_tfr_90$primary_edu_f + lfpr_tfr_90$junior_edu_f

##' Pearson相关性描述的是双变量线性关系，Spearman相关描述双变量单调关系（不一定保持恒定的相关性强度）。
##' 散点图将有助于可视化数据和理解应该使用哪个相关性系数。
##' 另一种方法是同时应用这两种相关性系数并检查哪一种表现得更好。
##' 如果结果显示Spearman系数大于Pearson系数，说明我们的数据是单调关系而不是线性关系。

lfpr_tfr_10 %>% 
  filter(sex != "total" & middle_high_edu) %>%
  ggplot(data = ., aes(x = middle_high_edu, y = lfpr, colour = sex)) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_smooth(method = "loess", fullrange = T) + 
  scale_colour_discrete(name = "性别", 
                      breaks=c("m", "f"),
                      labels=c("男", "女")) +
  ylab("劳动参与率") + 
  xlab("高中及以上教育程度人口在6岁以上人口中占比") + 
  #scale_x_continuous(breaks = seq(0, 0.6, 0.1), labels = seq(0, 0.6, 0.1), limits = c(0, 0.6)) +
  theme_bw() + 
  theme(text = element_text(family = "stsong", size = 18),
        panel.grid = element_blank())

ggsave("fig/lfpr_middle_edu.pdf", width = 10, height = 8) 

## school attending status 

head(data_90$CN1990A_EDLEV2)
data_90 %>%
  filter(age >= 15) %>%
  count(CN1990A_EDLEV2) %>%
  mutate(prop = n/sum(n))

head(data_00$EDUCCN)
data_00 %>% 
  filter(age >= 16) %>%
  mutate(edu_attend = ifelse(grepl("attending", EDUCCN, ignore.case = TRUE), 1, 2)) %>%
  count(edu_attend) %>%
  mutate(prop = n/sum(n))

# marketization ---------------------------------------------

unique(partici_rate10[, c("province", "city")])[!unique(partici_rate10$city) %in% citydata_10$city[-1],] %>%
  write.xlsx(., "data/lost_citys10.xlsx")

##
citydata_10 <- citydata_10 %>%
  mutate(private_worker = `城镇私营和个体从业人员_全市`/`年末单位从业人员数_全市`,
         industry_local = `规模以上工业产值_内资企业_全市`/`规模以上工业总产值(当年价万元)_全市`, 
         industry_foreign = (`规模以上工业产值_港澳台商投资企业_全市` + `规模以上工业产值_外商投资企业_全市`)/`规模以上工业总产值(当年价万元)_全市`,
         avgsalary = `职工平均工资_全市(元)`,
         gdp_growth = `地区生产总值增长率_全市(%)`,
         gdp_prop3 = `第三产业占GDP的比重_全市`,
         productwork_3 = `第三产业从业人员比重_全市`,
         gdp_percap = `人均地区生产总值_全市(元)`,
         edu_goven = `教育支出`/`地方财政一般预算内支出(万元)`)

partici_rate10 %>%
  filter(sex %in% c("女")) %>%
  left_join(., citydata_10 %>% 
              select(city, private_worker, industry_local, industry_foreign, edu_goven, avgsalary, productwork_3, gdp_growth, gdp_prop3, gdp_percap))
  left_join(citydata_10, by = c("city" = "city")) %>%
  ggplot(data= ., aes(x = industry_foreign, y = partici_rate)) +
  geom_point()

citydata_10$规模以上工业产值_港澳台商投资企业_全市[is.na(citydata_10$规模以上工业产值_港澳台商投资企业_全市)] <- 0
citydata_10$规模以上工业产值_外商投资企业_全市[is.na(citydata_10$规模以上工业产值_外商投资企业_全市)] <- 0

## proportion of industry 2010
data1 = read.xlsx("data/表6 各行业门类人口及三次产业人口比重(长表数据).xlsx", sheet = 1)
data1 = data1[-(1:2), c(1, (ncol(data1)-2):ncol(data1))] 
names(data1) = c("city", paste0("第", c("一", "二", "三"), "产业人口比重"))
data1[, 2:4] = sapply(data1[ , 2:4], FUN = function(x){as.numeric(x)/100})
data1$city = trimws(data1$city)
data1$city[data1$city=="自治区直辖县级行政区划"] = "新疆自治区直辖县级行政区划"
which(apply(data1[, 2:4], 1, sum) > 1)
worker_prop = left_join(unique(partici_rate10[, c("province", "city")]),
                        data1, by = c("city" = "city"))
worker_prop[is.na(worker_prop[,5]), ]

# industry ---------------------------------------

# 1990 
names(lfpr_tfr_90)
lfpr_tfr_90 = lfpr_tfr_90 %>%
  left_join(., industry90)

# 2000 
names(lfpr_tfr_00) 
#lfpr_tfr_00 = lfpr_tfr_00[, -grep("agri", names(lfpr_tfr_00))]
lfpr_tfr_00 = lfpr_tfr_00 %>%
  left_join(., non_agri00) %>% 
  left_join(subset(industry00_ipums, INDGEN == 10)[, c("name_harmonized", "prop_f")] %>% 
              transmute(name_harmonized, nonagri_f = 1-prop_f))

# 2010 
names(lfpr_tfr_10)
#lfpr_tfr_10 = lfpr_tfr_10[, -grep("agri", names(lfpr_tfr_10))]
lfpr_tfr_10 = lfpr_tfr_10 %>%
  left_join(., non_agri10 %>%
              pivot_wider(names_from = "sex", values_from = "nonagri") %>%
              select(name_harmonized, nonagri = `合计`, nonagri_f = f))

# join
names(lfpr_tfr_10);dim(lfpr_tfr_10)
lfpr_tfr_10 = lfpr_tfr_10[, -26:-34]
lfpr_tfr_10 = left_join(lfpr_tfr_10, econom10)

names(lfpr_tfr_00);dim(lfpr_tfr_00)
lfpr_tfr_00 = lfpr_tfr_00[, -25:-31]
lfpr_tfr_00 = left_join(lfpr_tfr_00, econom00)

names(lfpr_tfr_90);dim(lfpr_tfr_90)
lfpr_tfr_00 = lfpr_tfr_00[, -15:-21]
lfpr_tfr_90 = left_join(lfpr_tfr_90, econom90)

# 先验证基础的几个假设：教育程度占比，非农业从业者占比，
# 判断因果方向
# 用生育率预测的劳动参与率，加上其它变量，拟合对生育率的影响
# 用劳动参与率预测的生育率，
# 经济发展和生育率的交互作用，生育率本身是促进作用
unique(lfpr_tfr_00$region.y)
lfpr_tfr_10 %>% 
  filter(sex %in% c("f")) %>% 
  ggplot() +
  geom_boxplot(aes(y = lfpr, x = region.y, 
                   color = region.y), show.legend = FALSE) +
  theme_bw() +
  labs(x = NULL,
       y = "劳动参与率") +
  theme_bw() + 
  theme(text = element_text(family = "stsong", size = 18),
        panel.grid = element_blank())


subset(lfpr_tfr_10, sex == "f" & lfpr<.5 & compulsary_edu>0.7)

hist(subset(na.omit(lfpr_tfr_10), sex == "f")$compulsary_edu)

cor.test(filter(lfpr_tfr_00, sex %in% c("f") & region.y%in%c("东部"))[,c("tfr", "lfpr")])

as.dist(cor(subset(na.omit(lfpr_tfr_10),sex == "f" & !province %in% c("西藏","青海"))
            [, c("lfpr", "tfr", "prop_minor", "lowhigh_edu_f", "middle_high_edu")]))

as.dist(cor(subset(na.omit(lfpr_tfr_10),sex == "f")
            [, c("lfpr", "tfr", "primary_edu", "compulsary_edu", "middle_high_edu", "gdp_percap", 
                 "gdp23_prop", "nonagri", "nonagri_f","prop_minor", "housep")]))

# description -------------------------------------------------------
names(econom00)
econom10 %>%
  filter(!is.na(gdp3_prop)) %>% 
  summarise(mean = mean(gdp3_prop), sd = sd(gdp3_prop),
            min = min(gdp3_prop), max = max(gdp3_prop))
subset(econom00, is.na(gdp_percap))
subset(yearbook_00, is.na(`人均GRP`))
write.csv("data/describe_90.csv", row.names = T)

