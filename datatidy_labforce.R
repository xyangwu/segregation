library(dplyr)
library(tidyr)
library(ipumsr)
library(openxlsx)
library(ggplot2)
library(showtext)
library(RColorBrewer)

options(scipen = 999) 

# read in data -------------------------------------------------------------------

# codes and lables of occupation 
#occcode_00 = read.xlsx("C:/Users/WXY/Documents/R_learning/segregation/data/ipumsi_00001/ipumsi_china2000_occ.xlsx", sheet = 1)
#CN1990A_OCC = read.xlsx("C:/Users/WXY/Documents/R_learning/segregation/data/CN1990A_OCC.xlsx", sheet = 1)
occ_code00 = read.xlsx("data/occupation_code.xlsx", sheet = "2000")
occ_code90 = read.xlsx("data/occupation_code.xlsx", sheet = "1990")
ind_code00 = read.xlsx("data/occupation_code.xlsx", sheet = "2000_ind")
ind_code10 = read.xlsx("data/occupation_code.xlsx", sheet = "2010_ind")

# city names 
city_name00 = read.xlsx("data/cityname_00.xlsx", sheet = 1)
city_name90 = read.xlsx("data/cityname_90.xlsx", sheet = 1)

which(!city_name00$name_harmonized %in% city_name90$name_harmonized) %>% city_name00$name_harmonized[.]

# fertility and lfp in 2010
getSheetNames('data/lfp_tfr.xlsx')

lfp_10 = read.xlsx('data/lfp_tfr.xlsx', sheet = "LF10")[, 1:12] 
lfp_10[, c("就业人口_正在工作", "就业人口_暂未工作", "16岁及以上人口")] = apply(lfp_10[, c("就业人口_正在工作", "就业人口_暂未工作", "16岁及以上人口")], 2, as.numeric)
lfp_10 = lfp_10 %>% filter(grepl("  ", city) == FALSE, !city%in%c("total", "西南中沙群岛", "*长白山管委会"))

tfr_10 = read.xlsx('data/lfp_tfr.xlsx', sheet = "TFR10")
tfr_10[, 3:10] = apply(tfr_10[, 3:10], 2, as.numeric)
tfr_10[, 3:10] = apply(tfr_10[, 3:10], 2, function(x)x/1000)
tfr_10 = tfr_10[!tfr_10$city %in% c("西南中沙群岛", "*长白山管委会"), ]
tfr_10[is.na(tfr_10)] = 0

# data1, data2, data3 are census data in 2000
# data2 <- cbind(data2, data3[, -1], data5)
# data2 <- cbind(data2, data5)
data_00 <- readRDS("data/data2000.rds") 
data_90 <- readRDS("data/data1990.rds") 

## [!NOT RUN]
# GEO2_CN = ipums_val_labels(data2$GEO2_CN) %>% 
#   mutate(val = as.numeric(paste0(substr(val, 5, 6), substr(val, 8, 9)))) %>% 
#   left_join(., city_name00[, c('code', 'name_cn10')], by = c('val' = 'code')) 
# 
# GEO2_CN2000 = ipums_val_labels(data2$GEO2_CN2000) %>%
#   left_join(., city_name00[, c('val', 'name_cn10')], by = c('val' = 'val'))
# citys 城市名称列表
# write.xlsx(GEO2_CN, "data/ipumsi_00001/GEO2_CN.xlsx")

# tidy data -------------------------------------------------------------------

data_00 = data_00 %>% 
  mutate(age = 2000-BIRTHYR,
         agegroup = cut(age, breaks = c(seq(0, 101, 5)[-21], 101), right = FALSE, include.lowest = TRUE),
         sex = ifelse(SEX == 1, "m", "f"))

# 2000
# city name harmonized 
data_00$GEO2_CN2000 = haven::zap_labels(data_00$GEO2_CN2000)
grep("name_harmonized|name_cn00", names(data_00))
data_00 = data_00[, -c(grep("name_harmonized|name_cn00", names(data_00)))]

data_00 = data_00 %>% 
  left_join(., city_name00[, c("GEO2_CN2000", "name_cn00", "name_harmonized")], by = c("GEO2_CN2000" = "GEO2_CN2000"))

# 1990
data_90$GEO2_CN1990 = haven::zap_labels(data_90$GEO2_CN1990)
names(data_90)
grep("name_harmonized|name_cn90", names(data_90))
#data_90 = data_90[, -c(grep("name_harmonized|name_cn90", names(data_90)))]
data_90 = data_90 %>% 
  left_join(., city_name90[, c("GEO2_CN1990", "name_cn90", "name_harmonized")], by = c("GEO2_CN1990" = "GEO2_CN1990"))

# population by age 2000 -------------------------------------------------------------------

## employed population: age over 16 years old, employment status

## age structure: harmonized regions
agegroup_00_hmz <- data_00 %>% 
  zap_ipums_attributes(.) %>% # remove labels
  mutate(n = 1) %>% 
  aggregate(n ~ GEO2_CN + SEX + agegroup, data = ., FUN =  length) %>%
  mutate(GEO2_CN = as.numeric(paste0(substr(GEO2_CN, 5, 6), substr(GEO2_CN, 8, 9)))) %>%
  left_join(., city_name00[, c('code', 'name_cn10')], by = c('GEO2_CN' = 'code'))

agegroup_00_hmz$GEO2_CN[which(is.na(agegroup_00_hmz$name_cn10))] %>% unique(.) # check NA

## age structure: non-harmonized regions
agegroup_00 <- data_00 %>% 
  zap_ipums_attributes(.) %>% 
  mutate(n = 1) %>% 
  aggregate(n ~ GEO2_CN2000 + SEX + agegroup, data = ., FUN =  length) %>%
  mutate(SEX = ifelse(SEX == 1, 'm', ifelse(SEX == 2, 'f', SEX))) %>%
  left_join(., city_name00[, c('val', 'name_cn10', 'province')], by = c('GEO2_CN2000' = 'GEO2_CN2000')) %>%
  arrange(province, name_cn10)

agegroup_00$GEO2_CN2000[which(is.na(agegroup_00$name_cn10))] %>% unique(.) # check NA

## test if the census sample is representative 
## compare age structure with data from census `short table`
agegroup_00 %>%
  group_by(province, agegroup) %>%
  summarise(pop = sum(n)) %>%
  filter(province == '安徽') %>%
  mutate(prop = pop/565581)

## population pyramid
showtext_auto()
agegroup_00 %>% 
  ggplot(data = ., aes(x = agegroup, y = n, fill = factor(SEX))) +
  geom_bar(data = . %>% filter(SEX == 2), # female
           stat = "identity",
           position = "identity") +
  geom_bar(data = . %>% filter(SEX == 1), # male
           stat = "identity",
           position = "identity",
           mapping = aes(y = -n)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-800000, 800000, 200000), labels = abs(seq(-800000, 800000, 200000))) +
  annotate('text', x = c('[95-101]'), y = c(200000, -200000), label= c('女(5759928)', '男(6044416)'),
           size = 4.5, family = "stzhongs") +
  scale_fill_manual(labels = c("Male", "Female"),
                    values = c(brewer.pal(6, "Blues")[3],
                               brewer.pal(7, "Oranges")[3])
                    ) +
  labs(title = "分年龄和性别程度的人口分布",
       subtitle = '16-65岁(n = 8058856)',
       x = "",
       y = "") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15, family = "stsong",face = "bold"),
    plot.subtitle = element_text(size = 12, family = "stsong",face = "bold"),
    legend.spacing.x  = unit(0.2,'cm'),
    legend.spacing.y = unit(0,'cm'),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(margin = margin(r = 10), size = rel(1))
  )

# education in 2010 ------------------------------------------------------------

edulevel_10 = read.xlsx('data/census_edu.xlsx', sheet = 1)
edulevel_10 = edulevel_10[!edulevel_10$city %in% c("*长白山管委会"),]

city_name00$name_cn10[!city_name00$name_cn10 %in% edulevel_10$city]

edulevel_10 = edulevel_10 %>%
  filter(!grepl("  |全省|总计|合计", city)) %>%
  left_join(., unique(city_name00[, c("name_cn10", "name_harmonized")]), 
            by = c("city" = "name_cn10"))

which(is.na(edulevel_10$name_harmonized)) %>% edulevel_10$city[.]

edulevel_10[is.na(edulevel_10)] = 0

edu10 =  edulevel_10 %>% 
  group_by(name_harmonized, 教育程度) %>% 
  summarise(f = sum(女, na.rm = T), m = sum(男, na.rm = T), total = sum(总计, na.rm = T)) %>%
  ungroup() %>%
  # each education level comes with schooling years 
  mutate(edulevel = 教育程度,
         eduyear = ifelse(edulevel == "未上过学", 0,
                          ifelse(edulevel == "小学", 6,
                                     ifelse(edulevel == "初中", 9,
                                 ifelse(edulevel == "高中", 12,
                                            ifelse(edulevel %in% c("大学专科","大学本科","研究生"), 16, NA)
                                            ))))) %>%
  group_by(name_harmonized) %>% 
  # compute proportion of educational levels in population
  summarise(primary_edu = total[edulevel %in% c("小学")]/sum(total[!is.na(eduyear)]),
            junior_edu = total[edulevel %in% c("初中")]/sum(total[!is.na(eduyear)]),
            senior_edu = total[edulevel %in% c("高中")]/sum(total[!is.na(eduyear)]),
            high_edu = sum(total[edulevel %in% c("大学专科","大学本科","研究生")])/sum(total[!is.na(eduyear)]),
            eduyear_mean = sum(total*eduyear, na.rm = T)/sum(total[!is.na(eduyear)]),
            primary_edu_f = f[edulevel %in% c("小学")]/sum(f[!is.na(eduyear)]),
            junior_edu_f = f[edulevel %in% c("初中")]/sum(f[!is.na(eduyear)]),
            senior_edu_f = f[edulevel %in% c("高中")]/sum(f[!is.na(eduyear)]),
            high_edu_f = sum(f[edulevel %in% c("大学专科","大学本科","研究生")])/sum(f[!is.na(eduyear)]),
            eduyear_mean_f = sum(f*eduyear, na.rm = T)/sum(f[!is.na(eduyear)]),
            primary_edu_m = m[edulevel %in% c("小学")]/sum(m[!is.na(eduyear)]),
            junior_edu_m = m[edulevel %in% c("初中")]/sum(m[!is.na(eduyear)]),
            senior_edu_m = m[edulevel %in% c("高中")]/sum(m[!is.na(eduyear)]),
            high_edu_m = sum(m[edulevel %in% c("大学专科","大学本科","研究生")])/sum(m[!is.na(eduyear)]),
            eduyear_mean_m = sum(m*eduyear, na.rm = T)/sum(m[!is.na(eduyear)])
  ) 

write.csv(edu10, "data/edu10.csv", row.names = FALSE)
psych::describe(edu90$eduyear_mean)

# education in 2000 ------------------------------------------------------------

# compute using aggregated census data
edulevel_00 = read.xlsx('data/census_edu.xlsx', sheet = "edu00")
edulevel_00 = edulevel_00[!is.na(edulevel_00$city),]
edulevel_00[is.na(edulevel_00)] = 0 

edu00 = edulevel_00 %>% 
  left_join(., unique(city_name00[, c("name_cn00", "name_harmonized")]), by = c("city" = "name_cn00")) %>% 
  filter(!is.na(name_harmonized)) %>%
  pivot_longer(cols = 3:11, names_to = "edulevel", values_to = "pop") %>% 
  pivot_wider(names_from = sex, values_from = pop) %>%
  group_by(name_harmonized, edulevel) %>%
  summarise(m = sum(m), f = sum(f), total = m+f) %>%
  mutate(eduyear = ifelse(edulevel%in%c("未上过学","扫盲班"), 0,
                             ifelse(edulevel == "小学", 6,
                                        ifelse(edulevel == "初中", 9,
                                                   ifelse(edulevel %in%c("高中","中专"), 12,
                                                              ifelse(edulevel %in% c("大学专科","大学本科","研究生"), 16, NA)
                                                   ))))) %>%
  group_by(name_harmonized) %>%
  summarise(primary_edu = total[edulevel %in% c("小学")]/sum(total),
            junior_edu = total[edulevel %in% c("初中")]/sum(total),
            senior_edu = total[edulevel %in% c("高中","中专")]/sum(total),
            high_edu = sum(total[edulevel %in% c("大学专科","大学本科","研究生")])/sum(total),
            eduyear_mean = sum(total*eduyear, na.rm = T)/sum(total),
            # female
            primary_edu_f = f[edulevel %in% c("小学")]/sum(f),
            junior_edu_f = f[edulevel %in% c("初中")]/sum(f),
            senior_edu_f = f[edulevel %in% c("高中","中专")]/sum(f),
            high_edu_f = sum(f[edulevel %in% c("大学专科","大学本科","研究生")])/sum(f),
            eduyear_mean_f = sum(f*eduyear, na.rm = T)/sum(f)
  ) 

which(!edu00$name_harmonized %in% edu10$name_harmonized) %>% edu00$name_harmonized[.]

edu00 = edu00[!duplicated(edu00$name_harmonized), ] 

## compute using IPUMS data

data_00$edu
data_00 = data_00 %>% 
  mutate(edulevel = case_when(EDUCCN %in% c(0) ~ "未上过学",
                               EDUCCN %in% c(10:13, 19) ~ "小学",
                               EDUCCN %in% c(20:24, 29) ~ "初中",
                               EDUCCN %in% c(30:39, 29) ~ "高中",
                               EDUCCN %in% c(41:42, 51, 58, 61) ~ "大学专科",
                               EDUCCN %in% c(40, 43:44, 50, 52, 60, 62) ~ "大学本科",
                               TRUE ~ "NIU"
                               ))

edu00_ipums = subset(data_00, age >= 6 & EDUCCN != 99) %>%
  count(name_harmonized, sex, edulevel) %>% 
  pivot_wider(names_from = "sex", values_from = "n", values_fill = list(n = 0)) %>%
  mutate(total = f + m, 
         eduyear= ifelse(edulevel == "未上过学", 0,
                             ifelse(edulevel == "小学", 6,
                                        ifelse(edulevel == "初中", 9,
                                                   ifelse(edulevel == "高中", 12,
                                                              ifelse(edulevel %in% c("大学专科","大学本科","研究生"), 16, NA)
                                                   ))))) %>%
  group_by(name_harmonized) %>%
  summarise(primary_edu = total[edulevel %in% c("小学")]/sum(total[!is.na(eduyear)]),
             junior_edu = total[edulevel %in% c("初中")]/sum(total[!is.na(eduyear)]),
             senior_edu = total[edulevel %in% c("高中","中专")]/sum(total[!is.na(eduyear)]),
             high_edu = sum(total[edulevel %in% c("大学专科","大学本科","研究生")])/sum(total[!is.na(eduyear)]),
             eduyear_mean = sum(total*eduyear, na.rm = T)/sum(total[!is.na(eduyear)]),
             # female 
             primary_edu_f = f[edulevel %in% c("小学")]/sum(f[!is.na(eduyear)]),
             junior_edu_f = f[edulevel %in% c("初中")]/sum(f[!is.na(eduyear)]),
             senior_edu_f = f[edulevel %in% c("高中","中专")]/sum(f[!is.na(eduyear)]),
             high_edu_f = sum(f[edulevel %in% c("大学专科","大学本科","研究生")])/sum(f[!is.na(eduyear)]),
             eduyear_mean_f = sum(f*eduyear, na.rm = T)/sum(f[!is.na(eduyear)])
  ) 

range(edu00_ipums$eduyear_mean)
plot(edu00$eduyear_mean_f, edu00_ipums$eduyear_mean_f)

write.csv(edu00, "data/edu00.csv", row.names = FALSE)

## education in 1990 ------------------------------------------------------------

ipums_val_labels(data_90$CN1990A_EDLEV1)
table(data_90$CN1990A_EDLEV1)

data_90 = data_90 %>% 
  mutate(edulevel = case_when(CN1990A_EDLEV1 %in% c(1) ~ "未上过学",
                               CN1990A_EDLEV1 %in% c(2) ~ "小学",
                               CN1990A_EDLEV1 %in% c(3) ~ "初中",
                               CN1990A_EDLEV1 %in% c(4:5) ~ "高中",
                               CN1990A_EDLEV1 %in% c(6) ~ "大学专科",
                               CN1990A_EDLEV1 %in% c(7) ~ "大学本科",
                               CN1990A_EDLEV1 %in% c(0) ~ "NIU")) 

edu_n90 = subset(data_90, age >= 6 & CN1990A_EDLEV1 !=0) %>%
  count(name_harmonized, sex, edulevel)

edu90 = edu_n90 %>% 
  pivot_wider(names_from = "sex", values_from = "n", values_fill = list(n = 0)) %>%
  mutate(total = f + m, 
         eduyear= ifelse(edulevel == "未上过学", 0,
                         ifelse(edulevel == "小学", 6,
                                ifelse(edulevel == "初中", 9,
                                       ifelse(edulevel == "高中", 12,
                                              ifelse(edulevel %in% c("大学专科","大学本科","研究生"), 16, NA)
                                       ))))) %>%
  group_by(name_harmonized) %>%
  summarise(primary_edu = total[edulevel %in% c("小学")]/sum(total),
            junior_edu = total[edulevel %in% c("初中")]/sum(total),
            senior_edu = total[edulevel %in% c("高中","中专")]/sum(total),
            high_edu = sum(total[edulevel %in% c("大学专科","大学本科","研究生")])/sum(total),
            eduyear_mean = sum(total*eduyear, na.rm = T)/sum(total),
            # female 
            primary_edu_f = f[edulevel %in% c("小学")]/sum(f),
            junior_edu_f = f[edulevel %in% c("初中")]/sum(f),
            senior_edu_f = f[edulevel %in% c("高中","中专")]/sum(f),
            high_edu_f = sum(f[edulevel %in% c("大学专科","大学本科","研究生")])/sum(f),
            eduyear_mean_f = sum(f*eduyear, na.rm = T)/sum(f)
  )

write.csv(edu90, "data/edu90.csv", row.names = FALSE)

# industry 1990 ---------------------------------------
# non-agriculture industry 

ipumsr::ipums_val_labels(data_90$INDGEN)
ind90 = data_90 %>%  
  zap_ipums_attributes(.) %>%
  filter(age >= 15, EMPSTAT %in% c(1)) %>% 
  count(name_harmonized, INDGEN, sex, name = "n") %>% 
  pivot_wider(names_from = "sex", values_from = "n", values_fill = list(n = 0)) %>%
  mutate(total = f + m) %>% 
  group_by(name_harmonized) %>% 
  mutate(prop_f = f/sum(f), prop_m = m/sum(m), prop_t = total/sum(total)) %>% 
  ungroup() %>%
  mutate(industry = case_when(INDGEN %in% c(10) ~ "ind1",
                              INDGEN %in% c(20,30,40,50) ~ "ind2",
                              INDGEN %in% c(seq(60, 130, 10), 111:114, 999) ~ "ind3"))

skillcode90 = c(68,70,81:93)
ind90_2 = ind90 %>% 
  group_by(name_harmonized) %>% 
  summarise(ind1 = sum(prop_t[industry == "ind1"]),
            ind2 = sum(prop_t[industry == "ind2"]),
            ind3 = sum(prop_t[industry == "ind3"]),
            ind1_f = sum(prop_f[industry == "ind1"]),
            ind2_f = sum(prop_f[industry == "ind2"]),
            ind3_f = sum(prop_f[industry == "ind3"]),
            indskill = sum(prop_t[INDGEN %in% skillcode90])
            ) %>%
  ungroup()

# industry 2000 ---------------------------------------

# using IPUMS data 
head(data_00$IND) 
ipums_val_labels(data_00$IND)

ind00_ipums = data_00 %>% 
  zap_ipums_attributes(.) %>% 
  filter(age >= 15, CN2000A_WORK %in% c(1, 2)) %>% 
  count(name_harmonized, INDGEN, sex, name = "n") %>% 
  pivot_wider(names_from = "sex", values_from = "n", values_fill = list(n = 0)) %>%
  mutate(total = f + m) %>% 
  group_by(name_harmonized) %>% 
  mutate(prop_f = f/sum(f), prop_m = m/sum(m), prop_t = total/sum(total)) %>% 
  ungroup()

ind00_ipums2 = ind00_ipums %>%
  group_by(name_harmonized) %>% 
  summarise(ind1 = sum(prop_t[INDGEN %in% c(10)]),
          ind2 = sum(prop_t[INDGEN %in% c(20,30,40,50)]),
          ind3 = sum(prop_t[INDGEN %in% c(seq(60, 130, 10), 111:114, 999)]),
          ind1_f = sum(prop_f[INDGEN %in% c(10)]),
          ind2_f = sum(prop_f[INDGEN %in% c(20,30,40,50)]),
          ind3_f = sum(prop_f[INDGEN %in% c(seq(60, 130, 10), 111:114, 999)])
          ) %>% ungroup()

# using aggregated data from NBS
lf_00 = read.xlsx("data/occupation_city.xlsx", sheet = "行业00")
lf_00[is.na(lf_00)] = 0 
names(lf_00)

skillcode = c(7,10,13,16:18) 
ind00 = lf_00 %>% 
  # pivot_longer(cols = 2:ncol(.), names_to = "ind_class1", values_to = "n") %>%
  left_join(., unique(city_name00[, c("name_cn00", "name_harmonized")]), by = c("city" = "name_cn00")) %>%
  filter(!is.na(name_harmonized)) %>% 
  group_by(name_harmonized) %>% 
  summarise(pop_resid = sum(`人口`)*0.1, 
            lfage = sum(`15岁及以上人口`)/pop_resid, 
            lfp = sum(total)+sum(从未工作正在寻找工作)+sum(失去工作正在寻找工作),
            lfsize = lfp/pop_resid,
            unemp = 1-sum(total)/lfp, 
            lfpr00 = lfp/sum(`15岁及以上人口`),
            ind2 = sum(industry_2)/sum(total),
            ind3 = sum(industry_3)/sum(total),
            ind1 = 1-ind2-ind3, 
            indskill = sum(indskill)/sum(total)) %>% 
  ungroup()

# compare the proportion computed using diffrent data
# trivial diffrence, but it'd be better to adjust values of '大兴安岭地区', '伊春市'
par(mfrow = c(1,1)) 
plot(1-subset(ind00_ipums, INDGEN == 10)$prop,
     non_agri00$nonagri, 
     xlab = "ipums", ylab = "aggregated")
cor.test(1-subset(ind00_ipums, INDGEN == 10)$prop, non_agri00$nonagri) 
which(abs(1-subset(ind00_ipums, INDGEN == 10)$prop- non_agri00$nonagri)  > 0.04) %>% non_agri00$name_harmonized[.]

# industry 2010 ------------------------------------------------------------------

# note: 海南，广东, 广西, 山东细分行业数据有缺失，不影响分析 
getSheetNames("data/occupation_city.xlsx") 
ind10 = read.xlsx("data/occupation_city.xlsx", sheet = "行业10")
# transform to numeric type 
ind10[, 4:ncol(ind10)] = apply(ind10[, 4:ncol(ind10)], 2, as.numeric)
ind10[is.na(ind10)] = 0 

ind10 = ind10 %>% 
  left_join(., unique(lfp_10[, c("city", "name_harmonized")])) %>%
  filter(!is.na(name_harmonized))

# select main industry
ind10_class1 = ind10 %>% 
  select(c(1:3), name_harmonized, grep("一|二|三|四|五|六|七|八|九|十", colnames(.))) %>% 
  pivot_longer(cols = 5:ncol(.), names_to = "ind_class1", values_to = "n") %>%
  left_join(., ind_code10[, c("ind_class1","code_class1")]) %>%
  group_by(name_harmonized, code_class1, sex) %>% 
  summarise(n = sum(n)) %>% 
  pivot_wider(names_from = "sex", values_from = "n", values_fill = list(n = 0)) 

# skilled industry 
skillcode = c(7,10,13,16:18)
ind10_2 = ind10_class1 %>% 
  group_by(name_harmonized) %>%  
  summarise(ind1 = sum(total[code_class1 %in% c(1)])/sum(total),
            ind2 = sum(total[code_class1 %in% c(2:5)])/sum(total),
            ind3 = sum(total[code_class1 %in% c(6:20)])/sum(total),
            ind1_f = sum(f[code_class1 %in% c(1)])/sum(f),
            ind2_f = sum(f[code_class1 %in% c(2:5)])/sum(f),
            ind3_f = sum(f[code_class1 %in% c(6:20)])/sum(f),
            indskill = sum(total[code_class1 %in% skillcode])/sum(total),
            indunskill = ind3-indskill,
            ind3_skill = indskill/ind3,
            ind3_unskill = 1-ind3_skill) %>% 
  ungroup() 

plot(ind10_2$indunskill, ind10_2$indskill)
ind10_2[ind10_2$indunskill > 0.9,]
plot(ind10_2$indunskill+ind10_2$indskill, ind10_2$ind1)

ind10 = ind10 %>% 
  mutate(industry = case_when(ind_class1 %in% c("一、农、林、牧、渔业") ~ 10,
                              ind_class1 %in% c("二、采矿业") ~ 20,
                              ind_class1 %in% c("三、制造业") ~ 30,
                              ind_class1 %in% c("四、电力、燃气及水的生产和供应业") ~ 40,
                              ind_class1 %in% c("五、建筑业") ~ 50,
                              ind_class1 %in% c("八、批发和零售业") ~ 60,
                              ind_class1 %in% c("九、住宿和餐饮业") ~ 70,
                              ind_class1 %in% c("六、交通运输、仓储和邮政业") ~ 80,
                              ind_class1 %in% c("十、金融业") ~ 90,
                              ind_class1 %in% c("十九、公共管理和社会组织") ~ 100,
                              ind_class1 %in% c("十二、租赁和商务服务业") ~ 111,
                              ind_class1 %in% c("十一、房地产业") ~ 111,
                              ind_class1 %in% c("十三、科学研究、技术服务和地质勘查业") ~ 114, # research
                              ind_class1 %in% c("十四、水利、环境和公共设施管理业") ~ 100,
                              ind_class1 %in% c("十五、居民服务和其他服务业") ~ 120,
                              ind_class1 %in% c("十六、教育") ~ 112, 
                              ind_class1 %in% c("十七、卫生、社会保障和社会福利业") ~ 113,
                              ind_class1 %in% c("七、信息传输、计算机服务和软件业") ~ 114,
                              ind_class1 %in% c("十八、文化、体育和娱乐业") ~ 114, 
                              ind_class1 %in% c("二十、国际组织") ~ 130)
         )

head(ind10)

which(is.na(ind10$ind1_f)) %>% ind10[.,]
which(apply(ind10[, 2:4], 1, sum) !=1) %>% ind10[.,]

# Reference
# 第六次全国人口普查表填写说明: http://www.stats.gov.cn/tjsj/pcsj/rkpc/6rp/html/fu09.htm

# ethnic & urbanization ---------------------------------------------------------

# 2010 

pop10 = read.xlsx("data/ethnic_urban.xlsx", sheet = "10") 
pop10[is.na(pop10)] = 0 

names(pop10)
ethurb10 = pop10 %>% 
  filter(city %in% lfp_10$city) %>% 
  left_join(., unique(lfp_10[, c("city", "name_harmonized")])) %>%
  group_by(name_harmonized) %>%
  summarise(pop_local = sum(`户籍人口`), 
            urban = sum(`非农业户口人口比重（%）`*`户籍人口`/100),
            minor = sum(`少数民族人口比重（%）`*`户籍人口`/100), 
            prop_urban = urban/pop_local, 
            prop_minor = minor/pop_local,
            extend_fam = 1-(sum(家庭户类别一代户)/sum(家庭户户数))) %>% 
  ungroup()

# 2000

pop00 = read.xlsx("data/ethnic_urban.xlsx", sheet = "00")
pop00[, 2:ncol(pop00)] = apply(pop00[, 2:ncol(pop00)], 2, as.numeric)
pop00[is.na(pop00)] = 0
pop00 = pop00 %>% 
  filter(city %in% city_name00$name_cn00) %>% 
  left_join(., city_name00[, c("name_cn00", "name_harmonized")], by = c("city" = "name_cn00")) 

ethurb00 = pop00 %>%  
  group_by(name_harmonized) %>%
  summarise(pop_resid = sum(合计), 
            pop_local = sum(`户籍人口`), 
            urban = sum(`非农业户口人口比重（%）`*`户籍人口`/100),
            minor = sum(`少数民族人口比重（%）`*`户籍人口`/100), 
            prop_urban = urban/pop_local, 
            prop_minor = minor/pop_local, 
            extend_fam = 1-(sum(家庭户类别一代户)/sum(家庭户户数))) %>% 
  ungroup()

# 1990

ethnic90 = haven::zap_labels(data_90) %>% 
  count(name_harmonized, ETHNICCN) %>% 
  group_by(name_harmonized) %>%
  mutate(prop_ethnic = n/sum(n)) %>% 
  summarise(prop_minor = sum(prop_ethnic[ETHNICCN != 1])) %>%
  ungroup() 
ethnic90$prop_minor[ethnic90$name_harmonized == "鄂州市"] = 0.001
ethnic90$prop_minor[ethnic90$name_harmonized == "朔州市"] = 0.001

ethurb10[ethurb10$prop_minor-ethurb00$prop_minor > .05,]
ethnic90[ethurb00$prop_minor-ethnic90$prop_minor > .2,]

# "新疆自治区直辖县级行政区划" only includes "石河子" before 2000, so minor ethnics' population varys considerably due to citys included later.

urban90 = data_90 %>% 
  zap_ipums_attributes() %>% 
  count(name_harmonized, CN1990A_HHTYAP) %>%
  group_by(name_harmonized) %>%
  mutate(prop = n/sum(n)) %>% 
  summarise(prop_urban = sum(prop[CN1990A_HHTYAP != 1])) %>%
  ungroup() 

ethurb90 = urban90 %>% 
  left_join(., ethnic90)

# economic ------------------------------------------------------------------------------------
# GDP deflator base year 1990, 2000: 1.970, 2010:2.969

# 2010

yearbook_10 = read.xlsx("data/yearbook_new.xlsx", sheet = 1)
yearbook_10 = left_join(yearbook_10, unique(city_name00[, c("name_cn10", "name_harmonized")]), by = c("city" = "name_cn10"))
yearbook_10 = yearbook_10[!is.na(yearbook_10$name_harmonized),]
yearbook_10[is.na(yearbook_10)]
names(yearbook_10)
econom10 = yearbook_10 %>% 
  select(name_harmonized, 
         #fdi = `外商直接投资实际使用额(万美元)`,
         gdp = `GDP(亿元)`, pop = `常住人口(万人)`, gdp1 = `GDP(亿元)_第一产业`, gdp2 = `GDP(亿元)_第二产业`, gdp3 = `GDP(亿元)_第三产业`, 
         exp_gov = `地方财政一般预算支出(亿元)`, exp_edu = `地方财政一般预算支出(亿元)_教育`,
         housep = `商品房销售额(亿元)`, houseq = `商品房销售面积(万平方米)`
  ) %>% 
  group_by(name_harmonized) %>% 
  summarise(gdp = sum(gdp, na.rm = T), 
            pop = sum(pop, na.rm = T), 
            gdp_percap = gdp/pop, # in 10 thousand
            gdp2_prop = sum(gdp2, na.rm = T)/gdp, 
            gdp3_prop = sum(gdp3, na.rm = T)/gdp,
            housep = sum(housep, na.rm = T)/sum(houseq, na.rm = T),
            gdp_exp = sum(exp_gov, na.rm = T)/gdp,
            exp_edu = sum(exp_edu, na.rm = T)/sum(exp_gov, na.rm = T)
            #fdi = sum(fdi, na.rm = T)*7, fdi_prop = fdi/pop
  ) %>% 
  ungroup(.) 

# deflated based on 1990
econom10$gdpconst = econom10$gdp/2.969 # deflator 2.969
econom10$gdp_percapconst = econom10$gdp_percap/2.969 

econom10$name_harmonized[is.na(econom10$housep)]
subset(econom10, housep > 1 & !is.na(housep)) 
econom10$name_harmonized[is.na(econom10$gdp_percap)]
econom10[econom10$gdp_percap > 1.5, ]
econom10[econom10$gdp3_prop >1,]


psych::describe(econom10$exp_edu)

# 2000 

yearbook_00 = read.xlsx("data/yearbook_new.xlsx", sheet = "2000") 
yearbook_00 = left_join(yearbook_00, unique(city_name00[, c("name_cn00", "name_harmonized")]), by =c("city"="name_cn00"))
yearbook_00 = yearbook_00[!is.na(yearbook_00$name_harmonized),]
names(yearbook_00)
subset(yearbook_00, is.na(`人均GRP`))$name_harmonized

econom00 = yearbook_00 %>% 
  select(name_harmonized, 
         #fdi = `外商直接投资实际使用额(万美元)`,
         pop = `年底总人口(万人)`, 
         gdp = `GDP(亿元)`, grp = `人均GRP`,
         gdp1 = `GDP(亿元)_第一产业`, gdp2 = `GDP(亿元)_第二产业`, gdp3 = `GDP(亿元)_第三产业`, 
         exp_gov = `地方财政支出`, exp_edu = `地方财政支出_文教科卫`,
         housep = `商品房屋销售额(亿元)`, houseq = `商品房屋销售面积(万平方米)`) %>% 
  group_by(name_harmonized) %>% 
  summarise(gdp = sum(gdp, na.rm = T), 
            gdp_percap = gdp/sum(pop, na.rm = T),
            gdp_percap2 = sum(grp*pop)/sum(pop)/10000,
            gdp2_prop = sum(gdp2, na.rm = T)/gdp,
            gdp3_prop = sum(gdp3, na.rm = T)/gdp,
            housep = sum(housep, na.rm = T)/sum(houseq, na.rm = T),
            gdp_exp = sum(exp_gov, na.rm = T)/gdp,
            exp_edu = sum(exp_edu, na.rm = T)/sum(exp_gov, na.rm = T)
            #fdi = sum(fdi, na.rm = T)*7, fdi_prop = fdi/pop
  ) %>% 
  ungroup(.) 

plot(econom00$gdp_percap, econom00$gdp_percap2)
subset(econom00, abs(gdp_percap-gdp_percap2) > 0.3)[, 1:4]

# deflate 2000 based on 1990
econom00$gdpconst = econom00$gdp/1.970 # deflator 1.970
econom00$gdp_percapconst = econom00$gdp_percap2/1.970 

which(is.na(econom00$gdp_percapconst)) %>% econom00[.,]
psych::describe(econom00$gdp_percapconst)
subset(econom00, exp_edu != 0)$exp_edu %>% psych::describe(.)
subset(econom00,  gdp_percap2 < 0.2)
subset(econom00,  gdp2_prop < 0.2)
econom00 = subset(econom00, !name_harmonized %in% "海南省直辖县级行政区划")

## modified gdp in 2000 
yearbook_002 = read.xlsx("data/yearbook_new.xlsx", sheet = "00_cnki")
yearbook_002 = left_join(yearbook_002, city_name00[, c("name_cn10", "name_harmonized")], by =c("city"="name_cn10"))
yearbook_002 = yearbook_002[-which(duplicated(yearbook_002$city)), ]
yearbook_002[, 2:6] = apply(yearbook_002[, 2:6], 2, as.numeric)
yearbook_002 = yearbook_002 %>%
  group_by(name_harmonized) %>% 
  summarise(gdp = sum(`GDP(亿元)`, na.rm = T)*10000/1.970,
            gdp1 = sum(`第一产业增加值(亿元)`, na.rm = T)*10000/1.970)

which(abs(as.numeric(yearbook_00$`GDP(亿元).x`)-as.numeric(yearbook_00$`GDP(亿元).y`))/as.numeric(yearbook_00$`GDP(亿元).x`)>0.2) %>%
  yearbook_00$city[.]

which(is.na(yearbook_002$name_cn10)) %>% yearbook_002$city[.]
which(!city_name00$name_cn10 %in% yearbook_002$city) %>% city_name00$name_cn10[.]

# 1990

yearbook_90 = read.xlsx("data/yearbook_new.xlsx", sheet = "1990")
# yearbook_902 = read.xlsx("data/yearbook_new.xlsx", sheet = "城市年鉴90")
# yearbook_92 = read.xlsx("data/yearbook_new.xlsx", sheet = "1992")
# 
# yearbook_90$city2 = gsub("市|自治州|地区|藏|蒙古|族|回|傈僳", "", yearbook_90$city) 

# yearbook_92 = yearbook_92[!is.na(yearbook_92$city), ]
# which(!yearbook_92$city %in% yearbook_90$city) %>% yearbook_92$city[.]

# yearbook_90 = left_join(yearbook_90, yearbook_92[, c(2, 19:23)], by = c('city' = "city"))
# yearbook_90 = left_join(yearbook_90, yearbook_902[, c(1, 10:13)], by = c("city2" = "city"))

write.xlsx(yearbook_90, "data/yearbook_90.xlsx")

yearbook_90 = left_join(yearbook_90, unique(lfp_10[, c("city", "name_harmonized")]))
which(is.na(yearbook_90$name_harmonized)) %>% yearbook_90$city[.]
yearbook_90 = yearbook_90[!is.na(yearbook_90$name_harmonized), ]

## as numeric
yearbook_90[, 2:(ncol(yearbook_90)-1)] = apply(yearbook_90[, 2:(ncol(yearbook_90)-1)], 2, as.numeric)
econom90 = yearbook_90 %>% 
  select(name_harmonized, 
         #fdi = `外商直接投资实际使用额(万美元)`, 
         gdp = `GDP(亿元)`, pop = `常住人口`,
         gdp90 = `GDP(亿元)地区`,
         gdp1 = `GDP(亿元)_第一产业`,
         gdp2 = `GDP(亿元)_第二产业`,
         gdp3 = `GDP(亿元)_第三产业`,
         exp_gov = `地方财政预算内支出（万元）`,
         exp_edu = `教育事业费支出（万元）`) %>% 
  group_by(name_harmonized) %>% 
  summarise(gdp_percap = sum(gdp, na.rm = T)/sum(pop, na.rm = T),
            gdp2_prop = sum(gdp2, na.rm = T)/sum(gdp90, na.rm = T),
            gdp3_prop = sum(gdp3, na.rm = T)/sum(gdp90, na.rm = T),
            exp_edu = sum(exp_edu, na.rm = T)/sum(exp_gov, na.rm = T)
            #fdi = sum(fdi, na.rm = T)*7, fdi_prop = fdi/pop
  ) %>% 
  ungroup(.) 

subset(econom90, exp_edu < 0.05)
psych::describe(econom00$exp_edu, na.rm = T)

# fertility & lfp in 1990 ---------------------------------------------------------

##' EMPSTAT 
##' 0	NIU (not in universe), 1	Employed, 2	Unemployed, 3	Inactive, 9	Unknown/missing
##' LABFORCE: 
##' 1	No, not in the labor force, 2	Yes, in the labor force, 8	Unknown, 9	NIU (not in universe)

lf_90 = data_90 %>% 
  zap_ipums_attributes(.) %>%
  filter(age >=15) %>% 
  count(name_harmonized, LABFORCE, EMPSTAT) %>%
  group_by(name_harmonized) %>% 
  summarise(lfage = sum(n), 
            lfp = sum(n[LABFORCE %in% c(2)]),
            unemp = n[EMPSTAT %in% c(2)]/lfp) %>%
  ungroup() %>%
  left_join(., data_90 %>% count(name_harmonized, name = "pop"))

lf_90$lfsize = lf_90$lfp/lf_90$pop

lfpr_90 = data_90 %>% 
  zap_ipums_attributes(.) %>%
  filter(age >=15) %>% 
  count(name_harmonized, sex, LABFORCE) %>%
  group_by(name_harmonized, sex) %>% 
  mutate(lfpr = n/sum(n)) %>%
  ungroup()

tfr_90 = data_90 %>% 
  zap_ipums_attributes(.) %>%
  filter(sex == "f" & age >= 15 & age <= 49) %>%
  mutate(births = ifelse(BIRTHSLYR %in% c(0, 9), 0, BIRTHSLYR)) %>%
  group_by(name_harmonized, agegroup) %>% 
  summarise(n_female = n(),
            asf = sum(births),
            asfr = asf/n_female) %>% 
  ungroup() %>%
  select(name_harmonized, agegroup, asfr) %>% 
  pivot_wider(., names_from = "agegroup",
              values_from = "asfr")

## compute TFR //计算总和生育率 

tfr_90$tfr <- apply(tfr_90[, 2:8], 1, function(x)sum(x)*5)

## combine lfpr with tfr

lfpr_tfr_90 = lfpr_90 %>%
  left_join(., tfr_90[, c("name_harmonized", "tfr")],
            by = c('name_harmonized' = 'name_harmonized')) %>%
  left_join(.,
            unique(city_name90[, c("name_harmonized", "province", "region.x", "region.y")]), 
            by = c("name_harmonized" = "name_harmonized"))


write.csv(lfpr_tfr_90, "data/lfpr_tfr_90.csv", row.names = FALSE)

# fertility & lfp in 2000 ---------------------------------------------------------

## fertility by age //计算年龄别生育率 

tfr_00 <- data_00 %>% 
  haven::zap_formats() %>%
  filter(sex == "f" & age >= 15 & age <= 49) %>%
  mutate(births = ifelse(BIRTHSLYR == 1, 1, 0)) %>%
  group_by(name_harmonized, agegroup) %>%
  summarise(n_female = n(),
            asf = sum(births),
            asfr = asf/n_female) %>%
  ungroup() %>%
  select(name_harmonized, agegroup, asfr) %>%
  pivot_wider(., 
              names_from = "agegroup",
              values_from = "asfr") 

## compute TFR //计算总和生育率 

tfr_00$tfr <- apply(tfr_00[, 2:8], 1, function(x)sum(x)*5) 

# Labour Force
## Questions
##' CN2000A_WORK: whether or not the person worked for at least 1 hour with pay last week
##' 1	Yes
##' 2	No, due to training, vacation, or seasonal holidays
##' 3	No, for other reasons
##' 9	NIU (not in universe)
data2 %>% 
  zap_ipums_attributes(.) %>%
  filter(age >= 15) %>%
  count(CN2000A_WORK) %>%
  mutate(prop = n/sum(n))

## People who chose 3 in R17 (CN2000A_WORK), 
## persons who did't get employed but were actively looking for jobs chose:
##' 5	Had never worked before and is looking for a job
##' 6	Lost last job and is looking for a job
data2 %>% 
  filter(age >= 15) %>%
  count(CN2000A_ACTIVITY) %>%
  mutate(prop = n/sum(n)) 

(6447847+237318+131728+116501)/9062775 # LFPR of sample

# LFPR from census short table (not inluding the unemploymened)
66874889/90278554

# total labour force participate rate

lfpr_00 = haven::zap_labels(data_00) %>% 
  filter(age >=15) %>% 
  mutate(LABFORCE = ifelse(CN2000A_WORK %in% c(1, 2) | CN2000A_ACTIVITY %in% c(5, 6), 2, 1)) %>% 
  count(name_harmonized, sex, LABFORCE) %>%
  group_by(name_harmonized, sex) %>% 
  summarise(lf00 = sum(n), lfpr = n[LABFORCE==2]/lf00) %>% 
  ungroup() 

# combine lfpr with tfr
lfpr_tfr_00 = lfpr_00 %>% 
  left_join(., tfr_00[, c("name_harmonized", "tfr")],
            by = c('name_harmonized' = 'name_harmonized')) %>%
  left_join(., unique(city_name00[, c("name_harmonized", "province", "region.x", "region.y")]), 
            by = c("name_harmonized" = "name_harmonized"))

lfpr_tfr_00 = lfpr_tfr_00[lfpr_tfr_00$LABFORCE == 2, -3]
write.csv(lfpr_tfr_00, "data/lfpr_tfr_00.csv", row.names = FALSE)

lf_00ipums = haven::zap_labels(data_00) %>% 
  filter(age >=15) %>% 
  mutate(LABFORCE = ifelse(CN2000A_WORK %in% c(1, 2) | CN2000A_ACTIVITY %in% c(5, 6), 2, 1)) %>% 
  count(name_harmonized, LABFORCE) %>%
  group_by(name_harmonized) %>% 
  summarise(lfage = sum(n), 
            lfp = sum(n[LABFORCE %in% c(2)])) %>%
  ungroup() %>% 
  left_join(., data_00 %>% count(name_harmonized, name = "pop"))

lf_00ipums$lfsize = lf_00ipums$lfp/lf_00ipums$pop

# fertility & lfp in 2010 ---------------------------------------------
# harmonized

# age-speicific population, for re-compute tfr of harmonized regions
age_gender10 = read.xlsx("data/age_city.xlsx", sheet = "agegroup10")
age_gender10 = age_gender10 %>%
  filter(grepl("  |省$|自治区$|市辖区|县$", city) == FALSE, sex %in% c("f")) %>%
  select(-2)

tfr_10_long = tfr_10 %>%
  filter(grepl("  |total", city) == FALSE) %>% 
  select(1:9, 11) %>%
  pivot_longer(cols = 3:9, names_to = "agegroup", values_to = "asfr") %>%
  filter(agegroup %in% c("15-19岁", "20-24岁", "25-29岁", "30-34岁", "35-39岁", "40-44岁", "45-49岁"))

# age-specific births

age_gender10_long = age_gender10 %>%
  pivot_longer(cols = 2:20, names_to = "agegroup", values_to = "pop") %>%
  filter(agegroup %in% c("15-19岁", "20-24岁", "25-29岁", "30-34岁", "35-39岁", "40-44岁", "45-49岁")) %>%
  left_join(., tfr_10_long, by = c("city" = "city", "agegroup" = "agegroup")) %>%
  mutate(births = pop*asfr)

## use harmonized geographical regions 

tfr_10_hmnz = age_gender10_long %>% 
  select(name_harmonized, agegroup, births, pop) %>%
  group_by(name_harmonized, agegroup) %>%
  summarise(pop = sum(pop), births = sum(births)) %>%
  ungroup() %>% 
  transmute(name_harmonized, agegroup, asfr = births/pop) %>%
  pivot_wider(., 
              names_from = "agegroup",
              values_from = "asfr") 

## compute TFR //计算总和生育率 

tfr_10_hmnz$tfr <- apply(tfr_10_hmnz[, 2:8], 1, function(x)sum(x)*5)

# labor in 2010 

lfpr_10 = lfp_10 %>% 
  filter(grepl("  ", city) == FALSE, !city %in% c("total")) %>%
  group_by(name_harmonized, sex) %>% 
  summarise(pop_resid = sum(`总人口`),
            lfage = sum(`16岁及以上人口`)/pop_resid,
            lfsize = sum(`经济活动人口`)/pop_resid,
            lfpr = lfsize/lfage, 
            unemp = 1-sum(`就业人口_小计`)/sum(`经济活动人口`)) %>% 
  ungroup() 

lfpr_tfr_10 = lfpr_10 %>%
  left_join(., tfr_10_hmnz[, c("name_harmonized", "tfr")],
            by = c('name_harmonized' = 'name_harmonized')) %>%
  left_join(.,
            unique(city_name00[, c("name_harmonized", "province", "region.x", "region.y")]), 
            by = c("name_harmonized" = "name_harmonized"))

write.csv(lfpr_tfr_10, "data/lfpr_tfr_10.csv", row.names = FALSE)

# marriage --------------------------------------------------------------------------------------

marriage10 = read.xlsx("data/marriage_city.xlsx", sheet = "2010")

marriage10 %>% head()
marriage10 = rbind(marriage10[-1, c(1:6,12)], marriage10[-1, c(1,7:12)])
marriage10$sex = rep(c("m", "f"), each = nrow(marriage10)/2)
marriage10 = marriage10 %>%
  filter(grepl("  |省$|自治区$|市辖区|县$", city) == FALSE)

age10 = read.xlsx("data/age_city.xlsx", sheet = "agegroup10")

age10 = age10 %>%
  filter(grepl("  |省$|自治区$|市辖区|县$", city) == FALSE)
age10$childbear = apply(age10[, 7:13], 1, sum)

marriage10 = left_join(marriage10, age10[, c("city", "sex", "childbear")], by = c("city" = "city", "sex" = "sex"))

marrate10 = marriage10 %>%
  left_join(., unique(city_name00[, c("name_cn10", "name_harmonized", "province", "region.x", "region.y")]), by = c("city" = "name_cn10")) %>%
  group_by(name_harmonized) %>%
  summarise(total = sum(as.numeric(`16岁及以上人口`)), 
            total_f = sum(as.numeric(`16岁及以上人口`[sex == "f"])), 
            births = sum(as.numeric(`15-64岁妇女平均活产子女数`[sex == "f"])*childbear[sex == "f"])/sum(childbear[sex == "f"]),
            married = sum(as.numeric(`有配偶`))/total,
            married_f = sum(as.numeric(`有配偶`[sex == "f"]))/total_f,
            married_m = sum(as.numeric(`有配偶`[sex == "m"]))/(total-total_f),
            region.x = unique(region.x),
            region.y = unique(region.y),
            province = unique(province)) %>%
  ungroup()

cor.test(marrate10$births, marrate10$married_f)
ggplot(marrate10 %>% filter(province %in% c("江苏"))) +
  geom_point(aes(married, births, color = province))

# fines -----------------------------------------------------------------------------------------------
# !Run `fines.R` Before RUN Codes Blow

# 2010 

prop_women = census2010_age_gender %>%
  pivot_longer(., cols = 2:ncol(.), names_to = "agegroup", values_to = "n") %>% 
  left_join(., unique(city_name00[, c("name_cn10", "name_harmonized")]), by=c("city" = "name_cn10")) %>%
  group_by(name_harmonized) %>% 
  summarise(prop_women = sum(n[agegroup %in% c("15-19岁","20-24岁","25-29岁", "30-34岁","35-39岁",
                                               "40-44岁","45-49岁")])/sum(n))
dim(ethurb10)
names(ethurb10) 
fine10 = ethurb10 %>% 
  left_join(., unique(city_name00[, c("name_harmonized", "province")])) %>% 
  left_join(., fines %>% filter(birthyear == 2010) %>% select(fine, policy, province_cn), by = c("province" = "province_cn")) %>% 
  left_join(., marrate10[, c("name_harmonized", "births", "married")]) %>% 
  mutate(prop_minor0 = prop_minor - mean(prop_minor),
         prop_urban0 = prop_urban - mean(prop_urban),
         fine10 = case_when(
           policy %in% c(1) ~ (1-prop_minor+1/2*prop_minor)*fine, 
           policy %in% c(1.5) ~ (prop_urban+1/1.5*(1-prop_urban)+1/1.5*prop_minor)*fine,
           policy %in% c(2) ~ (prop_urban+1/2*(1-prop_urban)+1/2*prop_minor)*fine
    ),
    fine10_2 = case_when(
      policy %in% c(1) ~ (1-prop_minor0+1/2*prop_minor0)*fine, 
      policy %in% c(1.5) ~ ((prop_urban0+1/1.5*(1-prop_urban0))*(1-prop_minor0)+1/1.5*prop_minor0)*fine,
      policy %in% c(2) ~ ((prop_urban0+1/2*(1-prop_urban0))*(1-prop_minor0)+1/2*prop_minor0)*fine
    )
    )

city_name00$code = as.character(city_name00$code) 
fine10 = fine10 %>% 
  left_join(., unique(city_name00[!duplicated(city_name00$name_harmonized), c("name_harmonized", "code")])) %>% 
  left_join(., zap_labels(fines_prefect[, c("GB_00", "finerate1", "finerate2", "policy2")]), by = c("code" = "GB_00"))
hist(fine10$fine10)

# normalization 
fine10$fine10 = (fine10$fine10-min(fine10$fine10))/(max(fine10$fine10) - min(fine10$fine10))
fine10$fine10_2 = (fine10$fine10-min(fine10$fine10))/(max(fine10$fine10) - min(fine10$fine10))

# 2000

fine00 = ethurb00 %>% 
  left_join(., unique(city_name00[, c("name_harmonized", "province")])) %>% 
  left_join(., fines %>% 
              filter(birthyear == 2000) %>% 
              select(fine, policy, province_cn), by = c("province" = "province_cn")) %>% 
  mutate(fine00 = case_when(
    policy %in% c(1) ~ (1-prop_minor)*fine, 
    policy %in% c(1.5) ~ (prop_urban+1/1.5*(1-prop_urban))*(1-prop_minor)*fine,
    policy %in% c(2) ~ (prop_urban+1/2*(1-prop_urban))*(1-prop_minor)*fine
  ),
  fine00_2 = case_when(
    policy %in% c(1) ~ (1-prop_minor)*fine, 
    policy %in% c(1.5) ~ (1-prop_minor)*(1/1.5)*fine,
    policy %in% c(2) ~ (1-prop_minor)*(1/2)*fine)
  )

# normalization 
#ethurb00$fine00 = (ethurb00$fine00-min(ethurb00$fine00))/(max(ethurb00$fine00) - min(ethurb00$fine00))

fine00 = fine00 %>% 
  left_join(., unique(city_name00[!duplicated(city_name00$name_harmonized), c("name_harmonized", "code")])) %>% 
  left_join(., zap_labels(fines_prefect[, c("GB_00", "finerate1", "finerate2", "policy2")]), by = c("code" = "GB_00"))

# 1990

fine90 = ethurb90 %>% 
  left_join(., unique(city_name00[, c("name_harmonized", "province")])) %>% 
  left_join(., fines %>% filter(birthyear == 1990) %>% select(fine, policy, province_cn), by = c("province" = "province_cn")) %>% 
  mutate(fine90 = case_when(
    policy %in% c(1) ~ (1-prop_minor)*fine, 
    policy %in% c(1.5) ~ (prop_urban+1/1.5*(1-prop_urban))*(1-prop_minor)*fine,
    policy %in% c(2) ~ (prop_urban+1/2*(1-prop_urban))*(1-prop_minor)*fine
  ))

# normalization 
ethurb90$fine90 = (ethurb90$fine90-min(ethurb90$fine90))/(max(ethurb90$fine90) - min(ethurb90$fine90))

ethurb90 = ethurb90 %>% 
  left_join(., unique(city_name00[!duplicated(city_name00$name_harmonized), c("name_harmonized", "code")])) %>% 
  left_join(., zap_labels(fines_prefect[, c("GB_00", "finerate1", "finerate2", "policy2")]), by = c("code" = "GB_00"))

# check 

# weakly correlated with urban population 
cor.test(fine10$fine10_2, fine10$finerate1);plot(fine10$fine10_2, fine10$finerate1)
cor.test(fine00$fine00_2, fine00$fine00);plot(fine00$fine00_2, fine00$fine00)

subset(fine10, fine10_2 >5)[, c("name_harmonized", "fine10_2")]

# highly correlated with Epstein's calculation 
cor.test(fine00$fine00, fine00$finerate1); plot(fine00$fine00, fine00$finerate1)  
cor.test(ethurb00$fine00, ethurb00$finerate1); plot(ethurb00$fine00, ethurb00$finerate1)  

# deviant points in plots where fine00 is similar but finerate1 wide-spanned
subset(ethurb00, fine00 >0.7 & fine00 <0.8 & finerate1> 2 & finerate1< 4) 
