# Run codes below after running `datatidy.R` in this project. 
# `datatidy_segregation.R` is for computing occupational gender segregation.

library(tidyverse)
library(openxlsx)
library(ipumsr)

setwd("C:/Users/WXY/Documents/R_learning/segregation")

# load in data ------------------------------------------------------------------------------

occ_code00 = read.xlsx("data/occupation_code.xlsx", sheet = "2000")
occ_code90 = read.xlsx("data/occupation_code.xlsx", sheet = "1990")
city_name00 = read.xlsx("data/cityname_00.xlsx", sheet = 1)

# occupation data in 2010 ---------------------------------------------------------------------

occup10data = read.xlsx('data/occupation_city.xlsx', sheet = 1) 

occup10data = subset(occup10data, !grepl("  |市辖县|重庆市", city))

# create variable 'sex' //生成性别变量
occup10data$sex <- NA 
occup10data$sex[grep("f|m|total", occup10data$city)] <- occup10data$city[grep("f|m|total", occup10data$city)]
occup10data$sex <- zoo::na.locf(occup10data$sex)

# convert to numeric //转化为数字格式
occup10data[, 3:(ncol(occup10data) - 1)] <- sapply(occup10data[, 3:(ncol(occup10data)-1)], as.numeric)

# citys whose sample size is small
occup10data %>% 
  filter(sex == "total" & city !="total") %>%
  select(city, 合计) %>%
  filter(合计<10000)

# transform data structure //转换数据框结构
names(occup10data)[1:10]
occup10 <- occup10data[,-3] %>% 
  filter(sex %in% c("m", "f") & !city %in% c("m", "f")) %>% # select only two sexes
  pivot_longer(., names_to = "class2_2010", values_to = "n", 3:(ncol(.)-1)) %>% # long data with occupation categories in one coloumn
  filter(grepl("一|二|三|四|五|六|七", class2_2010) == FALSE) 

occup10 = occup10 %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  filter(!is.na(f)|!is.na(m)) %>% # delete empty occupations
  mutate(m = ifelse(is.na(m) & f>=3, 0.1, m), # replace na in female/male positions with 0.1
         f = ifelse(is.na(f) & m>=3, 0.1, f)
         ) %>% 
  filter(!is.na(f)&!is.na(m), f+m>3) %>% # delete occupations have less than 3 workers 
  pivot_longer(cols = 4:5, names_to = "sex", values_to = "n") %>% 
  left_join(., unique(occ_code00[, c("class2_2010", "class1_2010", "OCCISCO_label")]), by = c("class2_2010" = "class2_2010")) %>% 
  left_join(., unique(city_name00[, c("name_cn10", "name_harmonized")]), by = c("city" = "name_cn10")) 

occup10 %>% 
  pivot_wider(names_from = "sex", values_from = "n")%>% 
  filter(f == 0.1|m == 0.1) %>%
  View(.)

skill = occup10 %>% 
  group_by(name_harmonized) %>% 
  summarise(skill = sum(n[class1_2010 %in% c("二、专业技术人员")])/sum(n))

# occupation data in 2000 ------------------------------------------------------------

occup00data = data_00 %>% 
  zap_ipums_attributes(.) %>%
  filter(age >= 15 & CN2000A_OCC != 0) %>%
  left_join(., occ_code00[, c("CN2000A_OCC", "class2_2010")], by = c("CN2000A_OCC" = "CN2000A_OCC"))

occup00 = occup00data %>% 
  group_by(name_harmonized, sex, class2_2010) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  left_join(., occ_code00[!duplicated(occ_code00$class2_2010), c("class2_2010", "class1_2010", "OCCISCO_label")], by = c("class2_2010" = "class2_2010"))

occup00 = occup00 %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  filter(!(is.na(f)&is.na(m))) %>% # delete empty occupations
  mutate(m = ifelse(is.na(m) & f>=3, 0.1, m), # replace na in female/male positions with 0.1
         f = ifelse(is.na(f) & m>=3, 0.1, f)
  ) %>% 
  filter(!is.na(f)&!is.na(m), f+m>3) %>% # delete occupations have less than 3 workers 
  pivot_longer(cols = 5:6, names_to = "sex", values_to = "n") 
  
# ipums_val_labels(data2) 
# data00_city$code[data00_city$code == 1100] <- 1101
# data00_city$code[data00_city$code == 1200] <- 1201
# data00_city$code[data00_city$code == 3100] <- 3101
# data00_city$code[data00_city$code == 4600] <- 4690
# data00_city$code[data00_city$code == 5000] <- 5002

saveRDS(occup00, "data/occup00.rds") 

occup00 = readRDS("data/occup00.rds")

# occup00 = read.xlsx("data/occupation_city00.xlsx", sheet = 1) # contain missing data

# occupation data in 1990 ------------------------------------------------------------

occup90data = data_90 %>% 
  zap_ipums_attributes(.) %>%
  filter(age >= 15 & CN1990A_OCC != 0) %>%
  left_join(., occ_code90[, c("CN1990A_OCC", "class2_2010")], by = c("CN1990A_OCC" = "CN1990A_OCC"))

occup90 = occup90data %>%
  group_by(name_harmonized, sex, class2_2010) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  left_join(., occ_code90[!duplicated(occ_code90$class2_2010), c("class2_2010", "class1_2010", "OCCISCO_label")], by = c("class2_2010" = "class2_2010"))

occup90 = occup90 %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>% 
  filter(!(is.na(f)&is.na(m))) %>% # delete empty occupations
  mutate(m = ifelse(is.na(m) & f>=3, 0.1, m),
         f = ifelse(is.na(f) & m>=3, 0.1, f)
  ) %>% 
  filter(!is.na(f)&!is.na(m), f+m>3) %>% # delete occupations have less than 3 workers 
  pivot_longer(cols = 5:6, names_to = "sex", values_to = "n") 

test = occup90data %>%
  filter(name_harmonized == "玉树藏族自治州") %>% 
  group_by(sex, class2_2010) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "sex", values_from = "n") 
test = test %>% 
  filter(!(is.na(f)&is.na(m))) %>% # delete empty occupations
  mutate(m = ifelse(is.na(m) & f>=3, 0.1, m),
         f = ifelse(is.na(f) & m>=3, 0.1, f)
  ) %>% 
  filter(!is.na(f)&!is.na(m), f+m>3) %>% # delete occupations have less than 3 workers 
  pivot_longer(cols = 2:3, names_to = "sex", values_to = "n") 

test %>% View()
test$city = "a"
get_A(test, unit = "city", occupation = "class2_2010", group = "sex", weight = "n")

# data90_city$code[data90_city$code == 1100] <- 1101
# data90_city$code[data90_city$code == 1200] <- 1201
# data90_city$code[data90_city$code == 3100] <- 3101
# data90_city$code[data90_city$code == 4600] <- 4690
# data90_city$code[data90_city$code == 5000] <- 500
saveRDS(occup90, "data/occup90.rds")

occup90 = readRDS("data/occup90.rds")

# occupation data in 1982 ------------------------------------------------------------
# not used currently

# industry*occupation 2000 ------------------------------------------------------------

ind2_00 = data_00 %>% 
  zap_ipums_attributes(.) %>% 
  filter(age >= 15, CN2000A_WORK %in% c(1, 2)) %>% 
  count(name_harmonized, IND, CN2000A_OCC, sex, name = "n") %>% 
  left_join(., ind_code00[, 1:3], by = c("IND" = "code")) %>%
  left_join(., occ_code00[, c("CN2000A_OCC", "class2_2010", "class1_2010")], by = c("CN2000A_OCC" = "CN2000A_OCC"))
unique(data_90$IND)

ind2_90 = data_90 %>% 
  zap_ipums_attributes(.) %>% 
  filter(age >= 15, CN2000A_WORK %in% c(1, 2)) %>% 
  count(name_harmonized, IND, CN2000A_OCC, sex, name = "n") %>% 
  left_join(., ind_code00[, 1:3], by = c("IND" = "code")) %>%
  left_join(., occ_code00[, c("CN2000A_OCC", "class2_2010", "class1_2010")], by = c("CN2000A_OCC" = "CN2000A_OCC"))

ind2_00$IND = factor(ind2_00$IND, levels = sort(unique(ind2_00$IND)))

ind2_00 %>% 
  filter(ind_2 %in% c("State administrations", "Political party administrations"),
         class2_2010 %in% c("1.中国共产党中央委员会和地方各级组织负责人","2.国家机关及其工作机构负责人", 
                            "4.事业单位负责人", "20.行政办公人员")) %>%
  group_by(ind_2, class2_2010) %>%
  summarise(sexratio = sum(n[sex == "f"])/sum(n[sex == "m"])) %>%
  ggplot() +
  geom_line(aes(class2_2010, sexratio, group = ind_2, linetype = ind_2))

ind2_00$indrecode = as.character(ind2_00$IND)
ind2_00$indrecode[!ind2_00$IND %in% c('94','95','96','97')] = "other"
ind2_00$indrecode[ind2_00$IND %in% c('94','95','96','97')] = "public"

ind2_00 %>% 
  filter(class1_2010 %in% c("三、办事人员和有关人员"), !IND %in% c(99)) %>% 
  group_by(indrecode) %>%
  summarise(sexratio = sum(n[sex == "f"])/sum(n[sex == "m"])) %>%
  ggplot() + 
  geom_bar(aes(indrecode, sexratio), stat = "identity")
"20.行政办公人员"

##### function: compute D& Ds #####

# function: compute dissimilarity
get_D <- function(data, unit, occupation, group, weight, index = c("D", "Ds", "both")){
  data <- data[, c(unit, occupation, group, weight)]
  names(data) = c("unit", "occupation", "sex", "n")
  data$n[is.na(data$n)] = 0
  data$n <- as.numeric(data$n)
  data0 = data %>%
    group_by(unit, occupation, sex) %>% 
    summarise(n = sum(n, na.rm = T)) %>%
    pivot_wider(., names_from = "sex", values_from = "n", values_fill = list(n = 0))
  D = data0 %>% 
    group_by(unit) %>%
    summarise(d = sum(abs(f/sum(f) - m/sum(m)))/2)
  Ds = data0 %>% 
    mutate(p_f = f/(f+m), p_m = m/(f+m)) %>%
    group_by(unit) %>%
    summarise(ds = sum(abs(p_f/sum(p_f, na.rm=T) - p_m/sum(p_m, na.rm=T)), na.rm = T)/2)
  if(index == "D"){
    return(D)
  }
  if(index == "Ds"){
    return(Ds)
  }
  if(index == c("both")){
    D = left_join(D, Ds)
    return(D)
  }
}

# Debugging
# unit = "year"; occupation = "class2_2010";datamode = "long";group = "sex";weight = "n"
# data = occ_country90_10

##### function: compute A #####

get_A <- function(data, unit, occupation, group = "sex", weight, datamode = "long", local = FALSE){
   if(datamode == "long"){
    data = data[, c(unit, occupation, group, weight)]
    names(data) = c("unit", "occupation", "sex", "n")
    data$n[is.na(data$n)] = 0.1
    data0 = data %>%
      group_by(unit, occupation, sex) %>% 
      summarise(n = sum(n, na.rm = T)) %>%
      pivot_wider(., names_from = "sex", values_from = "n", values_fill = list(n = 0.01))
  }else{data0 = data}
  if(!"f" %in% names(data0)){
  names(data0)[3:4] = c("f", "m") 
  }
  data0 = data0 %>% 
    group_by(unit) %>% 
    mutate(logratio = log(f/m), log_diff = logratio - mean(logratio))
  if(local == TRUE){
    return(
      data.frame(data0[,c("unit", "occupation")], 
                 a_local = data0$log_diff, stringsAsFactors = FALSE)
      )
  }
  return(
    data0 %>% group_by(unit) %>% 
      summarise(a = exp(mean((log_diff)^2)^(1/2)),
                r = mean(abs(log_diff)))
         )
}

# Debug
# data provided in Grusky& Charles's paper
# debugdata = data.frame(country = rep(c("Japan", "Sweden"), each = 6),
#                        occupaion = rep(c(1:6), 2), 
#                        f = c(c(23661, 1386, 50787, 23661, 21780, 55440), c(6006, 869, 1089, 2057, 1628, 11407)),
#                        m = c(c(28710, 19503, 44649, 51381, 21780, 147312), c(7183, 231, 4675, 1793, 4994, 2453))
#                        )
# get_A(data = debugdata, datamode = "wide", occupation = "occupaion", group = "sex", weight = "n", local = TRUE)
unit = "name_harmonized"; occupation = "class2_2010";datamode = "long";group = "sex";weight = "n"
data = occup10_long
test = get_A(occup10_long, unit = "name_harmonized", occupation = "class2_2010",datamode = "long",group = "sex",weight = "n")
plot(test$r, test$a)

##### function: compute M #####

get_M <- function(data, occupation, local){
  colnames(data)[colnames(data)==occupation] = "occupation"
  data = data %>%
    spread(., key = "sex", value = "n", fill = 0.001
    ) %>%
    as.data.frame() %>%
    mutate(pf = f/sum(f+m), pm = m/sum(f+m), pn = pf+pm,
           entropy_sex = sum(pf)*log(1/sum(pf))+sum(pm)*log(1/sum(pm)),
           entropy_n = pf/pn*log(1/(pf/pn/sum(pf)))+pm/pn*log(1/(pm/pn/sum(pm))),
           m_local = entropy_sex-entropy_n) 
  if(local == TRUE){
    return(data[, c("occupation", "m_local")])
  }
  return(sum(data$m_local*data$pn))
}

test00 = occup00_class2 %>% 
  filter(name_harmonized %in% c("北京市"))
test10 = occup10_class2 %>%
  filter(name_harmonized %in% c("北京市"))

m_local = get_M(test10, occupation = "class2_2010", local = TRUE)

library(segregation)
mutual_total(test00, group = "sex", unit = "class2_2010", weight = "n")

# index D ----------------------------------------------------------------
# 3 occupation classifications 

# ISCO classifications
dsd10_isco <- occup10 %>%
  filter(!OCCISCO_label %in% c("Other occupations, unspecified or n.e.c.", "Skilled agricultural and fishery workers")) %>%
  get_D(., index = "both", unit = "name_harmonized", divide1 = "sex", divide2 = "OCCISCO_label", value = "n")

dsd00_isco <- occup00 %>%
  filter(!OCCISCO_label %in% c("Other occupations, unspecified or n.e.c.", "Skilled agricultural and fishery workers")) %>%
  get_D(., index = "both", unit = "name_harmonized", divide1 = "sex", divide2 = "OCCISCO_label", value = "n")

dsd90_isco <- occup90 %>%
  filter(!OCCISCO_label %in% c("Other occupations, unspecified or n.e.c.", "Skilled agricultural and fishery workers")) %>%
  get_D(., index = "both", unit = "name_harmonized", divide1 = "sex", divide2 = "OCCISCO_label", value = "n")

dsd_isco = left_join(dsd10_isco %>% transmute(unit, d10 = d, ds10 = ds),
                     dsd00_isco %>% transmute(unit, d00 = d, ds00 = ds)) %>% 
  left_join(., dsd90_isco %>% transmute(unit, d90 = d, ds90 = ds))

# class 1 classifications
dsd10_class1 <- occup10 %>%
  get_D(., index = "both", unit = "name_harmonized", divide1 = "sex", divide2 = "class1_2010", value = "n")

dsd00_class1 <- occup00 %>%
  get_D(., index = "both", unit = "name_harmonized", divide1 = "sex", divide2 = "class1_2010", value = "n")

dsd90_class1 <- occup90 %>%
  get_D(., index = "both", unit = "name_harmonized", divide1 = "sex", divide2 = "class1_2010", value = "n")

dsd_class1 = left_join(dsd10_class1 %>% transmute(unit, d00 = d, ds00 = ds),
                       dsd00_class1 %>% transmute(unit, d10 = d, ds10 = ds)) %>% 
  left_join(., dsd90_class1 %>% transmute(unit, d90 = d, ds90 = ds))

# class 2 classifications
dsd10_class2 <- occup10 %>% 
  get_D(., index = "both", unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n")

dsd10_class2nonagri <- occup10 %>%
  filter(!class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_D(., index = "both", unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n")

dsd10_class2agri <- occup10 %>%
  filter(class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_D(., index = "both", unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n")

dsd00_class2 <- occup00 %>%
  get_D(., index = "both", unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n")

dsd90_class2 <- occup90 %>%
  get_D(., index = "both", unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n")

# combine D& Ds of all periods
dsd_class2 = rbind(dsd10_class2, dsd00_class2, dsd90_class2)
dsd_class2$year = rep(seq(2010, 1990, -10), c(304, 321, 321))
dsd_class2 = dsd_class2 


cor.test(dsd10_class2$d, 
         dsd00_class2$d[dsd00_class2$unit %in% dsd10_class2$unit])

# scatter point plot & marginal boxplot 
showtext_auto() 
p = dsd_class2 %>% 
  ggplot(aes(d, ds, colour = factor(year))) +
  geom_point() + 
  #scale_y_continuous(limits = c(0, 0.5)) +
  #scale_x_continuous(limits = c(0, 0.5)) +
  coord_fixed() +
  ylab("Ds") + 
  xlab("D") + 
  theme_bw() + 
  theme(text = element_text(size = 18),
        panel.grid = element_blank()) 
p
p2 <- ggExtra::ggMarginal(p, type="boxplot")
p2
ggsave(p2, filename = "fig/d_ds_distribution10.pdf", width = 8, height = 8, dpi = 300)

# look at those extreme values //看一看极值
# D和Ds指数相差大于0.2的地区, 第一产业的占比很大，D受到职业结构的影响
dsd_10[abs(dsd_10$d - dsd_10$ds) > 0.2,]
occup10[occup10$name_harmonized %in% c(dsd_10[abs(dsd_10$d - dsd_10$ds) > 0.2,]$unit), ] %>%
  ggplot(aes(OCCISCO_label, n, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(facets = vars(name_harmonized)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = -90))

dsd = rbind(dsd_10, dsd_00, dsd_90)
dsd$year = rep(c(2010, 2000, 1990), c(nrow(dsd_10), nrow(dsd_00), nrow(dsd_90)))

# description
dsd %>% group_by(year) %>%
  summarise(mean = mean(d), sd = sd(d), min = min(d), max = max(d),
            meands = mean(ds), sdds = sd(ds), minds = min(ds), maxds = max(ds)) %>%
  apply(., 2, function(x)round(x,digits = 2))
  arrange(desc(year)) %>%
  write.csv(.,"data/describe_D_10.csv", row.names = T) 

# index A ----------------------------------------------------------------

# class 2 classifications
a10_class2 <- occup10 %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a00_class2 <- occup00 %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a90_class2 <- occup90 %>% 
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a10_nonagri <- occup10 %>%
  filter(!class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a00_nonagri <- occup00 %>%
  filter(!class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a90_nonagri <- occup90 %>%
  filter(!class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a10_agri <- occup10 %>%
  filter(class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a00_agri <- occup00 %>%
  filter(class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

a90_agri <- occup90 %>%
  filter(class1_2010 %in% c("五、农、林、牧、渔、水利业生产人员")) %>%
  get_A(., unit = "name_harmonized", group = "sex", occupation = "class2_2010", weight = "n", datamode = "long")

# combine a of all periods
a_class2 = left_join(a10_class2 %>% transmute(unit, a10 = a),
                       a00_class2 %>% transmute(unit, a00 = a)) %>% 
  left_join(., a90_class2 %>% transmute(unit, a90 = a))

a_class2$year = 
a10_class2 = left_join(a10_class2, a_class2, by = c("unit" = "unit"))
plot(a10_class2$a, a10_class2$a10)

a10_class2 %>% mutate(dif = abs(a-a10)) %>%
  ggplot() +
  geom_boxplot(aes(a10)) 

a10 = left_join(a10_class2, 
                a10_nonagri %>% 
                  select(unit, a_nonagri = a, r_nonagri = r)) %>%
  left_join(., a10_agri %>% 
              select(unit, a_agri = a, r_agri = r))

