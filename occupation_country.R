library(openxlsx)
library(dplyr)
library(tidyr)
library(showtext)
library(zoo)

# load in data ------------------------------------------------------------------------------

occ_code00 = read.xlsx("data/occupation_code.xlsx", sheet = "2000")
occ_code90 = read.xlsx("data/occupation_code.xlsx", sheet = "1990")

# occupation by age --------------------------------------------------------------------

# 2015
occ_country_ageedu15 = read.xlsx('data/occupation_province.xlsx', sheet = "2015_edu")

occ_country_ageedu15 = occ_country_ageedu15[,-3] %>% 
  filter(!grepl("不便分类的|total", occupation)) %>%
  pivot_longer(., cols = 3:4, names_to = "sex", values_to = "n") %>%
  ungroup() %>%
  pivot_wider(., names_from = "edu", values_from = "n") %>%
  transmute(occupation = trimws(occupation), sex, 
            本科及以上 = 大学本科+研究生, 
            大学专科, 
            高中 = 普通高中+中职, 
            初中, 
            小学及以下 = 小学+未上过学) %>%
  pivot_longer(., names_to = "edu", values_to = "n", col = 3:ncol(.)) 

occ_country_ageedu15$edu = factor(occ_country_ageedu15$edu, levels = c("小学及以下", "初中", "高中", "大学专科", "本科及以上"))

# 2010 
occ_country_age10 = read.xlsx('data/occupation_province.xlsx', sheet = "2010_age")
names(occ_country_age10) 
occ_country_age10 = occ_country_age10 %>% 
  pivot_longer(., names_to = "class2_2010", values_to = "n", col = 4:ncol(.)) %>%
  filter(!grepl("一|二|三|四|五|六|七|不便分类的", class2_2010),
         !age %in% c("total")) %>%
  left_join(., occ_code00[, c("class2_2010", "class1_2010")], by = c("class2_2010" = "class2_2010"))

# 2005 
occ_country_age05 = read.xlsx('data/occupation_province.xlsx', sheet = "2005_age")
occ_country_age05 = occ_country_age05 %>% 
  pivot_longer(., names_to = "class1_2010", values_to = "n", col = 4:ncol(.)) %>%
  filter(!grepl("七|不便分类的", class1_2010) & !age %in% c("total"))

# 2000 
occ_country_ageedu00 = read.xlsx('data/occupation_province.xlsx', sheet = "2000_edu_age")
occ_country_ageedu00[is.na(occ_country_ageedu00)] = 0
occ_country_ageedu00 = occ_country_ageedu00[, -4] %>%
  pivot_longer(., names_to = "class1_2010", values_to = "n", col = 4:ncol(.)) %>% 
  filter(grepl("-", age), !age %in% c("total"), !edu %in% c("total"), !grepl("七|不便分类的", class1_2010)) %>% 
  group_by(class1_2010, sex, age) %>%
  summarise(
    小学及以下 = sum(n[edu %in% c("小学", "未上过学", "扫盲班")], na.rm = TRUE),
    初中 = sum(n[edu %in% c("初中")], na.rm = TRUE),
    高中 = sum(n[edu %in% c("高中", "中专")], na.rm = TRUE),
    大学专科 = sum(n[edu %in% c("大学专科")], na.rm = TRUE),
    本科及以上 = sum(n[edu %in% c("大学本科", "研究生")], na.rm = TRUE)) %>% 
  pivot_longer(., names_to = "edu", values_to = "n", 4:ncol(.)) %>% 
  ungroup(.)

occ_country_ageedu00$edu = factor(occ_country_ageedu00$edu, levels = c("小学及以下", "初中", "高中", "大学专科", "本科及以上"))
occ_country_ageedu00$class1_2010 = factor(occ_country_ageedu00$class1_2010, 
                                          levels = c("一、国家机关、党群组织、企业、事业单位负责人","二、专业技术人员","三、办事人员和有关人员","四、商业、服务业人员","五、农、林、牧、渔、水利业生产人员","六、生产、运输设备操作人员及有关人员"))

# 1995
# occ_country_ageedu95 = read.xlsx('data/occupation_province.xlsx', sheet = "1995_edu_age")
# occ_country_ageedu95[is.na(occ_country_ageedu95)] = 0
# occ_country_ageedu95 = occ_country_ageedu95[-4] %>% 
#   transmute(age, sex, edu, 
#             `一、国家机关、党群组织、企业、事业单位负责人` = `二、国家机关、党群组`,
#             `二、专业技术人员` = `一、各类专业技术人员`,
#             `三、办事人员和有关人员` = `三、办事人员和有关人员`,
#             `四、商业、服务业人员` = `四、商业工作人员`+`五、服务性工作人员`,
#             `五、农、林、牧、渔、水利业生产人员` = `六、农、林、牧、渔劳动者`,
#             `六、生产、运输设备操作人员及有关人员` = `七、生产工人、运输工人和有关人员`
#   ) %>%
#   pivot_longer(., names_to = "class1_2010", values_to = "n", col = 4:ncol(.)) %>% 
#   filter(grepl("-", age), !age %in% c("total"), !edu %in% c("total"), !grepl("不便分类的", class1_2010)) %>% 
#   group_by(class1_2010, sex, age) %>%
#   summarise(
#     小学及以下 = sum(n[edu %in% c("小学", "未上过学", "扫盲班")], na.rm = TRUE),
#     初中 = sum(n[edu %in% c("初中")], na.rm = TRUE),
#     高中 = sum(n[edu %in% c("高中")], na.rm = TRUE),
#     大专以上 = sum(n[edu %in% c("大专以上")], na.rm = TRUE)) %>% 
#   pivot_longer(., names_to = "edu", values_to = "n", 4:ncol(.)) %>% 
#   ungroup(.)

# occ_country_ageedu95$edu = factor(occ_country_ageedu95$edu, 
#                                   levels = c("小学及以下", "初中", "高中", "大专以上"))
# occ_country_ageedu95$class1_2010 = factor(occ_country_ageedu95$class1_2010, 
#                                           levels = c("一、国家机关、党群组织、企业、事业单位负责人","二、专业技术人员","三、办事人员和有关人员","四、商业、服务业人员","五、农、林、牧、渔、水利业生产人员","六、生产、运输设备操作人员及有关人员"))

# 1990
occ_country_age90 = read.xlsx('data/occupation_province.xlsx', sheet = "1990_age")
occ_country_age90 = occ_country_age90[-3] %>% 
  transmute(age, sex,
            `一、国家机关、党群组织、企业、事业单位负责人` = `二、国家机关、党群组`,
            `二、专业技术人员` = `一、各类专业技术人员`,
            `三、办事人员和有关人员` = `三、办事人员和有关人员`,
            `四、商业、服务业人员` = `四、商业工作人员`+`五、服务性工作人员`,
            `五、农、林、牧、渔、水利业生产人员` = `六、农、林、牧、渔劳动者`,
            `六、生产、运输设备操作人员及有关人员` = `七、生产工人、运输工人和有关人员`
  ) %>%
  pivot_longer(., names_to = "class1_2010", values_to = "n", col = 3:ncol(.)) %>% 
  filter(grepl("-", age), !age %in% c("total")) 

occ_country_age90$class1_2010 = factor(occ_country_age90$class1_2010, 
                                          levels = c("一、国家机关、党群组织、企业、事业单位负责人","二、专业技术人员","三、办事人员和有关人员","四、商业、服务业人员","五、农、林、牧、渔、水利业生产人员","六、生产、运输设备操作人员及有关人员"))

# 1982
# dada82 %>%
#   zap_ipums_attributes()

# A in 2010
occ_country_age10 %>%
  group_by(sex, class1_2010) %>%
  summarise(`25-34岁` = sum(n[age %in% c("25-29岁", "30-34岁")]),
            `35-44岁` = sum(n[age %in% c("35-39岁", "40-44岁")]),
            `45-54岁` = sum(n[age %in% c("45-49岁", "50-54岁")]),
            `55-64岁` = sum(n[age %in% c("55-59岁", "60-64岁")])
            ) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "age", values_to = "n") %>%
  get_A(data = ., unit = "age", group = "sex", occupation = "class1_2010", weight = "n", datamode = "long")

# A in 2000
occ_country_ageedu00 %>% 
  group_by(sex, class1_2010) %>%
  summarise(`25-34岁` = sum(n[age %in% c("25-29岁", "30-34岁")]),
            `35-44岁` = sum(n[age %in% c("35-39岁", "40-44岁")]),
            `45-54岁` = sum(n[age %in% c("45-49岁", "50-54岁")]),
            `55-64岁` = sum(n[age %in% c("55-59岁", "60-64岁")])
  ) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "age", values_to = "n") %>%
  get_A(data = ., unit = "age", group = "sex", occupation = "class1_2010", weight = "n", datamode = "long")

# A in 1990
occ_country_age90 %>% 
  group_by(sex, class1_2010) %>%
  summarise(`25-34岁` = sum(n[age %in% c("25-29岁", "30-34岁")]),
            `35-44岁` = sum(n[age %in% c("35-39岁", "40-44岁")]),
            `45-54岁` = sum(n[age %in% c("45-49岁", "50-54岁")]),
            `55-64岁` = sum(n[age %in% c("55-59岁", "60-64岁")])
  ) %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "age", values_to = "n") %>%
  get_A(data = ., unit = "age", group = "sex", occupation = "class1_2010", weight = "n", datamode = "long")

# occupation by edu ------------------------------------------------------------

# 2015
# occ_country_ageedu15

# 2000
# occ_country_ageedu00

# 1995
# occ_country_ageedu95

# 1990 
occ_country_edu90 = read.xlsx('data/occupation_province.xlsx', sheet = "1990_edu") 
names(occ_country_edu90)
occ_country_edu90[is.na(occ_country_edu90)] = 0 
occ_country_edu90 = occ_country_edu90[,-3] %>% 
  filter(grepl("\\s\\s\\s", occupation), !grepl("不便分类的|total", occupation)) %>%
  transmute(occupation = trimws(occupation), sex, 
            本科及以上 = 大学本科, 
            大学专科, 
            高中 = 高中+中专, 
            初中, 
            小学及以下 = 小学+文盲) %>% 
  pivot_longer(., names_to = "edu", values_to = "n", col = 3:ncol(.)) %>%
  left_join(., occ_code90[, c("class3_1990", "class2_2010", "class1_2010")], 
            by = c("occupation" = "class3_1990"))

occ_country_edu90$edu = factor(occ_country_edu90$edu, levels = c('小学及以下','初中','高中','大学专科','本科及以上'))
occ_country_edu90$class1_2010 = factor(occ_country_edu90$class1_2010, 
                                       levels = c("一、国家机关、党群组织、企业、事业单位负责人","二、专业技术人员","三、办事人员和有关人员","四、商业、服务业人员","五、农、林、牧、渔、水利业生产人员","六、生产、运输设备操作人员及有关人员"))
# 1987

# 1982 lack of data

# sex ratio in education levels
edu_n00 = occ_country_ageedu00 %>%
  group_by(edu, sex) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = "sex", values_from = "n") %>%
  mutate(prop = f/m, year = 2000)

edu_n90 = occ_country_edu90 %>%
  group_by(edu, sex) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = "sex", values_from = "n") %>%
  mutate(prop = f/m, year = 1990)

edu_n15 = occ_country_ageedu15 %>%
  group_by(edu, sex) %>% 
  summarise(n = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = "sex", values_from = "n") %>%
  mutate(prop = f/m, year = 2015) 

rbind(edu_n90, edu_n00, edu_n15) %>%
  ggplot(data =.) + 
  geom_line(aes(factor(year), prop, group = edu), size = rel(0.7)) +
  #geom_hline(data = .%>% group_by(year) %>%summarise(ratio = sum(f)/sum(m)), aes(yintercept = ratio, linetype = factor(year)))
  #geom_line(aes(edu, prop, group = year, linetype = factor(year)), size = rel(0.7)) +
  #geom_bar(aes(edu, prop, fill = factor(year)), stat = "identity", position = "dodge") +
  geom_point(aes(factor(year), prop)) + 
  facet_wrap(.~edu, nrow = 2)
  # geom_hline(data = .%>% group_by(year) %>%summarise(ratio = sum(f)/sum(m)), aes(yintercept = ratio, linetype = factor(year)))

# 
occ_edu00 = occ_country_ageedu00 %>% 
  group_by(edu, class1_2010, sex) %>%
  summarise(n = sum(n)) %>%
  group_by(edu, sex) %>%
  mutate(prop = n/sum(n))

occ_edu90 = occ_country_edu90 %>% 
  group_by(edu, class1_2010, sex) %>%
  summarise(n = sum(n)) %>%
  group_by(edu, sex) %>%
  mutate(prop = n/sum(n))

get_A(occ_country_ageedu15, unit = "edu", occupation = "occupation", group = "sex", weight = "n", datamode = "long")
get_A(occ_country_ageedu00, unit = "edu", occupation = "class1_2010", group = "sex", weight = "n", datamode = "long")
get_A(occ_country_edu90, unit = "edu", occupation = "class1_2010", group = "sex", weight = "n", datamode = "long")

# trends in each educational level ------------------------------------------------------------

occ_trend = read.xlsx("data/occupation_edu.xlsx", sheet = 1)[, -4]
occ_trend2 = read.xlsx("data/occupation_edu.xlsx", sheet = 2)
occ_sexratio = read.xlsx("data/occupation_edu.xlsx", sheet = 3)[, c(1:2, 4:5)]

occ_sexratio = subset(occ_sexratio, province == "全国")[, -1] %>%
  pivot_longer(cols = 2:3, names_to = "sex", values_to = "sexratio") 

occ_trend[is.na(occ_trend)] = 0
occ_trend2[is.na(occ_trend2)] = 0

occ_trend1 = left_join(occ_trend, occ_trend2[, 1:4], 
                       by = c("edu" = "edu", "year" = "year", "sex" = "sex")) %>%
  filter(sex %in% c("f", "m"), !edu %in% c("f", "m", "total")) %>% 
  pivot_longer(cols = 4:10, names_to = "occupation", values_to = "n") %>%
  mutate(value = (n*就业人员))

occ_trend1$occupation = factor(occ_trend1$occupation, levels = unique(occ_trend1$occupation))

# occupations of female with university degree 
unique(occ_trend1$edu) 
occ_trend1 = occ_trend1 %>% 
  group_by(year, sex, occupation) %>% 
  summarise(
    本科及以上 = sum(value[edu %in% c("大学本科", "研究生")])/sum(就业人员[edu %in% c("大学本科", "研究生")]),
    大学专科 = sum(value[edu %in% c("大学专科", "高等职业教育")])/sum(就业人员[edu %in% c("大学专科", "高等职业教育")]), 
    #大专以上 = sum(value[edu %in% c("大学本科", "研究生", "大学专科", "高等职业教育")])/sum(就业人员[edu %in% c("大学本科", "研究生", "大学专科", "高等职业教育")]),
    高中 = sum(value[edu %in% c("高中", "中等职业教育")])/sum(就业人员[edu %in% c("高中", "中等职业教育")]), 
    初中 = sum(value[edu %in% c("初中")])/sum(就业人员[edu %in% c("初中")]), 
    小学及以下 = sum(value[edu %in% c("小学", "未上过学")])/sum(就业人员[edu %in% c("小学", "未上过学")])
    ) %>%
  pivot_longer(cols = 4:ncol(.), names_to = "edu", values_to = "n") %>%
  ungroup()

occ_trend1 = rbind.data.frame(occ_trend1,
                              # add data in 2000
                              occ_edu00 %>% 
                                filter(edu == "本科及以上") %>% 
                               ungroup() %>%
                               transmute(year = 2000, sex = rep(c("f", "m"), 6),
                                         occupation = rep(levels(occ_trend1$occupation)[-7], each=2),
                                         edu = "本科及以上", n = prop*100), 
                           occ_edu90 %>% # add data in 1990
                             filter(edu == "本科及以上") %>% 
                             ungroup() %>% 
                             transmute(year = 1990, sex = rep(c("f", "m"), 6),
                                       occupation = rep(levels(occ_trend1$occupation)[-7], each = 2),
                                       edu = "本科及以上", n = prop*100),
                           # add 1991-1999
                           data.frame(year = rep(c(1991:1999, 2001), each = 12), sex = rep(rep(c("f", "m"), each = 6), 10),
                                      occupation = rep(levels(occ_trend1$occupation)[-7], 20),
                                      edu = "本科及以上", n = NA)
                   ) 

occ_trend1$n = occ_trend1$n/100
occ_trend1$n[occ_trend1$year %in% c(2002:2004)] = NA
unique(occ_trend1$occupation)

occ_trend1 %>% 
  filter(sex %in% c("f", "m"), 
         edu %in% c("本科及以上"), 
         occupation %in% c("商业服务人员", "专业技术人员", "办事人员和有关人员", "单位负责人")) %>%
  group_by(occupation, sex) %>%
  arrange(year) %>% 
  mutate(approx = na.approx(n)) %>%
  ggplot(data = ., 
         aes(year, approx, group = sex, colour = sex)) + 
  geom_line(data = . %>% filter(!is.na(approx)), 
            aes(linetype = sex)) +
  geom_point() +  
  facet_wrap(.~occupation, scales = "free_x") +
  scale_x_continuous(breaks = seq(1990, 2015, 5), labels = seq(1990, 2015, 5)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.2),
                     labels = scales::percent_format(accuracy = 1), limits = c(0, 0.9)) +
  theme_classic() + 
  labs(x = NULL,
       y = "百分比(%)") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    #panel.grid.major.y = element_blank(),   
    panel.border = element_rect(colour = "gray70", fill = NA),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_blank(), # element_text(hjust = -0.02, vjust = 1.2, size = rel(1.2), face = "bold"),
    panel.spacing.x = unit(0.4, "lines"),
    axis.line = element_line(size = 0.6),
    axis.ticks.length = unit(-0.1, "cm"),
    axis.title.y = element_text(vjust = -2, size = rel(1.5)),
    axis.text.y = element_text(size = rel(1.5), margin=unit(c(0.5,0.2,0.5,0.5), "cm")),
    axis.text.x = element_text(vjust = 0, size = rel(1.5)),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.5)),
    legend.spacing = unit(0.1,"cm"),
    legend.direction = "horizontal",
    legend.position = "bottom", 
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "white", colour = "gray30")) + 
  guides(linetype = guide_legend(keywidth = 3, keyheight = 1))

ggsave(filename = "fig/chart_occ_university.pdf", plot = last_plot(), width = 12, height = 8)

occ_edu05_10 = subset(occ_trend2, year %in% c(2005, 2010) & sex %in% c("f", "m") & edu != "total") %>%
  select(edu, year, sex, 就业人员) %>%
  left_join(., occ_sexratio, by = c("year" = "year", "sex" = "sex")) %>%
  mutate(employee = 就业人员*sexratio) %>%
  group_by(year, sex) %>% 
  summarise(
    本科及以上 = sum(employee[edu %in% c("大学本科", "研究生")]),
    大学专科 = sum(employee[edu %in% c("大学专科", "高等职业教育")]), 
    高中 = sum(employee[edu %in% c("高中", "中等职业教育")]), 
    初中 = sum(employee[edu %in% c("初中")]), 
    小学及以下 = sum(employee[edu %in% c("小学", "未上过学")])
  ) %>%
  pivot_longer(cols = 3:ncol(.), names_to = "edu", values_to = "employee") %>%
  ungroup()

data = subset(test, year %in% c(2005, 2010)) %>%
  left_join(occ_edu05_10, by = c("sex" = "sex", "year" = "year", "edu" = "edu")) %>%
  mutate(n = n*employee)

# occupation at country level ------------------------------------------------------------

# 2010 
occ_country0010 = read.xlsx('data/occupation_province.xlsx', sheet = "2010_2000")
occ_country0010 = occ_country0010 %>% 
  pivot_longer(., names_to = "class2_2010", values_to = "n", col = 6:ncol(.)) %>%
  filter(!grepl("不便分类的|一|二|三|四|五|六", class2_2010), province %in% c("total")) %>%
  left_join(., occ_code00[, c("class2_2010", "class1_2010")], by = c("class2_2010" = "class2_2010"))

# 1990 
occ_country90 = occ_country_edu90 %>% 
  group_by(class2_2010, sex) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  left_join(., occ_code00[, c("class2_2010", "class1_2010")]) %>% 
  mutate(year = 1990) %>% 
  select(class2_2010, class1_2010, sex, n, year)

occ_country90_10 = rbind.data.frame(occ_country0010 %>% select(class2_2010, class1_2010, sex, n, year),
                                    occ_country90) 

# local A index 
a_90_10 = get_A(data = occ_country90_10, unit = "year", occupation = "class2_2010", group = "sex", weight = "n", datamode = "long", local = TRUE) 
a_90_10 = left_join(a_90_10, occ_code00[, c("class2_2010", "class1_2010")], by = c("occupation" = "class2_2010"))

a_90_10$occupation = factor(a_90_10$occupation, levels = unique(occ_code00$class2_2010)[-1])
a_90_10$class1_2010 = factor(a_90_10$class1_2010, levels = rev(unique(occ_code00$class1_2010)[-1]))

a_90_10 %>% 
  group_by(unit) %>% 
  arrange(occupation, .by_group = TRUE) %>%
  write.xlsx("data/a_90_10.xlsx")
  
# A index 
a_global90_10 = get_A(data = occ_country90_10, unit = "year", occupation = "class2_2010", group = "sex", weight = "n", datamode = "long") 

# A index of each major occupation 
a_occ90_10 = a_90_10 %>% 
  group_by(unit, class1_2010) %>% 
  summarise(a = exp(mean((a_local)^2)^(1/2))) %>% 
  ungroup() %>%
  transmute(occupation = class1_2010, index = "a", value = a, year = unit) 

# D index of each major occupation 
d_occ90_10 = lapply(c(1990, 2000, 2010), function(x){
         get_D(data = subset(occ_country90_10, year == x), unit = "class1_2010", occupation = "class2_2010", group = "sex", weight = "n", index = "both")
       }) %>% 
  do.call(rbind.data.frame, .) %>%
  pivot_longer(., cols = 2:3, names_to = "index", values_to = "value") %>%
  transmute(occupation = unit, index = index, value = value, year = rep(c(1990, 2000, 2010), each = 12))

d_a_occ = rbind(a_occ90_10, d_occ90_10) 
d_a_occ$index = factor(d_a_occ$index, levels = c("d", "ds", "a"))
d_a_occ$occupation = factor(d_a_occ$occupation, levels = unique(occ_code00$class1_2010)[-1])

# compare D, Ds, A
showtext_auto()
d_a_occ %>% 
  group_by(index, occupation) %>%
  mutate(value = value/value[year == min(year)]) %>% # standardize value
  ggplot() + 
  geom_line(aes(factor(year), value, group = index, linetype = index), size = rel(0.7)) +
  geom_hline(yintercept = 1, linetype = 2, size = rel(0.4), color = "gray60") + 
  geom_point(aes(factor(year), value), size = rel(1.5)) + 
  scale_linetype_manual(values = c("longdash", "dotted", "solid"), labels = c("D", "Ds", "A"), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(occupation~., nrow = 2, scales = "free_x") + 
  theme_classic() + 
  labs(x = NULL,
       y = "与1990年的百分比(%)") + 
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    #panel.grid.major.y = element_blank(),   
    panel.border = element_rect(colour = "gray70", fill = NA),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(hjust = -0.02, vjust = 1.2, size = rel(1.2), face = "bold"),
    panel.spacing.y = unit(0.5, "lines"),
    axis.line = element_line(size = 0.6),
    axis.ticks.length = unit(-0.1, "cm"),
    axis.title.y = element_text(vjust = -2, size = rel(1.5)),
    axis.text.y = element_text(size = rel(1.5), face = "bold", margin=unit(c(0.5,0.2,0.5,0.5), "cm")),
    axis.text.x = element_text(vjust = -1, size = rel(1.5), face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.5)),
    legend.spacing = unit(0.1,"cm"),
    legend.direction = "horizontal",
    legend.position = "bottom", 
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "white", colour = "gray30")) + 
    guides(linetype = guide_legend(keywidth = 3, keyheight = 1))

ggsave(filename = "fig/chart_d_ds_a.pdf", plot = last_plot(), width = 12, height = 8)

# Dual y axis, not recommended
# # Value used to transform the data
# coeff <- (max(d_a_occ$value[d_a_occ$index=="a"]) / max(d_a_occ$value[d_a_occ$index=="ds"]))
# 
# d_a_occ %>%
#   ggplot(data = ., mapping = aes(x = factor(year))) +
#   geom_line(data = . %>% filter(index != "a"), aes(y = value, linetype = index, group = index)) + 
#   geom_line(data = . %>% filter(index == "a"), aes(y = value / coeff, group = index), linetype = "solid") +
#   geom_point(data = . %>% filter(index == "a"), aes(y = value / coeff, group = index)) +
#   scale_linetype_manual(values = c("longdash", "dotted")) +
#   scale_y_continuous(
#     name = "",  # Features of the first axis
#     sec.axis = sec_axis(~.*coeff, name="a") # Add a second axis and specify its features
#   ) + 
#   facet_wrap(occupation~., nrow = 2) + 
#   theme(
#     axis.title.y = element_text(size=13),
#     axis.title.y.right = element_text(size=13)
#   )

xlsx::write.xlsx(a_90_10 %>% pivot_wider(names_from = "unit", values_from = "a_local"), "data/thesis_table.xlsx", sheetName = "1990_2010", append = TRUE)

# country level 

occup_country82 = read.xlsx('data/occupation_province.xlsx', sheet = "1982")[,-1]
occup_country82 = occup_country82 %>% 
  filter(grepl("\\([0-9]+\\)", occupation), !grepl("不便分类的其他劳动者", occupation)) %>%
  mutate(occupation = trimws(gsub("\\([0-9]+\\)", "", occupation))) %>%
  pivot_longer(., names_to = "sex", values_to = "n", col = 2:3) %>%
  left_join(., occ_code90[, c("class3_1990", "class2_2010", "class1_2010")], by = c("occupation" = "class3_1990"))

which(is.na(occup_country82$class2_2010)) %>% occup_country82$occupation[.] %>% unique(.)

# segregation profile ---------------------------------------------------
# 2010, 2000, 1990
showtext_auto()
p = a_90_10 %>% 
  ggplot(data = .) +
  geom_hline(aes(yintercept = 0), size = rel(1)) + 
  geom_hline(yintercept = c(-2,-1,1,2), linetype = 2, size = rel(0.4), color = "gray70") + 
  geom_line(aes(occupation, a_local, group = unit, linetype = factor(unit)), size = rel(0.5)) +
  geom_point(aes(occupation, a_local)) + 
  scale_linetype_manual(values = c("dotted", "longdash", "solid"))+
  #scale_color_manual(values = c("#fc8d62", "#8da0cb","#66c2a5"))+
  scale_x_discrete(breaks = levels(a_90_10$occupation), labels = 1:63) + 
  scale_y_continuous(breaks = c(-2:2), labels = c(-2:2)) + 
  facet_grid(.~class1_2010, scale ="free" , space = "free",
             switch = "y", # flip the facet labels along the y axis from the right side to the left
             labeller = as_labeller(
               c(`一、国家机关、党群组织、企业、事业单位负责人` = "国家机关、党群组织、\n企业、事业单位负责人", 
                 `二、专业技术人员` = "专业技术人员", 
                 `三、办事人员和有关人员` = "办事人员和\n有关人员",
                 `四、商业、服务业人员` = "商业、服务业人员",
                 `五、农、林、牧、渔、水利业生产人员` = "农、林、牧、渔、\n水利业生产人员",
                 `六、生产、运输设备操作人员及有关人员` = "生产、运输设备操作人员及有关人员")
             )
  ) + 
  labs(x = NULL,
       y = "指数值") +
  theme_classic() +
  theme(strip.background=element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(hjust=0, vjust = 1,angle=90, size = 10, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.spacing.y = unit(1.1, "lines"),
        panel.grid.major.y = element_blank(),   
        panel.grid.minor.y = element_blank(),   
        legend.position = c(0.98, 0.1),
        legend.justification = c(1, 0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, vjust = .5),
        legend.spacing = unit(-0.1,"cm"),
        legend.background = element_rect(fill = "white", colour = "gray30"),
        axis.line = element_line(size = 0.8),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.title.y = element_text(vjust = -2),
        axis.text.y = element_text(size = 10, face = "bold", margin=unit(c(0.5,0.2,0.5,0.5), "cm")),
        axis.text.x = element_text(vjust = -1, size = 14, face = "bold"),
        axis.ticks.x = element_blank()
        ) + 
  #coord_flip() +
  guides(linetype = guide_legend(keywidth = 2.2, keyheight = 1))

p

ggsave(filename = "fig/chart_a90_10horizon.pdf", plot = p, width = 18, height = 9)

p = ggplot(a_90_10, aes(x = class2_2010, y = a_local)) +
  geom_line(aes(group = unit, linetype = factor(unit))) +
  facet_grid(class1_2010 ~ ., scales = "free", space = "free") +
  scale_x_discrete(breaks = levels(a_90_10$class2_2010), labels = 1:63) +
  coord_flip()
p  

# occupation in regions -----------------------------------------------------------------

occ_country0010 = read.xlsx("data/occupation_province.xlsx", sheet = "2010_2000") 

occ_country0010 = occ_country0010 %>% 
  pivot_longer(., names_to = "class2_2010", values_to = "n", col = 6:ncol(.)) %>%
  filter(!grepl("不便分类的|一|二|三|四|五|六", class2_2010), !province %in% c("total")) %>%
  left_join(., city_name00[, c("province", "region.y")] %>% unique(.), by = c("province" = "province"))

occ_country0010$n[is.na(occ_country0010$n)] = 0

a_region = lapply(c(2000, 2010), function(x){
  get_A(data = subset(occ_country0010 %>%
                        group_by(year, region.y, class2_2010, sex) %>%
                        summarise(n = sum(n)), year == x), unit = "region.y", occupation = "class2_2010", group = "sex", weight = "n", datamode = "long", local = TRUE)
}) %>%
  do.call(rbind.data.frame, .) %>% 
  mutate(year = rep(c(2000, 2010), each = 504/2))

a_region %>%
  ggplot(data = .) +
  geom_hline(aes(yintercept = 0), size = rel(1)) + 
  geom_hline(yintercept = c(-2,-1,1,2), linetype = 2, size = rel(0.4), color = "gray70") + 
  geom_line(aes(occupation, a_local, group = year, linetype = factor(year)), size = rel(0.5)) +
  #geom_point(aes(occupation, a_local)) + 
  scale_linetype_manual(values = c("solid", "longdash")) +
  facet_wrap(.~unit) +
  theme_bw()
