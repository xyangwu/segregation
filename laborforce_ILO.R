library(tidyverse)
library(showtext)
library(openxlsx)

agegroup = read.xlsx('data/agegroup.xlsx', sheet = 1)
ilostat = read.csv("data/ilostat-cn.csv")
ilostat_est = read.csv("data/ilostat-cn-estimate.csv")

names(agegroup)
names(ilostat)
unique(agegroup$year) %>% sort(.)

ilostat$sex.label = gsub("Sex: ", "", ilostat$sex.label)
ilostat_est$sex.label = gsub("Sex: ", "", ilostat_est$sex.label)

## LFPR(labour participation rate) --------------------
## 劳动参与率的变化
showtext_auto()
ilostat %>%
  filter(sex.label %in% c("Male", "Female") & classif1.label %in% c("Age (5-year bands): Total")) %>%
  ggplot(aes(time, obs_value, linetype = sex.label)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = unique(ilostat$time), 
                     labels = unique(ilostat$time)) +
  scale_y_continuous(breaks = seq(60, 90, 5), 
                     labels = paste0(seq(60, 90, 5), ".0"),
                     limits = c(60, 90)) +
  ylab("劳动参与率(%)") +
  xlab("") +
  scale_colour_discrete(name = "性别", 
                        breaks = c("Male", "Female"), 
                        labels = c("男", "女")) +
  scale_linetype_discrete(name = "性别",
                          breaks = c("Male", "Female"), 
                          labels = c("男", "女")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = rel(1.2)))

ggsave( "fig/lfpr_census.pdf", width = 7, height = 6)


## estimates
ilostat_est %>%
  filter(sex.label %in% c("Male", "Female") & classif1.label %in% c("Age (10-year bands): Total", "Age (Youth, adults): 15-64")) %>%
  select(time, sex.label, classif1.label, obs_value) %>%
  ggplot(aes(time, obs_value, colour = sex.label, linetype = classif1.label)) +
  geom_line()


## marriage's influence on female LFPR --------------------
## 婚姻状况(以25岁为界限)对劳动力参与率的影响
ilostat_est %>%
  filter(sex.label %in% c("Male", "Female") & classif1.label %in% c("Age (10-year bands): 15-24", "Age (Youth, adults): 25+")) %>%
  select(time, sex.label, classif1.label, obs_value) %>%
  ggplot(aes(time, obs_value, linetype = classif1.label)) +
  geom_line() +
  #geom_text(aes(label = round(obs_value, 3))) +
  xlim(1980, 2020) +
  ylab("劳动参与率") +
  xlab("") +
  facet_wrap("sex.label") +
  theme_classic() +
  #scale_colour_discrete(name = "性别", 
   #                  breaks = c("Male", "Female"), 
    #                 labels = c("男", "女")) +
  scale_linetype_discrete(name = "年龄",
                      breaks = c("Age (10-year bands): 15-24", "Age (Youth, adults): 25+"), 
                      labels = c("15-24", "25+"))

ilostat_est$obs_value[ilostat_est$time == 2005 & ilostat_est$classif1.label ==  "Age (10-year bands): Total"]


