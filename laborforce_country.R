library("openxlsx")
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

# fertility at state level
fertility_age = read.xlsx('data/description.xlsx', sheet = 1)
fertility_age = as.data.table(fertility_age)
fertility_age$`平均育龄妇女人数(人)` <- as.numeric(fertility_age$`平均育龄妇女人数(人)`)
fertility_age$出生人数 <- as.numeric(fertility_age$出生人数)
fertility_age$year = as.integer(fertility_age$year)
fertility_year = read.xlsx('C:/Users/WXY/Desktop/employment.xlsx', sheet = 2)

# reasons for unemployment
unemp_reason = read.xlsx('data/description.xlsx', sheet = 3)
unemp_reason = unemp_reason[, -12:-length(unemp_reason)] 
unemp_reason[is.na(unemp_reason)] = 0

# reasons for unemployment --------------------------------------------
labels = c("失业人员", "正在上学", "毕业后未工作", "单位原因", "个人原因", "承包土地被征用", "离退休", "料理家务", "其他")

unemp_reason_long = unemp_reason %>%
  pivot_longer(cols = 4:ncol(.),
               names_to = "reason",
               values_to = "percent")

level = subset(unemp_reason_long, age == "total" & sex %in% c("female")) %>% arrange(percent) %>% select(reason) %>% unlist()

unemp_reason_long$reason = factor(unemp_reason_long$reason, levels = rev(level))
unemp_reason_long$percent = as.numeric(unemp_reason_long$percent)

showtext::showtext_auto() 

## unemployment due to housekeeping //家务原因
unemp_reason_long %>% 
  filter(year >= 2005, grepl("total", age), sex %in% c("female", "male"), reason %in% c("料理家务"))  %>% 
  ggplot(aes(year, percent)) + 
  geom_line(aes(group = sex, linetype = sex)) +
  geom_point(aes(shape = sex)) +
  theme_bw() + 
  labs(x = NULL,
       y = "百分比(%)") + 
  ylim(c(0, 50)) +
  scale_linetype_manual(values = c("solid", "longdash"), labels = c("女", "男"), name = NULL) +
  scale_shape_discrete(labels = c("女", "男"), name = NULL) + 
  scale_x_continuous(limits = c(2005, 2018), breaks = seq(2005, 2018, 1), labels = seq(2005, 2018, 1)) +
  theme(axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 12, margin=unit(c(0.5,0.2,0.5,0.2), "cm")),
        axis.text.x = element_text(vjust = 1, size = 12),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = rel(1.1)),
        legend.spacing = unit(0.1,"cm"),
        legend.direction = "vertical",
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", colour = "gray30")) + 
  guides(linetype = guide_legend(keywidth = 2.2, keyheight = 1)) 

ggsave( "fig/unemployment_housework.pdf", width = 9, height = 6)

unemp_reason_long %>% 
  filter(year >= 2005, grepl("-", age), sex %in% c("female", "male"), reason %in% c("料理家务"))  %>% 
  ggplot(aes(factor(year), percent, fill = sex)) +
  geom_line(aes(group = sex)) +
  facet_wrap(.~age) +
  theme_bw()

## reasons for unemployment //不在工作的原因
unemp_reason_long %>% 
  filter(age == "total" & sex %in% c("female", "male")) %>% 
  ggplot(aes(reason, percent, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", colour  = "black") + 
  theme_classic() +
  scale_fill_manual(aesthetics = "fill", values = c("#fc8d59","#91bfdb"), 
                    name = "性别",
                    breaks=c("female", "male"),
                    labels=c("女", "男")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45)) + 
  theme_classic() + 
  labs(x = NULL,
       y = "百分比(%)") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.border = element_rect(colour = "gray20", fill = NA),
    axis.line = element_line(size = 0.6),
    axis.ticks.length = unit(-0.1, "cm"),
    axis.title.y = element_text(vjust = -2, size = rel(1.5)),
    axis.text.y = element_text(size = rel(1.2), margin=unit(c(0.5,0.2,0.5,0.5), "cm")),
    axis.text.x = element_text(size = rel(1.2), margin=unit(c(0.5,0.2,0.5,0.5), "cm")),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1)),
    legend.position = c(0.6, 0.9),
    legend.justification = c(1, 0.5), 
    legend.direction = "horizontal",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "white", colour = "gray30"))

datareason %>%
  ggplot(.) +
  geom_segment( aes(x=reason, xend=reason, y=female, yend=male), color="grey40") +
  geom_point( aes(x=reason, y=male), color = "black", fill = "black", size=3 ) +
  geom_point( aes(x= reason, y=female), color = "grey90", fill = "black", size=3 ) +
  coord_flip()+
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    legend.position = "none",
  ) +
  xlab("") + 
  ylab("百分比(%)")

ggsave( "fig/unemployment_housework.pdf", width = 9, height = 6)



# total fertility rate by year -----------------------------------------
fertility_age %>%
  filter(age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) %>%
  select(year, age, `平均育龄妇女人数(人)`, `出生人数`) %>%
  mutate(fertirate = `出生人数`/`平均育龄妇女人数(人)`) %>%
  group_by(year) %>%
  summarise(tfr = sum(fertirate)*5) %>%
  write.xlsx(., "data/fertility_year.xlsx")

fertility_year %>%
  filter(age %in% c("15-49")) %>%
  ggplot(aes(year, TFR)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = sort(unique(fertility_year$year)))


# LFP and TFR 1990 --------------------------------------------

## diffenrence between female and male //劳动参与率的性别差异
p = lfpr_tfr_90 %>%
  filter(sex %in% c("f", "m")) %>%
  ggplot(aes(tfr, partici_rate, colour = sex, shape = sex,
             text = paste(province, " ", name_cn10, "\n",
                          "TFR: ", round(tfr, 3), "\n",
                          "LFPR: ", scales::percent(partici_rate, accuracy = 2), "\n",
                          sep = "")
  )) +
  geom_point(size = 3, alpha = 0.6) +
  xlim(0.15, 3.5) +
  ylim(0.35, 1) +
  ylab("劳动参与率") +
  xlab("总和生育率") +
  scale_shape_manual(name = "性别", values= c(16, 17), 
                     breaks = c("f", "m"), 
                     labels = c("女", "男")) +
  scale_colour_manual(name = "性别", values = c("#F8766D", "#00BFC4"),
                      breaks = c("f", "m"), 
                      labels = c("女", "男")) +
  theme_classic()

p
p = ggplotly(p, tooltip = "text")
htmlwidgets::saveWidget(as_widget(p), "fig/TFR_LFPR.html")
# unique(ggplot_build(p)$data[[1]]["colour"]) # extract colour code

# LFP and TFR 2000 --------------------------------------------

# difference between regions //劳动参与率的地区差异

partici_rate_00 %>%
  filter(SEX %in% c("f")) %>%
  ggplot(aes(tfr, partici_rate, colour = region.y, shape = region.y)) +
  geom_point(size = 2, alpha = 0.7) +
  xlim(0.15, 3.5) +
  ylim(0.35, 1) +
  ylab("劳动参与率") +
  xlab("总和生育率") +
  facet_wrap(c("region.y")) +
  theme_classic() +
  scale_shape_discrete(name = "地区") +
  scale_colour_discrete(name = "地区")

ggsave( "fig/tfr_lfpr_region.pdf", width = 7, height = 6)
table(partici_rate_00$region.y)
table(partici_rate_00[, c("region.y", "province")])

## difference in years //劳动参与率的年份差异

ggplot(data = rbind(partici_rate_00 %>% 
                      filter(SEX %in% c("f")) %>%
                      mutate(year = "2000") %>%
                      select(year, tfr, partici_rate),
                    partici_rate10 %>% 
                      filter(sex %in% c("女")) %>%
                      mutate(year = "2010") %>%
                      select(year, tfr, partici_rate)),
       aes(tfr, partici_rate, colour = year)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_colour_manual(name = "时间", values= c("red", "grey30"), 
                      labels = c("2000", "2010")) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0.15, 3.5) +
  ylim(0.35, 1) +
  ylab("劳动参与率") +
  xlab("总和生育率") +
  theme_classic()

ggsave( "fig/tfr_lfpr_year.pdf", width = 7, height = 6)


# LFP and TFR 2010 ---------------------------------------------

## lfp in 2010 //2010年生育率

lfpr_tfr_10 = lfp_10 %>% 
  mutate(lfpr = `经济活动人口`/`16岁及以上人口`) %>%
  filter(grepl("  ", city) == FALSE & !city%in%c("男", "女", "合计"), sex %in% c("m", "f", "合计")) %>%
  select(city, province, sex, lfpr) %>%
  left_join(., tfr_10 %>% 
              mutate(tfr = `总和生育率`/1000) %>% 
              select(city, tfr), by = c("city" = "city")) %>%
  left_join(., unique(city_list[, c("province_name", "region.x", "region.y")]), by = c("province" = "province_name"))

## diffenrence between female and male
lfpr_tfr_10 %>% 
  filter(sex %in% c("m", "f")) %>%
  ggplot(aes(tfr, partici_rate, colour = sex, shape = sex)) +
  geom_point(size = 3, alpha = 0.6) + 
  # add labels 
  #geom_text(data = partici_rate10 %>% 
  #            filter(city %in% c("北京市", "上海市") & sex %in% c("男", "女")),
  #        aes(tfr, partici_rate, label = city)) +
  xlim(0.15, 3.5) +
  ylim(0.35, 1) +
  ylab("劳动参与率") +
  xlab("总和生育率") +
  scale_shape_manual(name = "性别", values= c(16, 17), 
                     breaks = c("f", "m"), 
                     labels = c("女", "男")) +
  scale_colour_manual(name = "性别", values = c("#F8766D", "#00BFC4"),
                      breaks = c("f", "m"), 
                      labels = c("女", "男")) +
  theme_classic()

ggsave( "fig/tfr_lfpr_fm.pdf", width = 7, height = 6)

## difference between regions
lfpr_tfr_10 %>%
  filter(sex %in% c("女")) %>%
  ggplot(aes(tfr, partici_rate, colour = region.y, shape = region.y)) +
  geom_point(size = 2, alpha = 0.7) +
  xlim(0, 3) +
  ylim(0.35, 1) +
  facet_wrap(c("region.y"))

unique(lfpr_tfr_10$city)[!unique(lfpr_tfr_10$city) %in% unique(lfpr_tfr_10$name_cn10)]

lfpr_tfr_10[lfpr_tfr_10$tfr > 2.5, ]
cor.test(x = lfpr_tfr_10$tfr[lfpr_tfr_10$sex == "女" & lfpr_tfr_10$province != "海南"], y = lfpr_tfr_10$partici_rate[lfpr_tfr_10$sex == "女"& partici_rate10$province != "海南"])

names(tfr_total)


# LFP and TFR in 1990&2000 & 2010 ---------------------------------------------

## diffenrence between female and male

lfpr_tfr_hmnz = rbind(lfpr_tfr_90 %>% transmute(name_harmonized, sex, lfpr, tfr, year = 1990),
                      lfpr_tfr_10 %>% transmute(name_harmonized, sex, lfpr, tfr, year = 2010), 
                      lfpr_tfr_00_hmnz %>% transmute(name_harmonized, sex, lfpr, tfr, year = 2000))

which(lfpr_tfr_10_hmnz$tfr == max(lfpr_tfr_10_hmnz$tfr)) %>% lfpr_tfr_10_hmnz[.,]

lfpr_tfr_hmnz %>%
  filter(sex %in% c("f", "m")) %>%
  ggplot(aes(tfr, lfpr, colour = sex, shape = sex)) +
  geom_point(size = 2, alpha = 0.6) + 
  # add labels 
  #geom_text(data = partici_rate10 %>% 
  #            filter(city %in% c("北京市", "上海市") & sex %in% c("男", "女")),
  #        aes(tfr, partici_rate, label = city)) +
  xlim(0.5, 5.4) +
  ylim(0.2, 1) + 
  ylab("劳动参与率") +
  xlab("总和生育率") +
  scale_shape_manual(name = "性别", values= c(16, 17), 
                     breaks = c("f", "m"), 
                     labels = c("女", "男")) +
  scale_colour_manual(name = "性别", values = c("#F8766D", "#00BFC4"),
                      breaks = c("f", "m"), 
                      labels = c("女", "男")) + 
  theme_classic() +
  facet_wrap(vars(year))

## boxplot
lfpr_tfr_hmnz %>%
  filter(sex %in% c("f", "m")) %>%
  ggplot(aes(sex, lfpr, colour = sex)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(breaks = c("f", "m"), labels = c("女", "男")) +
  #scale_color_manual(values = c("lightcoral", "lightskyblue")) +
  facet_wrap(vars(year)) +
  labs(x = NULL,
       y = "劳动参与率") +
  theme_bw() + 
  theme(text = element_text(family = "stsong", size = 18),
        panel.grid = element_blank())

ggsave("fig/lfpr_year_census.pdf", width = 10, height = 8)


ggsave( "fig/tfr_lfpr2.pdf", width = 9, height = 6)

