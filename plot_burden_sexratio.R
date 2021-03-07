library(tidyverse)
library(RColorBrewer)
library(showtext)
library(ggrepel)

######################## read in data ########################
ocp_sex <- openxlsx::read.xlsx('C:/Users/WXY/Downloads/1_按中类分.xlsx', sheet=1)
pop_age <- openxlsx::read.xlsx('data/按年龄分类.xlsx',sheet=1)

######################## tidy data ########################
ocp_sex$地区 <- str_trim(ocp_sex$地区, side = c("right"))
ocp_sex <- ocp_sex[!grepl(" ", ocp_sex$地区), c(1:3)]
ocp_sex$sex <- NA
ocp_sex$sex[grep("女|男|总计", ocp_sex$地区)] <- ocp_sex$地区[grep("女|男|总计",ocp_sex$地区)]
ocp_sex$sex <- zoo::na.locf(ocp_sex$sex)
ocp_sex <- ocp_sex[!duplicated(ocp_sex[, c("地区", "省", "sex")]), ]

names(data10_city)
emp_female <- ocp_sex %>% 
  filter(city%in%citysample) %>%
  distinct(city, sex) %>%
  tidyr::pivot_wider(., names_from = "sex", values_from = "pop") %>%
  mutate(f_ratio = (f/f + m ))


care_ratio <- pop_age %>%
  select(c(1, 9:11))
care_ratio$city <- care_ratio$city %>% str_squish(.)

f_ratio <- left_join(emp_female[, c("地区", "f_ratio")], care_ratio, 
                        by = c("地区" = "city")) %>% 
  pivot_longer(., cols = c(3:5), names_to = "group", values_to = "percent") %>%
  mutate(colour = case_when(
    group == "老年抚养比" ~ "#FF7F00",
    group == "幼儿抚养比" ~ "#4DAF4A",
    group == "抚养比" ~ "#6BAED6",
    T ~ "#5d5e62")
  ) %>%
  unnest()

#------------------------- 3.5 correlation care rate, sex ratio,D and Ds ------------------------- 
cor.test(city_D_Ds$幼儿抚养比, city_D_Ds$f_ratio,method = "pearson") #0.42685
cor.test(city_D_Ds$老年抚养比,city_D_Ds$f_ratio,method = "pearson") #0.42685
cor.test(city_D_Ds$抚养比,city_D_Ds$f_ratio,method = "pearson") #0.42685

showtext_auto()
ggplot(f_ratio,
       aes(percent, f_ratio, group = group, color = colour)) +
  geom_point(size = 2, 
             stroke = 1,
             alpha = 0.5) +
  geom_smooth(aes(group = group, color = colour),
              method = "glm", 
              se = F, 
              linetype = "dashed",
              size = 1) +
  scale_color_identity(guide = "legend") +
  scale_fill_identity() +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_y_continuous(breaks = seq(0.3, 0.55, 0.05), 
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0.3, 0.55)
                     ) +
  scale_x_continuous(breaks = seq(0, 0.7, 0.1), 
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.7)) + 
  labs(title = "女性从业者比例与抚养比",
       subtitle = "2010人口普查",
       x = "抚养比",
       y = "就业人口中女性比例") +
  theme_classic()

ggsave( "export/care_rate.pdf", width = 7, height = 6)
