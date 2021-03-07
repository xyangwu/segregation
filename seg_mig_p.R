# -- migrantion map ------------------------------------------------------------------
library(showtext)
font <- font_files()
font_add('hanyi', 'HanYiChangSongJian-1.ttf')
font_add('courier', 'cour.ttf', 'courbd.ttf')
font_families()
theme_base <- list(theme(panel.grid.minor = element_blank(), # theme #
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.title = element_text(colour='#08306B',size = 10, family = "hanyi",face = 'bold',lineheight = 1.2),
                         legend.position = c(0, 1),
                         legend.justification = c(0,1),
                         legend.background = element_rect(fill= alpha('white', 0)),
                         legend.key.size = unit(0.3, "cm"),
                         legend.text = element_text(size = 10, hjust = 1.3, face = 'bold'),
                         legend.spacing.y = unit(c(1, 0, 0, 0), 'lines'),
                         plot.title = element_text(size=16,  family="hanyi",face="bold"),
                         plot.subtitle=element_text(size=12, family="hanyi",face="bold"),
                         plot.margin = unit(c(1, 0, 0, 0), 'lines')
)
)

mig_pop <- sh.b1 +
    geom_polygon(data = test%>%filter(year.y==2000|year.y==2003|year.y==2006|year.y==2009|year.y==2012|year.y==2017), 
               aes(x = long, y = lat, group = group, fill = mig_prop),
               alpha = 0.5) + 
  
  geom_path(data = test%>%filter(year.y==2000|year.y==2003|year.y==2006|year.y==2009|year.y==2012|year.y==2017),
            aes(x = long, y = lat, group = group), 
            colour='grey40', alpha=0.5, lwd= 1) +
  
  scale_fill_continuous(low = "#9ECAE1", high = "red", name=" 外来人口在常\n住人口中占比") + # add interesting scale
  
  facet_wrap(~ year.y) +
  
  labs(title = NULL,
       subtitle = "历年各县区外来人口占比"
       #caption = 'Data source: 上海市统计年鉴'
  ) +
  
  theme_base

export::graph2ppt(mig_pop, 'export/mig_pop.pptx')
# 2003年以前闵行区的外来人口占比最高，随后几年外来人口逐渐流向更为外围的地区。青浦、嘉定、松江等区县的外来人口。十分明显的一点是，中

# 心城区的外来人口比例从未超过10%，即使在最高的年份也是%。


length(test)
viridis::viridis_pal()(10)
scales::show_col(viridis_pal()(40))
viridis::scale_fill_viridis()


# -- education level map -------------------------------------------------------------
edu_map <- sh.b1 +
    geom_polygon(data = test,
               aes(x = long, y = lat, group = group, fill = 专科), alpha = 0.5) + 
  
  geom_path(data = test,
            aes(x = long, y = lat, group = group), 
            colour='grey40', alpha=0.5, lwd= 1) +
  
  scale_fill_viridis(name=" 高等教育人口在劳\n动力人口中占比") + # add interesting scale
  
  labs(title = NULL,
       subtitle = "2010拥有专科以上文凭的劳动力占比"
       #caption = 'Data source: 上海市统计年鉴'
  ) +
  theme_base

export::graph2ppt(edu_map, 'export/edu_map.pptx')
