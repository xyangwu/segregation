# codes for computing fines 

finerate = 0.3
year = 14
discount = 0.02
#fine = finerate*(1-discount)^0
fine = 0
for(i in c(1:year)){
 fine0 = finerate*((1-discount)^i)
 fine = fine0 + fine
}

fine

finerate + finerate*13*0.98

# guangdong 20% 7 1.294398
# guangdong 10% 14 1.20625
# beijing 10% 7 .6471991
# tianjing 20% 5 0.9426

((1.20625-0.1)/0.098)

# read in fines data ------------------------------------------------------------------------------------------
# 1990, 2000, 2010

# data from Epstein
fines_ep = haven::read_dta("data/fines.dta") 
fines_prefect = haven::read_dta("data/china2000_prefect.dta") 

names(fines_prefect)[5] = "policy2"

fines_ep = subset(fines_ep, birthyear %in% c(1990, 2000, 2010))
fines_ep = fines_ep[, c("birthyear", "province", "fine", "policy")]
fines_ep$province = as.character(fines_ep$province) 

# collected by author 
fines2010 = read.xlsx("data/family_fine.xlsx", sheet = "policy2010")

fines2010$province = as.character(fines2010$province)

fines = left_join(fines2010, unique(fines_ep[, c("province", "policy")]), by = c("province" = "province")) 
fines = fines2010 %>%
  transmute(birthyear = year, province, fine = (fine1_min+fine1_max)*0.5) %>%
  rbind(fines_ep %>% select(birthyear, province, fine), .) %>% 
  arrange(province) %>% 
  left_join(., unique(fines_ep[, c("province", "policy")])) %>%
  left_join(., fines2010[, c("province", "province_cn")])

fines$fine[fines$province_cn == "西藏" & fines$birthyear == 2010] = 0.5
# correct fines in Jilin province
fines$fine[fines$province_cn == "吉林" & fines$birthyear %in% c(1990, 2000)] = 3.621463

showtext::showtext.auto()
fines %>% 
  ggplot() + 
  geom_line(aes(factor(birthyear), fine, group = province_cn)) +
  facet_wrap(.~province_cn) +
  theme_bw() + 
  labs(x = NULL,
       y = "征收标准（年收入倍数）") 
ggsave("fig/chart_fines_province.pdf")
