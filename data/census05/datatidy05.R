library(dplyr)
library(data.table)

setwd("C:/Users/WXY/Documents/R_learning/segregation/")

# load in data ---------------------------------------

# 家庭 
data_05 <- haven::read_dta('data/census05/data_num.dta', encoding = "utf-8") 
data_05 <- as.data.table(data_05)

##' R19.上周工作情况
##' 10月25-31日是否为取得收入而从事了一小时以上的劳动：
##' 1. 是 上周工作时间：小时
##' 2.在职休假、学习、临时停工或季节性歇业未工作
##' 3.未做任何工作→R26"	

data_05 %>%
  filter(age >= 16) %>%
  count(r19) %>%
  mutate(prop = n/sum(n))
			
## R26 上周未工作原因 
## 1.在校学习 →R30 
## 2.丧失劳动能力→R29 
## 3.离退休
## 4.料理家务
## 5.毕业后未工作
## 6.因单位原因失去原工作
## 7.因本人原因失去原工作
##' 8.承包土地被征用
##' 9.其他
##' 
##' R27.三个月内是否找过工作 	
##' 1.在职业介绍机构求职
##' 2.委托亲友找工作
##' 3.参加招聘会
##' 4.应答或刊登广告
##' 5.为自己经营做准备
##' 6.其他
##' 7.未找过工作
##' 
##' R28.能否工作 如有工作机会能否在两周内开始工作 
##' 1.能 连续未工作时间： 个月
##' 2.不能 

## reasons for unemployment 

data_05 %>% 
  filter(r19 %in% c(3), age >= 16) %>%
  count(r26) %>%
  mutate(total = sum(n), prop = n/sum(n))
"在校学生 丧失 离退休 料理家务 正在 其他"
c(688894, 844895, 700982, 1052963, 259584, 483884)/sum(c(4031202))

## seeking for jobs 

data_05 %>% 
  filter(age >= 16) %>%
  count(r27) %>%
  mutate(prop = n/sum(n))

## r28 能在两周内开始工作的人口
data_05 %>% 
  filter(age >= 16, r19 %in% c(3), r26 %in% c(3:9), r27 %in% c(1:6)) %>%
  count(r28) %>% 
  mutate(total = sum(n), prop = n/sum(n)) 


# unemeployment rate ----------------------------------
table(data_05$r11) 
unemp = nrow(data_05[age >= 16 & r19 %in% c(3) & r26 %in% c(3:9) & r27 %in% c(1:6) & r28 %in% c(1), ])
emp = nrow(data_05[age >= 16 & r19 %in% c(1, 2),])
unemp/(unemp + emp)

head(data_05$r7_03)
259584/(259584+9287409)


