
# shanghai pop --------------------------------------------------------------------------
sh.b1 <- readRDS('data/sh_b1.RDS')
sh_df_broader <- readRDS('F:/articles/sh_df_broader.RDS')

sh_pop <- xlsx::read.xlsx('F:/Lab/R learning/segregation/data/上海各区人口.xlsx', sheetName = 'sh_pop', encoding = 'UTF-8')[, -c(7:8)]
readr::guess_encoding(sh_pop)
library(tidyverse)
sh_pop$district <- as.character(sh_pop$district) %>% str_trim()
sh_pop$admin_area <- as.character(sh_pop$admin_area) %>% str_trim() %>% as.numeric()
sh_pop$pop_live <- as.character(sh_pop$pop_live) %>% str_trim() %>% as.numeric()
sh_pop$pop_mig <- as.character(sh_pop$pop_mig) %>% str_trim() %>% as.numeric()
sh_pop$pop_dens <- as.character(sh_pop$pop_dens) %>% str_trim() %>% as.numeric()

sh_pop$district[which(sh_pop$district=='南汇区')] <- '浦东新区'
sh_pop$district[which(sh_pop$district=='卢湾区')] <- '黄浦区'
sh_pop$district[which(sh_pop$district=='闸北区')] <- '静安区'

sh_pop_1 <- sh_pop %>% group_by(year, district) %>% mutate(admin_area = sum(admin_area),  pop_live = sum(pop_live), pop_mig = sum(pop_mig)) %>% ungroup()
sh_pop_1 <- sh_pop_1[!duplicated(sh_pop_1[, c('year', 'district')]), ]
sh_pop_1$pop_dens <- sh_pop_1$pop_live/sh_pop_1$admin_area*10000
sh_pop_1$mig_prop <- sh_pop_1$pop_mig/sh_pop_1$pop_live
test <- left_join(sh_df_broader, sh_pop_1, by = c('district' = 'district'))
# -- level of education ------------------------------------------------------------
edu_level <- xlsx::read.xlsx('F:/Lab/R learning/segregation/data/劳动力结构.xlsx', sheetName = 'edu_level', encoding = 'UTF-8')
edu_level$district <- as.character(edu_level$district)
test <- left_join(test, edu_level, by = c('district' = 'district'))
test <- left_join(test, edu_level[,-2], by = c('district' = 'district'))
# -- infrastractrue, location -------------------------------------------------------------
data13 <- readRDS('data/data13.rds')
garbage <- readRDS('data/garbage.rds')
db.village <- readRDS('data/db.village.rds') #refer to the community the outlier belongs to
commu_obsv_df <- cbind.data.frame(garbage, data13[-c(530, 663), !names(data13)%in%names(garbage)])

# -- SUNS data ----------------------------------------------------------------------
suns_df <- haven::read_stata('F:/articles/garbage.proj/data/wastesort.dta', encoding = "GB18030")

suns_label <- data.frame(variable = names(suns_df), label = Hmisc::label(suns_df), stringsAsFactors = F, row.names = 1:72)
# migrant population
suns_df$migrant <- NA  
suns_df$migrant[which(nchar(suns_df$shhukouplcode)==12)] <- 0
suns_df$migrant[which(nchar(suns_df$shhukouplcode)!=12)] <- 1
suns_df %>% count(migrant)
# age groups
library(sjlabelled)  
suns_df$age2 <- car::Recode(suns_df$age, "15:19 ='15-19'; 20:24='20-24'; 25:29= '25-29'; 30:34='30-34';35:39='35-39';40:44='40-44'; 45:49='45-49'; 50:54='50-54';
                            55:59='55-59'; 60:64='60-64'; 65:69='65-69';70:74='70-74';75:79='75-79';80:84='80-84';85:89='85-89';90:94='90-94';95:99='95-99';100:104='100-104'")
# recode educational level
suns_df$edu_lev <- car::Recode(remove_all_labels(suns_df$ysch), "0:6 ='0-6'; 7:12='7-12'; 13:16= '13-16'; 17:22='17-22';998:999=NA")
suns_df$edu_lev[suns_df$pp_M1Edu==1] <- '0-6'
suns_df$edu_lev[suns_df$pp_M1Edu==2|suns_df$pp_M1Edu==3|suns_df$pp_M1Edu==4|suns_df$pp_M1Edu==5|suns_df$pp_M1Edu==6|suns_df$pp_M1Edu==7] <- '7-12'
suns_df$edu_lev[suns_df$pp_M1Edu==8|suns_df$pp_M1Edu==9] <- '13-16'
suns_df$edu_lev[suns_df$pp_M1Edu==10] <- '17-22'
suns_df$edu_lev <- factor(suns_df$edu_lev, levels = c('0-6', '7-12', '13-16', '17-22'))
which(is.na(suns_df$edu_lev))
sum(is.na(suns_df$edu_lev));suns_df$pp_M1Edu[which(is.na(suns_df$edu_lev))] # check
# housework time
suns_df$housewk <- car::Recode(remove_all_labels(suns_df$chore5min), "0:30 ='1'; 31:60='2'; 61:120= '3'; 120:999='4';NA='NA'")
# edu level
suns_df %>% count(edu_lev)  %>% na.omit() %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
# gender edu
male_edu <- suns_df %>% filter(pp_Gender==1) %>% count(edu_lev) %>% na.omit() %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
female_edu <- suns_df %>% filter(pp_Gender==5) %>% count(edu_lev) %>% na.omit() %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(female_edu, 'export/wastesort.xlsx', sheetName = 'female_edu', append = T)

hs_female %>% count(edu_lev) %>% na.omit() %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
hs_female[which(hs_female$age < 50),] %>% nrow()

# edu_lev by gender, housewk
male_hsw_edu <- suns_df %>% filter(pp_Gender==1) %>% group_by(housewk) %>% count(edu_lev) %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
female_hsw_edu <- suns_df %>% filter(pp_Gender==5) %>% group_by(housewk) %>% count(edu_lev) %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(male_hsw_edu, 'export/wastesort.xlsx', sheetName = 'male_hsw_edu', append = T)
# housewk by gender, edu_lev
male_edu_hsw <- suns_df %>% filter(pp_Gender==1) %>% group_by(edu_lev) %>% count(housewk) %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
female_edu_hsw <- suns_df %>% filter(pp_Gender==5) %>% group_by(edu_lev) %>% count(housewk) %>% mutate(sum = sum(n)) %>% mutate(percent = n/sum) %>% ungroup()
xlsx::write.xlsx(female_edu_hsw, 'export/wastesort.xlsx', sheetName = 'female_edu_hsw', append = T)

dim(hs_female)