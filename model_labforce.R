# 2SLS ---------------------------------------------------------------

library(AER)
dataiv10 = subset(lfpr_tfr_10_hmnz, sex == "f"& !province %in% c("西藏","青海", "新疆"))
dataiv00 = subset(lfpr_tfr_00, sex == "f"& !province %in% c("西藏","青海", "新疆"))
dataiv90 = subset(lfpr_tfr_90, sex == "f"& !province %in% c("西藏","青海", "新疆"))

plot(dataiv00$tfr, dataiv10$tfr)

dataiv10$name_harmonized[dataiv10$tfr > 2.5]

# perform the first stage regression
tfr_s1 <- lm(tfr ~ lfpr, data = dataiv10)
coeftest(tfr_s1, vcov = vcovHC, type = "HC1")
summary(tfr_s1)

# run the stage 2 regression
tfr_s2 <- lm(dataiv00$tfr ~ tfr_s1$fitted.values)
coeftest(tfr_s2, vcov = vcovHC) 
summary(tfr_s2)

# perform TSLS using 'ivreg()'
tfr_ivreg <- ivreg(lfpr ~ tfr | prop_minor, data = dataiv10)
coeftest(tfr_ivreg, vcov = vcovHC, type = "HC1")
summary(tfr_ivreg)

## lfpr 

# perform TSLS using 'ivreg()'
tfr_ivreg <- ivreg(tfr ~ lfpr | exp_edu, data = dataiv10)
coeftest(tfr_ivreg, vcov = vcovHC, type = "HC1") 
summary(tfr_ivreg)

# 2 periods

# compute the differences 
diff_tfr <- (dataiv10$tfr + dataiv00$tfr)/2
diff_lfpr <- dataiv10$lfpr - dataiv00$lfpr

diff_tfr <- dataiv10$tfr - dataiv90$tfr
diff_lfpr <- dataiv10$lfpr - dataiv90$lfpr

diff_tfr <- dataiv00$tfr - dataiv90$tfr
diff_lfpr <- dataiv00$lfpr - dataiv90$lfpr

plot(diff_tfr, diff_lfpr)

range(diff_tfr) 
which(abs(diff_tfr) > 1) %>% dataiv10$name_harmonized[.]

hist(diff_lfpr)
cor.test(diff_tfr, diff_lfpr) 

# estimate a regression using differenced data
diff_mod <- lm(diff_lfpr ~ diff_tfr)

coeftest(diff_mod, vcov = vcovHC, type = "HC1") 

plot(dataiv00$tfr, dataiv00$lfpr)


# housep/prop_minor -> tfr -> lfpr; 
mod1 = lm(lfpr ~ prop_minor, data = subset(lfpr_tfr_10_hmnz, sex == "f"))
mod2 = lm(lfpr ~ tfr, data = subset(lfpr_tfr_10_hmnz, sex == "f"))
mod3 = lm(lfpr ~ prop_minor + , data = subset(lfpr_tfr_10_hmnz, sex == "f"))
summary(mod1); summary(mod2)
summary(mod3) 

## female 2010 
fit1 = lm(lfpr ~ middle_high_edu, data = lfpr_tfr_10_hmnz %>% filter(sex == "f"))
fit3 = lm(lfpr ~ tfr, data = lfpr_tfr_10_hmnz %>% filter(sex == "f"))
fit4 = lm(lfpr ~ middle_high_edu + I(middle_high_edu^2), data = lfpr_tfr_10_hmnz %>% filter(sex == "f"))
fit5 = lm(lfpr ~ tfr+middle_high_edu + I(middle_high_edu^2), data = lfpr_tfr_10_hmnz %>% filter(sex == "f"))
summary(fit3)
stargazer::stargazer(fit3, fit1, fit4, fit5, align = T, type = "html", out = "fig/fit_table3.html")

## male 2010
fit3 = lm(lfpr ~ middle_high_edu, data = lfpr_tfr_10_hmnz %>% filter(sex == "m"))
fit4 = lm(lfpr ~ middle_high_edu + I(middle_high_edu^2), data = lfpr_tfr_10_hmnz %>% filter(sex == "m"))

## female 1990
fit5 = lm(lfpr ~ tfr, data = lfpr_tfr_90_hmnz %>% filter(sex == "f"))
fit6 = lm(lfpr ~ tfr+I(tfr^2), data = lfpr_tfr_90_hmnz %>% filter(sex == "f"))
fit7 = lm(lfpr ~ tfr+middle_high_edu+I(middle_high_edu^2), data = lfpr_tfr_90_hmnz %>% filter(sex == "f"))

stargazer::stargazer(fit1, fit2, fit3, fit4, type = "html", out = "fig/fit_table.txt")

# panel data ------------------------------

data_00_10 = rbind(lfpr_tfr_10_hmnz[, c("name_harmonized", "lfpr", "tfr", "nonagri", "middle_high_edu", "housep",
                                        "exp_edu","eduyear_f","sex", "gdp_percap", "gdp_percapconst", "gdp23_prop", "prop_minor", 
                                        "extend_fam", "eduyear", "province", "region.x", "region.y")],
                   lfpr_tfr_00[, c("name_harmonized", "lfpr", "tfr", "nonagri", "middle_high_edu", "housep",
                                   "exp_edu","eduyear_f","sex", "gdp_percap", "gdp_percapconst", "gdp23_prop", "prop_minor", 
                                   "extend_fam", "eduyear", "province", "region.x", "region.y")])
data_00_10$year = factor(rep(c(2010, 2000), c(nrow(lfpr_tfr_10_hmnz), nrow(lfpr_tfr_00))))
data_00_10 = subset(data_00_10, sex == "f" & !province %in% c("西藏","青海"))

range(lfpr_tfr_10_hmnz$eduyear_f)
hist(lfpr_tfr_10_hmnz$eduyear_f[lfpr_tfr_10_hmnz$sex=="f"])
# panel data 
data_90_10 = rbind(lfpr_tfr_90[, c("name_harmonized", "sex", "lfpr", "tfr", "nonagri", "middle_high_edu",
                                   "gdp_percap", "province", "region.x", "region.y")],
                   lfpr_tfr_00[, c("name_harmonized", "sex", "lfpr", "tfr", "nonagri", "middle_high_edu",
                                   "gdp_percap", "province", "region.x", "region.y")],
                   lfpr_tfr_10_hmnz[, c("name_harmonized", "sex", "lfpr", "tfr", "nonagri", "middle_high_edu",
                                        "gdp_percap", "province", "region.x", "region.y")])

data_90_00$year = factor(rep(c(1990, 2000), c(nrow(lfpr_tfr_90), nrow(lfpr_tfr_00))))
data_90_00 = subset(data_90_00, sex == "f" & !province %in% c("西藏","青海"))

# panel data vis --------------------------------------

subset(data_00_10, sex == "f") %>%
  ggplot(aes(eduyear_f, lfpr, color = factor(year))) + 
  geom_point() 

lfpr_tfr_10_hmnz %>% 
  filter(sex %in% c("f")) %>% 
  ggplot(aes(eduyear_mean_f, tfr)) + 
  geom_point(alpha = 0.8) +
  #scale_y_continuous(limits = c(0.3, max(lfpr_tfr_90$tfr))) +
  #geom_smooth(method = "loess") + 
  ylab("劳动参与率") + 
  xlab("平均教育年限") + 
  theme_bw() + 
  theme(text = element_text(family = "stsong", size = 18),
        panel.grid = element_blank()) 

# panel data model -----------------------------------

# mean regresion, between group 
names(data_00_10)
panel_mean = data_00_10 %>% 
  filter(sex == "f") %>% 
  group_by(name_harmonized) %>%
  mutate(tfr_mean = mean(tfr, na.rm = T),
         lfpr_mean = mean(lfpr, na.rm = T),
         nonagri_mean = mean(nonagri, na.rm = T),
         middle_high_edu_mean = mean(middle_high_edu, na.rm = T),
         gdp_percap_mean = mean(log(gdp_percap)),
         housep_mean = mean(log(housep+0.001), na.rm = T))

which(is.na(panel_mean$housep_mean)) %>% panel_mean$name_harmonized[.]

mod_lfpr1.1 = lm(lfpr_mean ~ tfr_mean, 
                 data = na.omit(panel_mean))
mod_lfpr1.2 = lm(lfpr_mean ~ tfr_mean + nonagri_mean + 
                   middle_high_edu_mean + I(middle_high_edu_mean^2), 
                 data = na.omit(panel_mean))
mod_lfpr1.3 = lm(lfpr_mean ~ tfr_mean + nonagri_mean + I(nonagri_mean^2), #+
                 #middle_high_edu_mean + + I(middle_high_edu_mean^2) + 
                 #housep_mean + tfr_mean*housep_mean, 
                 data = panel_mean) 

mod_tfr1.1 = lm(tfr_mean ~ lfpr_mean, 
                data = na.omit(panel_mean)) 
mod_tfr1.2 = lm(tfr_mean ~ lfpr_mean + nonagri_mean + middle_high_edu_mean, 
                data = panel_mean)
mod_tfr1.3 = lm(tfr_mean ~ lfpr_mean + nonagri_mean + middle_high_edu_mean + housep_mean, 
                data = panel_mean)

summary(mod_lfpr1.3)

# demean regression, within group
panel_demean = data_00_10 %>% 
  filter(sex == "f") %>% 
  group_by(name_harmonized) %>%
  mutate(tfr_dem = tfr - mean(tfr, na.rm = T),
         lfpr_dem = lfpr - mean(lfpr, na.rm = T),
         middle_high_edu_dem = middle_high_edu - mean(middle_high_edu, na.rm = T),
         nonagri_dem = nonagri - mean(nonagri, na.rm = T),
         gdp_percap_dem = log(gdp_percap) - mean(log(gdp_percap)),
         housep_dem = log(housep+0.001)-mean(log(housep+0.001), na.rm = T)
  )

which(is.na(panel_demean$housep_mean))

mod_lfpr2.1 = lm(lfpr_dem ~ tfr_dem, 
                 data = na.omit(panel_demean))
mod_lfpr2.2 = lm(lfpr_dem ~ tfr_dem + nonagri_dem + middle_high_edu_dem + I(middle_high_edu_dem^2), 
                 data = na.omit(panel_demean))
mod_lfpr2.3 = lm(lfpr_dem ~ tfr_dem + nonagri_dem + middle_high_edu_dem + housep_dem + lfpr_dem*housep_dem, 
                 data = panel_demean)
mod_tfr2.1 = lm(tfr_dem ~ lfpr_dem, 
                data = na.omit(panel_demean))
mod_tfr2.2 = lm(tfr_dem ~ lfpr_dem + nonagri_dem + middle_high_edu_dem, 
                data = panel_demean)
mod_tfr2.3 = lm(tfr_dem ~ lfpr_dem + nonagri_dem + middle_high_edu_dem + housep_dem + lfpr_dem*housep_dem, 
                data = panel_demean)

summary(mod_tfr2.3)
stargazer(mod_lfpr1.1, mod_lfpr1.2, mod_lfpr1.3, mod_lfpr2.1, mod_lfpr2.2, mod_lfpr2.3, 
          digits = 3,
          header = FALSE,
          type = "html", 
          #se = rob_se,
          title = "女性劳动参与率为因变量的面板回归模型",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          out = "fit_panel_table1.html")

stargazer(mod_tfr1.1, mod_tfr1.2, mod_tfr1.3, mod_tfr2.1, mod_tfr2.2, mod_tfr2.3, 
          digits = 3,
          header = FALSE,
          type = "html", 
          #se = rob_se,
          title = "面板回归模型",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          out = "fit_panel_table2.html")

summary(mod_tfr1.3)

# estimate all seven models

library(plm)
mod1 = lm(lfpr ~ tfr, data = data_00_10)
mod1.1 = lm(lfpr ~ tfr + housep, data = data_00_10)
mod1.2 = lm(lfpr ~ tfr + housep + housep*tfr, data = data_00_10)
mod1.3 = lm(lfpr ~ tfr + gdp_percapconst, data = data_00_10)
mod1.4 = lm(lfpr ~ tfr + gdp_percap*tfr, data = data_00_10)
summary(mod1.4)

mod2 = plm(lfpr ~ tfr, 
           index = c("name_harmonized", "year"),
           model = "within",
           effect = "twoways",
           data = data_90_00)
summary(mod2)
mod3 = plm(lfpr ~ tfr + name_harmonized+year + log(gdp_percap)+nonagri+prop_minor, 
           index = c("name_harmonized", "year"),
           model = "within",
           effect = "twoways",
           data = data_00_10)
mod4 = plm(lfpr ~ tfr + name_harmonized+year + log(gdp_percap)+
             nonagri+prop_minor+middle_high_edu+I(middle_high_edu^2), 
           index = c("name_harmonized", "year"),
           model = "within",
           effect = "twoways",
           data = data_00_10)

summary(mod1)

library(stargazer)

# gather clustered standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
               sqrt(diag(vcovHC(mod1.1, type = "HC1"))),
               sqrt(diag(vcovHC(mod2, type = "HC1"))),
               sqrt(diag(vcovHC(mod3, type = "HC1"))),
               sqrt(diag(vcovHC(mod4, type = "HC1"))))

# generate the table
stargazer(mod1, mod1.1, mod2, mod3, mod4, 
          digits = 3,
          header = FALSE,
          type = "html", 
          se = rob_se,
          title = "面板回归模型",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
          out = "fit_table.html")


## Reference

# https://www.r-bloggers.com/2019/11/an-introduction-to-causal-inference/
