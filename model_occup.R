library(AER)
library(plm)


# 2010

datamodel10 = left_join(a10, dsd10_class2, by = c("unit" = "unit"))
datamodel10 = datamodel10 %>%
  left_join(., dsd10_class2nonagri %>% 
            select(unit, dnonagri = d, dsnonagri = ds)) %>%
  left_join(., dsd10_class2agri %>%
            select(unit, dagri = d, dsagri = ds))

datamodel10 = left_join(datamodel10, econom10, by = c("unit" = "name_harmonized"))
datamodel10 = left_join(datamodel10, edu10, by = c("unit" = "name_harmonized"))
datamodel10 = left_join(datamodel10, subset(lfpr_tfr_10, sex == "f"), by = c("unit" = "name_harmonized"))
datamodel10 = left_join(datamodel10, ind10_2, by = c("unit" = "name_harmonized")) 
datamodel10 = left_join(datamodel10, fine10, by = c("unit" = "name_harmonized")) 
datamodel10 = left_join(datamodel10, skill, by = c("unit" = "name_harmonized")) 

datamodel10$nonagri = 1-datamodel10$ind1
datamodel10$expedu_per = datamodel10$gdp*datamodel10$gdp_exp*datamodel10$exp_edu*10000/datamodel10$pop_local
datamodel10$fine10_mar = datamodel10$fine*datamodel10$married

# labor force growth 
# lf_00ipums %>% names()
datamodel10 = datamodel10 %>%
  left_join(.,lf_00ipums %>%
              select(unit = name_harmonized, lfsize00 = lfsize)) %>% 
  mutate(lfgrowth = (lfsize/lfsize00)-1) %>% 
  ungroup() 

summary(datamodel10$unemp);sd(datamodel10$unemp)

# model 2010 -------------------------------------------

datamodel00[datamodel10$unemp > 0.1,]
plot(datamodel10$indskill, datamodel10$a)
plot(datamodel10$fine10, datamodel10$tfr) 
cor.test(datamodel10$lfgrowth, datamodel10$ds)    

subset(datamodel10, 
       !unit %in% c("新疆自治区直辖县级行政区划") &
         province.x %in% c("西藏", "新疆", "内蒙古")) 

# A 

model10.a1agri = lm(formula = dnonagri ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill +
                  eduyear_mean_f + 
                  unemp +
                  tfr, 
                data = datamodel10) 
summary(model10.a1nonagri) 

summary(model10.a1) 

model10.a2 = lm(a ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + exp_edu + tfr,
                data = subset(datamodel10, 
                              !province.x %in% c("西藏", "青海", "新疆")))
summary(model10.a2) 

which(is.na(datamodel10$tfr)) %>% datamodel10$unit[.]

# 1st
pred_tfr = lm(tfr ~ fine10, data = datamodel10)
datamodel10$pred_tfr = predict(pred_tfr)

# 2st
model10.a2 = lm(formula = ds ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill +
                  eduyear_mean_f + 
                  lfgrowth + unemp +
                  pred_tfr, 
                data = datamodel10)
summary(model10.a2) 

model10.a3 = ivreg(a ~ log(gdp_percap) + 
                     ind1 + ind3 + indskill + 
                     eduyear_mean_f +
                     unemp + 
                     tfr|
                     log(gdp_percap) + 
                     ind1 + ind3 + indskill + 
                     eduyear_mean_f +
                     unemp +
                     fine10, 
                   data = datamodel10) 

summary(model10.a3, vcov = sandwich, diagnostics = TRUE) 


# D 

model10.d1 = lm(formula = d ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill +
                  eduyear_mean_f + 
                  unemp +
                  tfr, 
                data = datamodel10) 
summary(model10.d1) 

model10.d1nonagri = lm(formula = dnonagri ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill +
                  eduyear_mean_f + 
                  unemp +
                  tfr, 
                data = datamodel10 %>% filter(!unit %in% c(""))) 
summary(model10.d1nonagri) 

model10.d1agri = lm(formula = dagri ~ log(gdp_percap) + 
                         ind1 + ind3 + indskill +
                         eduyear_mean_f + 
                         unemp +
                         tfr, 
                       data = datamodel10) 
summary(model10.d1agri) 

model10.d2 = lm(d ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + exp_edu + unemp + tfr, 
                data = datamodel10)
summary(model10.d2)

model10.d3 = ivreg(ds ~ log(gdp_percap) + 
                     ind1 + ind3 + indskill + 
                     eduyear_mean_f + exp_edu +
                     lfgrowth + unemp + 
                     tfr|
                     log(gdp_percap) + 
                     ind1 + ind3 + indskill + 
                     eduyear_mean_f + exp_edu +
                     lfgrowth + unemp + 
                     fine10, 
                   data = datamodel10) 

summary(model10.d3, vcov = sandwich, diagnostics = TRUE) 

# Ds
model10.ds1 = lm(formula = ds ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill +
                  eduyear_mean_f + 
                  unemp +
                  tfr, 
                data = datamodel10) 
summary(model10.ds1)

model10.ds1agri = lm(formula = dsagri ~ log(gdp_percap) + 
                         ind1 + ind3 + indskill +
                         eduyear_mean_f + 
                         unemp +
                         tfr, 
                       data = datamodel10) 
summary(model10.d1nonagri) 

model10.ds3 = ivreg(ds ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + exp_edu + tfr |
                      log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + exp_edu + fine10, 
                    data = datamodel10) 
model10.ds3 = ivreg(lfpr ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + tfr |
                      log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + finerate1, 
                    data = datamodel10) 
model10.ds3 = lm(lfpr ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + tfr,
                 data = datamodel10) 
summary(model10.ds3)


cor.test(datamodel10$gdp3_prop, datamodel10$a)
cor.test(datamodel10$gdp3_prop[datamodel10$a<=6], datamodel10$a[datamodel10$a<=6])

names(datamodel10)
subset(datamodel10, a > 5)
datamodel10 %>%
  ggplot() + 
  geom_point(aes(tfr, a)) + 
  geom_smooth(aes(tfr, a), method = "lm")

cor.test(datamodel10$fine10, datamodel10$tfr)
plot(datamodel10$fine10, datamodel10$tfr)

# model 2000 -------------------------------------------

# 2000 

plot(datamodel00$fine00, datamodel00$tfr)
cor.test(datamodel00$fine00_2, datamodel00$tfr)

datamodel00 = left_join(a00_class2, dsd00_class2, by = c("unit" = "unit")) 
datamodel00 = left_join(datamodel00, econom00, by = c("unit" = "name_harmonized")) 
datamodel00 = left_join(datamodel00, edu00, by = c("unit" = "name_harmonized")) 
datamodel00 = left_join(datamodel00, subset(lfpr_tfr_00, sex == "f"), by = c("unit" = "name_harmonized")) 
datamodel00 = left_join(datamodel00, ind00, by = c("unit" = "name_harmonized")) 
datamodel00 = left_join(datamodel00, fine00, by = c("unit" = "name_harmonized")) 
datamodel00 = left_join(datamodel00, lf_00ipums, by = c("unit" = "name_harmonized")) 

names(datamodel00)
datamodel00 = left_join(datamodel00, 
                        lf_90 %>% select(unit = name_harmonized, lfsize90 = lfsize))

datamodel00$lfgrowth = datamodel00$lfsize.y/datamodel00$lfsize90-1
hist(datamodel00$lfgrowth)
datamodel00 = na.omit(datamodel00)

model00.a1 = lm(formula = a ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill + 
                  eduyear_mean_f + 
                  unemp +
                  tfr, 
                data = datamodel00)
summary(model00.a1) 

# A
# 1st
pred_tfr00 = lm(tfr~fine00, 
                data = na.omit(datamodel00))
coeftest(pred_tfr00, vcov = vcovHC, type = "HC1")

# 2st
datamodel00$pred_tfr = predict(pred_tfr00) 
model00.a1 = lm(formula = a ~ log(gdp_percap) + 
                  ind1 + ind3 + indskill + 
                  eduyear_mean_f + exp_edu + 
                  unemp + lfgrowth +
                  pred_tfr, 
                data = datamodel00)
summary(model00.a1) 

cor.test(datamodel00$tfr, datamodel00$fine00) 


model00.a3 = ivreg(a ~ log(gdp_percap) + 
                     ind1 + ind3 + indskill + 
                     eduyear_mean_f +
                     unemp + 
                     tfr|
                     log(gdp_percap) + 
                     ind1 + ind3 + indskill + 
                     eduyear_mean_f +
                     unemp + 
                     fine00_2, 
                   data = datamodel00)

summary(model00.a3, vcov = sandwich, diagnostics = TRUE) 

model00.a3 = lm(a ~ gdp3_prop, 
              data = subset(datamodel00, region.y == "西部")) 
summary(model00.a3)

# Ds

model00.ds1 = lm(ds ~ gdp_percap + gdp2_prop + gdp3_prop + eduyear_mean_f + tfr, 
            data = data_dsd00) 
summary(model00.ds1)

model00.ds2 = lm(ds ~ tfr, 
              data = data_dsd00) 
summary(model00.ds2) 

model00.ds3 = ivreg(ds ~ log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu + tfr|
                     log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu + fine00, 
                   data = subset(datamodel00, exp_edu != 0)) 

summary(model001)

# D
model00.d2 = lm(d ~ log(gdp_percap) + gdp2_prop + gdp3_prop + eduyear_mean_f + exp_edu + tfr, 
                 data = data_dsd00) 
summary(model00.d2) 

# model 1990 ---------------------------------------------------

# 1990

datamodel90 = left_join(a90_class2, dsd90_class2, by = c("unit" = "unit")) 
datamodel90 = left_join(datamodel90, edu90, by = c("unit" = "name_harmonized"))
datamodel90 = left_join(datamodel90, subset(lfpr_tfr_90, sex == "f"&LABFORCE==2), by = c("unit" = "name_harmonized"))
datamodel90 = left_join(datamodel90, econom90, by = c("unit" = "name_harmonized"))
datamodel90 = left_join(datamodel90, fine90, by = c("unit" = "name_harmonized")) 
datamodel90 = left_join(datamodel90, lf_90, by = c("unit" = "name_harmonized")) 
datamodel90 = left_join(datamodel90, ind90_2, by = c("unit" = "name_harmonized")) 
names(datamodel90)
summary(datamodel90$ind3)
sd(datamodel90$ind3)
# D

model90.d2 = lm(d ~ eduyear_mean_f + gdp_percap + gdp2_prop + gdp3_prop + tfr + I(tfr^2), 
                 data = subset(datamodel90, exp_edu != 0))
summary(model90.d2)

# Ds
model90.ds2 = lm(ds ~ eduyear_mean_f, 
                data = subset(datamodel90, exp_edu != 0))
summary(model90.ds2)

model90.a3 = lm(a ~ log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu + tfr, 
                   data = subset(datamodel90, exp_edu != 0))

model90.a3 = ivreg(a ~ log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu +prop_urban + tfr|
                     log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu + prop_urban +fine90, 
                   data = subset(datamodel90, exp_edu != 0)) 

summary(model90.a3)

model90.ds3 = ivreg(ds ~ log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu + tfr|
                     log(gdp_percap) + gdp2_prop + gdp3_prop  + eduyear_mean_f + exp_edu + fine90, 
                   data = subset(datamodel90, exp_edu != 0)) 

summary(model90.ds3)

library(ivpack)
library(sandwich)
library(lmtest)
stargazer::stargazer(model10.d1agri, model10.d1nonagri, 
                     star.char = c("+", "*", "**", "***"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     notes = c(". p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
                     align = T, notes.append = F, 
                     type = "html", out = "fig/fittable_D.html")

stargazer::stargazer(model10.a3, model10.d3, model10.ds3, 
                     align = T, type = "html", 
                     out = "fig/fit_table10.html")

# 2SLS --------------------------------------------------------------- 

library(AER)

cor.test(datamodel10$prop_minor, datamodel10$eduyear_mean)

# perform the first stage regression
s1 <- lm(tfr ~ fine00 +
              log(gdp_percap) + 
              ind1 + ind3 + indskill +
              eduyear_mean_f + 
              unemp, data = datamodel00)
coeftest(s1, vcov = vcovHC, type = "HC1")

waldtest(s1,.~.-fine00) 

# run the stage 2 regression
tfrs2 <- lm(data_a10$tfr ~ tfrs1$fitted.values)
coeftest(tfr_s2, vcov = vcovHC) 
summary(tfr_s2)

# perform TSLS using 'ivreg()'  
mod_ivreg <- ivreg(d ~ tfr | finerate1, data = data_dsd10)
coeftest(mod_ivreg, vcov = vcovHC, type = "HC1")
summary(mod_ivreg)

# weak instrument test 
# do regressions for partial F-tests
# first-stage:
fs = lm(tfr ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + exp_edu + fine10, 
        data = datamodel10) 
# null first-stage (i.e. exclude IVs):
fn = lm(tfr ~ log(gdp_percap) + ind2 + ind3 + eduyear_mean_f + exp_edu,
        data = datamodel10)
# simple F-test 
waldtest(fs, fn)
# F-test robust to heteroskedasticity
waldtest(fs, fn, vcov = vcovHC(fs, type="HC0"))$F[2]


# # quadritic line
# # mininum point
# b0 = model1$coefficients[1]
# b1 = model1$coefficients[2]
# b2 = model1$coefficients[3]
# -b1/(2*b2)
# (4*b0*b2-b1^2)/4*b2
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R 
# https://www.r-bloggers.com/2019/11/intrumental-variable-regression-and-machine-learning/
