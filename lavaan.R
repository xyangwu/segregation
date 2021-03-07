library(lavaan)
############ CFA #############

HS.model <- 'visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)

# display summary output
summary(fit, fit.measures=TRUE)

############ CFA #############
model <- '
    # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
    # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)

MD11.5 <- read.table("http://www.da.ugent.be/datasets/MaxwellDelaney11.5.dat", header=TRUE)

colMeans(MD11.5)

library(reshape)
MD11.5$subject <- factor(paste("subject",1:nrow(MD11.5),sep=""))
MD11.5.long <- melt(MD11.5, id.var="subject", variable_name="A")
MD11.5.long$age <- as.numeric(MD11.5.long$A) - 1
names(MD11.5.long)[3] <- "y"
head(MD11.5.long)

# classic analysis: paired t-test

t.test(MD11.5$age30, MD11.5$age36, paired=TRUE)

# repeated measures ANOVA
# use a classic univariate repeated measures ANOVA to test if the means are different across time

fit <- aov(y ~ A + Error(subject), data=MD11.5.long)
summary(fit)

##'use a SEM model to test if the means are different across time; first fit a model where the intercepts are
##'different; next fit a model where we constrain the intercepts to be equal; compare the two models with a
##'chi-square difference test

model <- '
  age30 ~ i1*1; age36 ~ i2*1; age42 ~ i3*1; age48 ~ i4*1
  age30 ~~ v1*age30
  age36 ~~ v2*age36
  age42 ~~ v3*age42
  age48 ~~ v4*age48
  age30 ~~ age36 + age42 + age48
  age36 ~~ age42 + age48
  age42 ~~ age48
  '
fit <- lavaan(model, data = MD11.5)
summary(fit)

model.equal <- '
 age30 ˜ i1*1; age36 ˜ i1*1; age42 ˜ i1*1; age48 ˜ i1*1

 age30 ˜˜ v1*age30
 age36 ˜˜ v1*age36
 age42 ˜˜ v1*age42
 age48 ˜˜ v1*age48

 age30 ˜˜ c*age36 + c*age42 + c*age48
 age36 ˜˜ c*age42 + c*age48
 age42 ˜˜ c*age48
 '
fit.equal <- lavaan(model.equal, data = MD11.5)
anova(fit, fit.equal)

MEAN <- c(3.06893, 2.92590, 3.11013, 3.02577, 2.85656, 3.09346)
SDS <- c(0.84194, 0.88934, 0.83470, 0.84081, 0.90864, 0.83984)
lower <- '
  1.00000
  0.55226 1.00000
  0.56256 0.60307 1.00000
  0.31889 0.35898 0.27757 1.00000
  0.24363 0.35798 0.31889 0.56014 1.00000
  0.32217 0.36385 0.32072 0.56164 0.59738 1.00000 '
COV <- getCov(lower, sds=SDS, names = c("Glad1", "Cheer1", "Happy1",
                                        "Glad2", "Cheer2", "Happy2"))
COV

lower <- '
  2.926
  1.390 4.257
  1.698 2.781 4.536
  1.628 2.437 2.979 5.605
  1.240 0.789 0.903 1.278 3.208
  0.592 1.890 1.419 1.004 1.706 3.994
  0.929 1.278 1.900 1.000 1.567 1.654 3.583
  0.659 0.949 1.731 2.420 0.988 1.170 1.146 3.649 '
COV <- getCov(lower, names=c("anti1", "anti2", "anti3", "anti4",
                             "dep1", "dep2", "dep3", "dep4"))
MEANS <- c(1.750, 1.928, 1.978, 2.322, 2.178, 2.489, 2.294, 2.222)

# MANOVA

d12 <- c(1,-1,0,0)
d23 <- c(0,1,-1,0)
d34 <- c(0,0,1,-1)
M <- cbind(d12,d23,d34)
M
fit <- lm( cbind(age30, age36, age42, age48) %*% M ~ 1, data=MD11.5)
anova(fit, test="Wilks")

# the linear mixed model
library(lme4)
fit.lmer <- lmer(y ~ 1 + age + (1 + age | subject), data=MD11.5.long)
summary(fit.lmer)

# The SEM approach to longitudinal data analysis

model.equal <- '
# TODO
'
fit.equal <- sem(model.equal, sample.cov=COV, sample.mean=MEANS,
                 sample.nobs=180, sample.cov.rescale=FALSE, mimic="EQS")
anova(fit, fit.equal)

####################################

RICLPM <- ' 
    ### Random intercepts ### 
    RIucla =~ 1*uclaT1 + 1*uclaT2 + 1*uclaT3 + 1*uclaT4 
    RIqol =~ 1*qolT1 + 1*qolT2 + 1*qolT3 + 1*qolT4 
     
    ### within-person residuals ### 
    Lucla_1 =~ 1*uclaT1 
    Lucla_2 =~ 1*uclaT2 
    Lucla_3 =~ 1*uclaT3 
    Lucla_4 =~ 1*uclaT4 
     
    Lqol_1 =~ 1*qolT1 
    Lqol_2 =~ 1*qolT2 
    Lqol_3 =~ 1*qolT3 
    Lqol_4 =~ 1*qolT4 
     
    ### autoregressive parameters ### 
    Lucla_4 ~ sx*Lucla_3 
    Lucla_3 ~ sx*Lucla_2 
    Lucla_2 ~ sx*Lucla_1 
     
    Lqol_4 ~ sy*Lqol_3 
    Lqol_3 ~ sy*Lqol_2 
    Lqol_2 ~ sy*Lqol_1 
     
    ### cross-lagged ### 
    Lucla_4 ~ cyx*Lqol_3 
     
    Lucla_3 ~ cyx*Lqol_2 
     
    Lucla_2 ~ cyx*Lqol_1 
     
    Lqol_4 ~ cxy*Lucla_3 
     
    Lqol_3 ~ cxy*Lucla_2 
     
    Lqol_2 ~ cxy*Lucla_1 
     
    ### within-time associations ### 
    Lucla_1 ~~ Lqol_1 
    Lucla_2 ~~ Lqol_2 
    Lucla_3 ~~ Lqol_3 
    Lucla_4 ~~ Lqol_4 
     
    ### some further constraints on the variance structure ### 
    # The error variances of the observed variables need to be constrained to 
    zero 
    uclaT1~~0*uclaT1 
    uclaT2~~0*uclaT2 
    uclaT3~~0*uclaT3 
    uclaT4~~0*uclaT4 
     
    qolT1~~0*qolT1 
    qolT2~~0*qolT2 
    qolT3~~0*qolT3 
    qolT4~~0*qolT4 
    
    # The covariance between the intercepts and exogenous variables need to be 
    constrained to zero 
    # This includes the latent derivatives at T1, which are exogenous variables 
    Lucla_1~~0*RIucla 
    Lucla_1~~0*RIqol 
     
    Lqol_1~~0*RIucla 
    Lqol_1~~0*RIqol 
    ' 
