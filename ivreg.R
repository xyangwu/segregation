library(AER)

data("PSID1976")

model1 <- lm(log(wage) ~ education,
             data = PSID1976)

model2 <- lm(log(wage) ~ education,
             data = subset(PSID1976, participation=="yes"))
summary(model2)

plot(PSID1976$education[PSID1976$participation=="yes"],
     log(PSID1976$wage[PSID1976$participation=="yes"]),
     col = "red", main = "log(wage) vs education",
     xlab = "education (years of schooling)",
     ylab = "log(wage)")
abline(model2)

# feducation iv
model3 <- lm(education ~ feducation,
             data = subset(PSID1976, participation=="yes"))
summary(model3)

# 2 stage
model4 <- lm(log(wage) ~ fitted.values(model3),
             data = subset(PSID1976, participation=="yes"))
summary(model4)

# ivreg
model5 <- ivreg(log(wage) ~ education | feducation,
                data = subset(PSID1976, participation=="yes"))
summary(model5)
# Same beta estimates as 2SLS with correct standard errors.

# Get 90% confidence intervals for the return to education
confint(model2, 'education', level=0.90)

confint(model5, 'education', level=0.90)

# Notice that confidence intervals are wider for the IV model.

model6 <- lm(education ~ meducation,
             data = subset(PSID1976, participation=="yes"))
summary(model6)

# meducation iv
model7 <- ivreg(log(wage) ~ education | meducation,
                data = subset(PSID1976, participation=="yes"))
summary(model7)

######## over-identified #######
# an over-identified case i.e. we have multiple IVs for an endogenous variable. 
# We will also look at tests for endogeneity and over-identifying restrictions.

df <- subset(PSID1976, participation=="yes")

as.dist(cor(df[, c("wage", "education", "feducation", "meducation", "experience")]))

model2 <- lm(log(wage) ~ education + experience + I(experience^2),
             data = df)
summary(model2)



#The OLS estimate for return to education is 10.7%

model3 <- lm(education ~ experience + I(experience^2) +
               feducation + meducation, data = df)
summary(model3)
#Both 'feducation' and 'meducation' are individually significant.

# Test the hypothesis that feducation and meducation are jointly equal to zero.

linearHypothesis(model3,c("feducation = 0",
                          "meducation = 0"),
                 test="F")
#Both variables are jointly significant in the model.

model5 <- lm(log(wage) ~ experience + I(experience^2) +
               fitted.values(model3), data = df)
summary(model5)

model6 <- ivreg(log(wage) ~ education + experience + I(experience^2)|
                  feducation + meducation + experience + I(experience^2),
                data = df)
summary(model6)

model7 <- ivreg(log(wage) ~ education + experience + I(experience^2)|
                  .-education + feducation + meducation,
                data = df)
summary(model7)

#Note that return to education has a p-value slightly higher than 5%.
#Higher standard errors are a common consequence of the IV technique.

# test endogenrity

model8 <- lm(log(wage) ~ education + experience + I(experience^2) +
               residuals(model3),
             data = df)
summary(model8)
#The coefficient for the residuals is significant at the 10% level. 
# We can conclude that education is endogenous.

# test for over-identifying restrictions

model9 <- lm(residuals(model7) ~ experience + I(experience^2) +
               meducation + feducation,
             data = df)
#names(summary(model9)) : to view extractable items

pchisq(summary(model9)$r.squared*nrow(df), 1, lower.tail=FALSE)

#p-value is higher than 5%. So, 'meducation' and 'feducation'
#clear the over-identification criteria.

# the same test results with the summary command.

summary(model7, diagnostics = TRUE)

# Note that the p-values of Hausman and Sargan tests are the same as we obtained earlier.

# GMM --------------------------------------------------
library(gmm)

model2a <- lm(log(wage) ~ education, data = df)
summary(model2a)

model2b <- gmm(log(df$wage) ~ df$education, x = df$education)
summary(model2b)
# standard errors are different because OLS assumes homoskedasticity while GMM does not.

model3a <- ivreg(log(df$wage) ~ df$education | df$feducation)
summary(model3a)

model3b <- gmm(log(df$wage) ~ df$education, x = df$feducation)
summary(model3b)

# use the gmm function to estimate the same model using OLS’s moment conditions

model4a <- lm(log(wage) ~ education + experience + I(experience^2),
              data = df)
summary(model4a)

mm <- cbind(df$education, df$experience, I(df$experience^2))
model4b <- gmm(log(wage) ~ education + experience + I(experience^2),
               x = mm, data = df)
summary(model4b) 

# using feducation as the IV

model5a <- ivreg(log(wage) ~ education + experience + I(experience^2) |
                   .- education + feducation, data = df)
summary(model5a)

mm <- cbind(df$feducation, df$experience, I(df$experience^2))
model5b <- gmm(log(wage) ~ education + experience + I(experience^2),
               x = mm, data = df)
summary(model5b)

# over-identified case. using feducation and meducation as IVs
model6a <- ivreg(log(wage) ~ education | feducation + meducation, data = df)
summary(model6a)

mm <- cbind(df$feducation, df$meducation)
model6b <- gmm(log(wage) ~ education,
               x = mm, data = df)
summary(model6b)

# add experience variable

model7a <- ivreg(log(wage) ~ education + experience + I(experience^2) |
                   .-education + feducation + meducation, data = df)
summary(model7a)

mm <- cbind(df$feducation, df$meducation, df$experience, I(df$experience^2))
model7b <- gmm(log(wage) ~ education + experience + I(experience^2),
               x = mm, data = df)
summary(model7b)

specTest(model6b)
specTest(model7b)
# Cannot reject the null that the moment conditions hold approximately.

# iterative method 
mm <- cbind(df$feducation, df$meducation, df$experience, I(df$experience^2))
model9 <- gmm(log(wage) ~ education + experience + I(experience^2),
              x = mm, data = df, type = "iter", traceIter = TRUE)

plot(model7b, which = 3) 

# https://www.r-exercises.com/2017/05/15/instrumental-variables-in-r-exercises-part-1/
# https://www.r-exercises.com/2017/05/15/instrumental-variables-in-r-exercises-part-1-solutions/

