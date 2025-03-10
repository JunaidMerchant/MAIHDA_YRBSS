rm(list = ls())

library(ggeffects)   # predictions and significance testing
library(insight)     # extracting random effects variances
library(datawizard)  # data wrangling and preparation
library(parameters)  # model summaries
library(performance) # model fit indices, ICC
library(glmmTMB)     # multilevel modelling
library(marginaleffects)
library(ggplot2)
library(Amelia)
library(gtsummary)
library(gt)
library(writexl)
library(flextable)
library(tidyr)
library(forcats)
library(ggtext)
library(sandwich)
library(emmeans)
library(pglm)
library(pdR)
library(plm)

load("C:/Users/jmerch/Desktop/LGBTQ/MAIHDA/StataResults_May1/RnR/ReDo/suicide_MAIHDA_modeldata.RData")

m1_glmm = m_null
m4_glmm = m_4
m2_glmm = m_2
m3_glmm = m_3
m5_glmm = m_5

m2_fe_pglm=fe_m_2
m4_fe_pglm=fe_m_4


rm(list=ls(pattern="^m_"))
rm(list=ls(pattern="^v_")) 

rm(i,lastFuncGrad,lastFuncParam,m7_predictions,m7_predictions_FE,m7_predictions_RE,OutcomeNum,Outcomes,
   predictions1,m5_predictions_RE,m6_predictions_RE)


m4_fe_glm = glm("suicide ~ prepost2020 + gender_char + sexorient_char + race_char",
                data=tmpData, family = binomial(link='logit'),method = "glm.fit",na.action=na.omit)

m2_fe_glm = glm("suicide ~ prepost2020",
                data=tmpData, family = binomial(link='logit'),method = "glm.fit",na.action=na.omit)


print(htest_pglm(RE=m4_glmm,  FE=m4_fe_glm, re.method="glmmTMB"))
print(htest_pglm(RE=m4_glmm,  FE=m4_fe_pglm, re.method="glmmTMB"))
print(htest_pglm(RE=m4_glmm,  FE=m4_fe_pglm_orig, re.method="glmmTMB"))


print(htest_pglm(RE=m2_glmm,  FE=m2_fe_pglm, re.method="glmmTMB"))
print(htest_pglm(RE=m2_glmm,  FE=m2_fe_glm, re.method="glmmTMB"))





phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call
  coef.wi <- coef(glmMod)
  coef.re <- fixef(glmerMod)$cond  ## changed coef() to fixef() for glmer
  vcov.wi <- vcov(glmMod)
  vcov.re <- vcov(glmerMod)$cond
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  
  cnst.no.re = which(names(coef.re) == "(Intercept)")
  names.re <- names(coef.re)[-cnst.no.re]
  
  cnst.no.wi = which(names(coef.wi) == "(Intercept)")
  names.wi <- names(coef.wi)[-cnst.no.wi]
  
  coef.h <- names.re[names.re %in% names.wi]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter, 
              method = "Hausman Test",  alternative = alternative,
              data.name=deparse(getCall(glmerMod)$data))  ## changed
  class(res) <- "htest"
  return(res)
}


print(phtest_glmer(m4_glmm,m4_fe_glm))
print(phtest_glmer(m4_glmm,m4_fe_pglm))

print(phtest_glmer(m2_glmm,m2_fe_glm))
print(phtest_glmer(m2_glmm,m2_fe_pglm))

################################################################################
################################################################################

rm(list = ls())

library(ggeffects)   # predictions and significance testing
library(insight)     # extracting random effects variances
library(datawizard)  # data wrangling and preparation
library(parameters)  # model summaries
library(performance) # model fit indices, ICC
library(glmmTMB)     # multilevel modelling
library(marginaleffects)
library(ggplot2)
library(Amelia)
library(gtsummary)
library(gt)
library(writexl)
library(flextable)
library(tidyr)
library(forcats)
library(ggtext)
library(sandwich)
library(emmeans)
library(pglm)
library(pdR)
library(plm)

load("C:/Users/jmerch/Desktop/LGBTQ/MAIHDA/StataResults_May1/RnR/ReDo/suicide_MAIHDA_modeldata_25states.RData")

m4_glmm = m_4
m2_glmm = m_2
m2_fe_pglm=fe_m_2
m4_fe_pglm_orig=fe_m_4


rm(list=ls(pattern="^m_"))
rm(list=ls(pattern="^v_")) 

rm(i,lastFuncGrad,lastFuncParam,m7_predictions,m7_predictions_FE,m7_predictions_RE,OutcomeNum,Outcomes,
   predictions1,m5_predictions_RE,m6_predictions_RE)



m4_fe_pglm = pglm("suicide ~ prepost2020 + gender_char + sexorient_char + race_char",
                  data=tmpData,family = binomial(link='logit'),
                  model="pooling",index=c("stratum3_char"),method = "bfgs",
                  na.action=na.omit,start=NULL)

m4_fe_glm = glm("suicide ~ prepost2020 + gender_char + sexorient_char + race_char",
                data=tmpData, family = binomial(link='logit'),method = "glm.fit",na.action=na.omit)

m2_fe_glm = glm("suicide ~ prepost2020",
                data=tmpData, family = binomial(link='logit'),method = "glm.fit",na.action=na.omit)


print(htest_pglm(RE=m4_glmm,  FE=m4_fe_glm, re.method="glmmTMB"))
print(htest_pglm(RE=m4_glmm,  FE=m4_fe_pglm, re.method="glmmTMB"))
print(htest_pglm(RE=m4_glmm,  FE=m4_fe_pglm_orig, re.method="glmmTMB"))


print(htest_pglm(RE=m2_glmm,  FE=m2_fe_pglm, re.method="glmmTMB"))
print(htest_pglm(RE=m2_glmm,  FE=m2_fe_glm, re.method="glmmTMB"))





phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call
  coef.wi <- coef(glmMod)
  coef.re <- fixef(glmerMod)$cond  ## changed coef() to fixef() for glmer
  vcov.wi <- vcov(glmMod)
  vcov.re <- vcov(glmerMod)$cond
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  
  cnst.no.re = which(names(coef.re) == "(Intercept)")
  names.re <- names(coef.re)[-cnst.no.re]
  
  cnst.no.wi = which(names(coef.wi) == "(Intercept)")
  names.wi <- names(coef.wi)[-cnst.no.wi]
  
  coef.h <- names.re[names.re %in% names.wi]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter, 
              method = "Hausman Test",  alternative = alternative,
              data.name=deparse(getCall(glmerMod)$data))  ## changed
  class(res) <- "htest"
  return(res)
}


print(phtest_glmer(m4_glmm,m4_fe_glm))
print(phtest_glmer(m4_glmm,m4_fe_pglm))

print(phtest_glmer(m2_glmm,m2_fe_glm))
print(phtest_glmer(m2_glmm,m2_fe_pglm))






################################################################################
################################################################################

glmMod=m2_fe_glm
glmerMod=m2_glmm

coef.wi <- coef(glmMod)
coef.re <- fixef(glmerMod)$cond  ## changed coef() to fixef() for glmer
vcov.wi <- vcov(glmMod)
vcov.re <- vcov(glmerMod)$cond
names.wi <- names(coef.wi)
names.re <- names(coef.re)

cnst.no.re = which(names(coef.re) == "(Intercept)")
names.re <- names(coef.re)[-cnst.no.re]

cnst.no.wi = which(names(coef.wi) == "(Intercept)")
names.wi <- names(coef.wi)[-cnst.no.wi]


coef.h <- names.re[names.re %in% names.wi]
dbeta <- coef.wi[coef.h] - coef.re[coef.h]
df <- length(dbeta)
dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]

stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()


pval <- pchisq(stat, df = df, lower.tail = FALSE)
names(stat) <- "chisq"
parameter <- df
names(parameter) <- "df"
alternative <- "one model is inconsistent"
res <- list(statistic = stat, p.value = pval, parameter = parameter, 
            method = "Hausman Test",  alternative = alternative,
            data.name=deparse(getCall(glmerMod)$data))  ## changed
class(res) <- "htest"

res
