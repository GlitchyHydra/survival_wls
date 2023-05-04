library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(haven)
library(survminer)
library(survival)
library(pROC)
library(base)

setwd("E:/education/thesis")

source("data_preprocessing.R")

df_surv$death_status <- df_surv$death_status + 0

df_surv$z_sexrsp <- as.factor(df_surv$z_sexrsp)

df_surv$z_gx352re <- as.factor(df_surv$z_gx352re)
df_surv$z_gx342re <- as.factor(df_surv$z_gx342re)
df_surv$z_gx346re <- as.factor(df_surv$z_gx346re)
df_surv$z_gx348re <- as.factor(df_surv$z_gx348re)
df_surv$z_gx356re <- as.factor(df_surv$z_gx356re)

df_surv <- df_surv[df_surv$z_gx342re != "-3",]
df_surv$z_gx342re <- droplevels(df_surv$z_gx342re)

df_surv <- df_surv[df_surv$z_gx346re != "-3",]
df_surv$z_gx346re <- droplevels(df_surv$z_gx346re)

df_surv <- df_surv[df_surv$z_gx348re != "-3",]
df_surv <- df_surv[df_surv$z_gx348re != "-1",]
df_surv$z_gx348re <- droplevels(df_surv$z_gx348re)

df_surv <- df_surv[df_surv$z_gx352re != "-3",]
df_surv$z_gx352re <- droplevels(df_surv$z_gx352re)

df_graduates = df_surv[df_surv$rtype == 'g',]
df_siblings = df_surv[df_surv$rtype == 's',]

cox.mod_grad <- coxph(Surv(surv_years, death_status) ~ z_sexrsp + z_gx352re + z_gx346re +
                   z_gx348re + z_gx356re,  data = df_graduates)
sink("dataset comparisment/cox_grad.txt")
summary(cox.mod_grad)
sink()

cox.mod_sibs <- coxph(Surv(surv_years, death_status) ~ z_sexrsp + z_gx352re + z_gx346re +
                   z_gx348re + z_gx356re,  data = df_siblings)
sink("dataset comparisment/cox_sibs.txt")
summary(cox.mod_sibs)
sink()