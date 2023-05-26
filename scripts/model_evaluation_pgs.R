library(caTools)
library(dplyr)
library(ggfortify)
library(haven)
library(survminer)
library(pROC)
library(base)
library(rlist)
library(fuzzySim)
library(survival)
#library(survcomp)
library(randomForest)
library(survivalmodels)


df <- read_wls_dataset(include_pgs_datasets = TRUE)
df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-142) #-142 -62
df_filtered <- filter_less_important_variables(df, 
                                               df_features_effect,
                                               threshold = 1e-60) #-60 -30
df_filtered <- filter_correlated_variables(df_filtered, na_threshold = 5000)
df_filtered <- transform_columns(df_filtered, df_features_effect)

df_filtered_pgs <- cbind(df_filtered, get_depression_dataset(df)) 
df_filtered_pgs <- df_filtered_pgs[!is.na(df_filtered_pgs$pgs_dep_mtag),]

#z_statcdna has one level after filtration of NA pgs
table(df_filtered_pgs$z_statcdna)
df_filtered_pgs <- df_filtered_pgs %>% dplyr::select(-any_of(c("z_statcdna")))

#imputation
df_filtered_pgs_imputed <- impute_dataset(df_filtered_pgs, cols_to_exclude = c("pgs_dep_mtag"))

stat_before = calculate_features_importance(df_filtered_pgs, with_progress = FALSE)
stat_after = calculate_features_importance(df_filtered_pgs_imputed, with_progress = FALSE)
filter_bad_imputed_data(df_filtered_pgs_imputed,
                        stat_before,
                        stat_after)

df_final_pgs <- df_filtered_pgs_imputed
df_final_pgs[df_final_pgs$depression_status == -1,]$depression_status = 0
cc = coxph(formula = as.formula(paste("Surv(time, depression_status) ~ ",
                                      "pgs_dep_mtag")),
           data = df_final_pgs)
summary(cc)
plot_hist_stratified(df_filtered_pgs_imputed, "depression_status", "pgs_dep_mtag", "Has depression?")

df_final_pgs <- transform_columns(df_filtered_pgs_imputed,stat_after)



is.factor(df_filtered_pgs_imputed$z_ochh57u)
as.factor()
table(df_filtered_pgs_imputed$depression_status)
