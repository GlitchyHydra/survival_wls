library(dplyr)

#z_gx346re	R5 Has a doctor ever told you that you have high blood sugar?
#z_gx347re	R5 In what year were you first diagnosed with high blood sugar?
#z_gx356re	R5 Has a doctor ever told you that you had a stroke?
#z_gx357re	R5 In what year did you last have a stroke?
#z_gx352re	R5 Did you have a heart attack or myocardial infarction?
#z_gx353re	R5 In what year did you have your most recent heart attack or myocardial infarction?
#z_gx342re	R5 Has a doctor ever told you that you have diabetes?
#z_gx343re	R5 In what year were you first diagnosed with diabetes?

cvd_cols <- c("z_gx346re", "z_gx347re", "z_gx356re","z_gx357re",
              "z_gx352re","z_gx353re", "z_gx342re","z_gx343re",
              "z_gx341re", "death_status", "time", "z_brdxdy")
df_cvd <- df %>% dplyr::select(any_of(cvd_cols))

transfom_incident_year_to_age <- function(dataset, col)
{
  dataset[which(dataset[,col] > 0), col] = 
    dataset[which(dataset[,col] > 0), col] - (1900 + dataset[which(dataset[,col] > 0), ]$z_brdxdy)
  return (
    dataset
  )
}

df_cvd <- transfom_incident_year_to_age(df_cvd, "z_gx347re")
df_cvd <- transfom_incident_year_to_age(df_cvd, "z_gx357re")
df_cvd <- transfom_incident_year_to_age(df_cvd, "z_gx353re")
df_cvd <- transfom_incident_year_to_age(df_cvd, "z_gx343re")
df_cvd$z_gx346re <- as.factor(df_cvd$z_gx346re)
df_cvd$z_gx356re <- as.factor(df_cvd$z_gx356re)
df_cvd$z_gx352re <- as.factor(df_cvd$z_gx352re)
df_cvd$z_gx342re <- as.factor(df_cvd$z_gx342re)
df_cvd$z_gx341re <- as.factor(df_cvd$z_gx341re)

col = "z_gx346re"
levels(df_cvd[,col])
tmp = df_cvd 
tmp = tmp[which(!tmp[,col] %in% c(-1, -2, -3)),] 
plot_kaplan_factorized(tmp, col, "", scale_lvls = c("Yes", "No"))

cvd_best_imputed <- read.csv("data/datasets/cvd_best_imputed.csv")
filename = "cox_impact_cvd_imputed.csv"
cvd_best_imputed$death_status <- df$death_status
cvd_best_imputed$time <- df$time
stats= calculate_features_importance(cvd_best_imputed, filename)

fe_cvd <- read.csv(file="results/features impact/cox_impact_cvd.csv", sep=",")
fe_imputed <- read.csv(file="results/features impact/cox_impact_imputed2.csv", sep=",") 
filter_bad_imputed_data(cvd_best_imputed,
                        fe_cvd,
                        stats,
                        delta_threshold=13)

df_cvd_final <- cvd_best_imputed %>% dplyr::select(-any_of("z_gx347re"))
df_cvd_final <- df_cvd_final %>% dplyr::select(any_of( c("z_gx343re","z_gx352re", "z_gx353re",
                                                       "z_gx357re", "z_gx341re", "z_gx356re",
                                                       "z_gx342re", "death_status", "time")))
is.factor(df_cvd_final$z_gx352re)

df_cvd_final$z_gx356re <- as.factor(df_cvd_final$z_gx356re)
df_cvd_final$z_gx352re <- as.factor(df_cvd_final$z_gx352re)
df_cvd_final$z_gx342re <- as.factor(df_cvd_final$z_gx342re)
df_cvd_final$z_gx341re <- as.factor(df_cvd_final$z_gx341re)

model_data_ds_cvd <- get_best_model_data(df_cvd_final,
                                         svmod.fit = train_ds,
                                         svmod.predict = predict_ds,
                                         param = best_param,
                                         test_drop_cols = c("time", "death_status"),
                                         time_to_predict = 60,
                                         steps_num=5,
                                         inner_steps_num = 5)
plot_roc_curve(model_data_ds_cvd$ROC)



#nrow(df_cvd[is.na(df_cvd$z_gx347re),])
#nrow(df_cvd[is.na(df_cvd$z_gx357re),])
#nrow(df_cvd[is.na(df_cvd$z_gx353re),])
#nrow(df_cvd[is.na(df_cvd$z_gx343re),])
#nrow(df_cvd[is.na(df_cvd$z_gx346re),])
#nrow(df_cvd[is.na(df_cvd$z_gx356re),])
#nrow(df_cvd[is.na(df_cvd$z_gx352re),])
#nrow(df_cvd[is.na(df_cvd$z_gx342re),])
#nrow(df_cvd[is.na(df_cvd$z_gx341re),])

sample_in_range <- function(dataset, col_to_impute)
{
  min_age = min(dataset[which(dataset[, col_to_impute] > 0), col_to_impute])
  max_age = max(dataset[which(dataset[, col_to_impute] > 0), col_to_impute])
  n = nrow(dataset[is.na(dataset[, col_to_impute]),])
  
  count2 = nrow(dataset[which(dataset[,col_to_impute] == -2),])
  count1 = nrow(dataset[which(dataset[,col_to_impute] == -1),])
  sample_data = c(min_age:max_age, rep(-1, count1), rep(-2, count2))
  
  dataset[is.na(dataset[, col_to_impute]), col_to_impute] =
    sample(sample_data, size=n, replace=TRUE)
  return(dataset)
}



