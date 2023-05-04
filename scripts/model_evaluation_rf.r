library(caTools)
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
library(rlist)
library(fuzzySim)
library(progress)
library(progress)
library(randomForest)
library(Rtools)
library(randomForestSRC)
library(survivalmodels)

setwd("E:/education/thesis")
source("scripts/common/data_preprocessing.r")
source("scripts/common/filter_dataset.r")
source("scripts/random_forest_main.r")
source("scripts/evaluation_func.R")
source("scripts/common/plotter.r")

df <- read_wls_dataset(include_pgs_datasets = TRUE);
table(df$time)
df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-40)
df_filtered <- filter_less_important_variables(df, df_features_effect, threshold = 1e-17)
df_filtered <- filter_correlated_variables(df_filtered)
df_filtered <- transform_columns(df_filtered, df_features_effect)
df_filtered <- preprocess_dataset(df_filtered)

levels(df_filtered$selsibtype) <- c(0, 1, 2)

#TODO move to read
df_filtered$death_status <- df_filtered$death_status + 0 

df_filtered_omitted <- na.omit(df_filtered)

df_filtered_final <- df_filtered_omitted


#transform by hazard coefficients
hazard_coeffs <- extract_coeff_data(df_filtered)
df_filtered_hazard <- transform_by_hazards(df_filtered, hazard_coeffs)
df_filtered_hazard_omitted <- na.omit(df_filtered_hazard)

age_to_predict = 80

#-----------------------------------------------------
# nrow(df[(df$rtype=="s") & (df$death_status==TRUE),])
# 2666
# nrow(df[(df$rtype=="s") & (df$death_status==FALSE),])
# 3945
# death_ratio = 2666/6611
# nrow(df[(df$rtype=="g") & (df$death_status==TRUE),])
# 3385
# nrow(df[(df$rtype=="g") & (df$death_status==FALSE),])
# 6339
# death_ratio = 3385/9724
#separate the whole dataset to grad and siblings. Check the model accuracy

#check some correlated variables
#sort(cor_matrix_rm[,"z_gx356re"])
#z_gx356re	R5 Has a doctor ever told you that you had a stroke?
#z_gx358re	R5 Do you still have any remaining health problems because of your stroke, such as muscle weakness or difficulty speaking?
#z_gx357re	R5 In what year did you last have a stroke?
#--------build one model-----------
#sd <- separate_dataset(df_filtered_final_omitted)
#df_train <- na.omit(sd$train)
#df_test <- na.omit(sd$test)
#x_train = df_train[,!names(df_train) %in% c("death_status")]
#y_train = df_train[,"death_status"]
# rf <- randomForest(x=x_train, y=y_train, 
#                    importance = TRUE, proximity = TRUE,
#                    na.action=na.omit)
# 
# x_test = df_test[,!names(df_test) %in% c("death_status")]
# x_test = na.omit(x_test)
# y_test = df_test[,"death_status"]
# 
# auc(y_test, predict(rf, x_test))

#with rtype and transformed by hazard Area under the curve: 0.9114
#

#---------------------------------Random Forest model------------------------------------------


#remove who has age lower than age for prediction (threshold) and not dead (censored)
80
predicat = !((df_filtered_hazard_omitted$surv_years < age_to_predict) & (df_filtered_hazard_omitted$death_status==FALSE))
nrow(df_filtered_hazard_omitted[predicat,])
length(df_filtered_omitted[df_filtered_omitted$surv_years < age_to_predict,]$death_status == TRUE)

#predict that survived to certain age
df_filtered_hazard_final <- df_filtered_hazard_omitted[predicat,]
#df_filtered_hazard_final <- (df_filtered_hazard_final %>% select(!c("surv_years")))

#mtry_good = sqrt(ncol(df_filtered_hazard_omitted)) for classification
mtry_good = ncol(df_filtered_hazard_omitted)/3
rf_params = list(
  "ntree" = c(100, 200, 400, 500, 600, 1000, 2000, 4000), #n_estimators
  "max.depth" = c(4,5,6,7,8,9,10, NULL), #Max_Depth
  "mtry" = c(4, 22, 44, mtry_good, mtry_good + 10) #max_features
)

best_params <- hyperparams_tuning(df_filtered_hazard_final,
                   svmod.fit = train_rf,
                   svmod.predict = predict_rf,
                   test_drop_cols = c("death_status"),
                   params = rf_params,
                   k_folds = 5)

model_data <- get_best_model_data(df_filtered_hazard_final,
                                  svmod.fit = train_rf,
                                  svmod.predict = predict_rf,
                                  y_age = 80,
                                  svmod.test.preproc = preproc_test_rf,
                                  test_drop_cols = c("death_status"),
                                  steps_num=5)

rf.mod = model_data$mod
plot_roc_curve(model_data$ROC)
model_data$auc

which(row.names(importance(rf.mod)) %in% c("surv_years"))

importance(rf.mod)[187,]
varImpPlot(rf.mod)


#---------------------------------Random Survival Forest model------------------------------------------
rfsrc(as.formula(Surv(surv_years, death_status) ~ .), df_filtered_omitted)
coxph(as.formula(Surv(surv_years, death_status) ~ .), df_filtered_omitted)
is.factor(df_filtered_omitted$z_xcat56_1)

#---------------------------------DeepSurv model------------------------------------------

#TODO
dsurv_params = list(
  "frac" = c(100, 200, 400, 500, 600, 1000, 2000, 4000),
  "activation" = c("celu", "relu", "softmax"), 
  "num_nodes" = c(4, 22, 44),
  "dropout" = c(),
  "epochs" = c(100, 400, 800),
  "batch_size" = c()
)

best_params_ds <- hyperparams_tuning(df_filtered_final,
                                  svmod.fit = train_ds,
                                  svmod.predict = predict_ds,
                                  test_drop_cols = c("surv_years", "death_status"),
                                  params = dsurv_params,
                                  k_folds = 5)

model_data_ds <- get_best_model_data(df_filtered_final,
                                  svmod.fit = train_ds,
                                  svmod.predict = predict_ds,
                                  test_drop_cols = c("surv_years", "death_status"),
                                  y_age = 80,
                                  steps_num=5)

plot_roc_curve(model_data_ds$ROC)
model_data_ds$auc
ds.mod = model_data_ds$mod
remove(ds.mod)
concordance(ds.mod)

predict(model_data_ds$mod, df_filtered_final[1,1:186], type="survival")
#with NA

model_data_ds2 <- get_best_model_data(df_filtered,
                                     svmod.fit = train_ds,
                                     svmod.predict = predict_ds,
                                     test_drop_cols = c("surv_years", "death_status"),
                                     y_age = 80,
                                     steps_num=5)

#df_filtered_omitted <- df_filtered_omitted %>% dplyr::select(-any_of(c("selsibtype")))

log.path = "errors"
tryCatch(
  {
    predict(dsurv.mod, x[1:5])
  }, error = function(err.msg) {
    write(toString(err.msg), log.path, append=TRUE)
  })




