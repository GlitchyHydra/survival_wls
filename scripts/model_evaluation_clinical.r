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

setwd("E:/education/thesis")
source("scripts/common/data_preprocessing.r")
source("scripts/common/filter_dataset.r")
source("scripts/random_forest_main.r")
source("scripts/evaluation_func.R")
source("scripts/common/plotter.r")

df <- read_wls_dataset(include_pgs_datasets = TRUE)
df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-142) #-142 -62
df_filtered <- filter_less_important_variables(df, 
                                               df_features_effect,
                                               threshold = 1e-60) #-60 -30
df_filtered <- filter_correlated_variables(df_filtered, na_threshold = 5000)
df_filtered <- transform_columns(df_filtered, df_features_effect)

#imputation
df_filtered_imp <- impute_dataset(df_filtered,
               n_iter=1000)
stat_imp = calculate_features_importance(df_filtered_imp)
cols_to_choose <- filter_bad_imputed_data(df_filtered_imp,
                        fe,
                        stat_imp,
                        delta_threshold = 10)
df_final <- df_filtered_imp %>% dplyr::select(any_of(c(cols_to_choose, "death_status", "time"))) 

#choose time to predict
time_to_predict = 60

#---------------------------------Random Forest model------------------------------------------
#transform by hazard coefficients
hazard_coeffs <- extract_coeff_data(df_final)
df_hazard_final <- transform_by_hazards(df_final, hazard_coeffs)

#remove who has age lower than age for prediction (threshold) and not dead (censored)

predicat = !((df_hazard_final$time < time_to_predict) & (df_hazard_final$death_status==0))
df_hazard_not_censored <- df_hazard_final[predicat,]
write.csv(df_hazard_not_censored, "results/df_hazard_not_censored.csv", row.names=FALSE)

#mtry_good = sqrt(ncol(df_filtered_hazard_omitted)) for classification
mtry_good = ncol(df_filtered_hazard_omitted)#/3
rf_params = list(
  "ntree" = c(100, 200, 400, 500, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000,
              2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800,
              4000), #n_estimators
  "max.depth" = c(4:10, NULL), #Max_Depth
  "mtry" = c(4:ncol(df_filtered_hazard_omitted)), #max_features
  "replace" = c(TRUE, FALSE)
)

best_params <- hyperparams_tuning(dataset=df_filtered_hazard_final,
                   svmod.fit = train_rf,
                   svmod.predict = predict_rf,
                   metric = "AUC",
                   test_drop_cols = c("death_status"),
                   is_random = TRUE,
                   params = rf_params,
                   k_folds = 5)

model_data <- get_best_model_data(df_hazard_final,
                                  svmod.fit = train_rf,
                                  svmod.predict = predict_rf,
                                  time_to_predict = 60,
                                  test_drop_cols = c("death_status"),
                                  steps_num=5,
                                  inner_steps_num = 5)

rf.mod = model_data$mod
model_data = data.frame(
  ROC = read.csv("results/scores/roc_rf_clinical.csv")
)
md = read.csv("results/scores/roc_rf_clinical.csv")
plot_roc_curve(md)
model_data$auc

which(row.names(importance(rf.mod)) %in% c("surv_years"))

importance(rf.mod)[187,]
varImpPlot(rf.mod)


#---------------------------------DeepSurv model------------------------------------------
max_batch_num = get_max_batch_size(df_final, 5)
dsurv_params = list(
  "num_nodes" = list(
    "num_layers" = c(1L:5L),
    "num_units" = c(2L:64L)
  ),
  "activation" = c("celu", "elu", "gelu", "glu", "hardshrink", "hardsigmoid", "hardswish",
                   "hardtanh", "relu6", "leakyrelu", "logsigmoid", "logsoftmax",
                   "prelu", "rrelu", "relu", "selu", "sigmoid",
                   "softmax", "softmax2d", "softmin", "softplus", "softshrink", "softsign",
                   "tanh", "tanhshrink", "threshold"),
  "optimizer" = c("adadelta", "adagrad", "adam", "adamax","rmsprop", "sgd"),
  "lr" = sapply(50:5000, function(x) round(x * 1e-5, 5)),
  "frac" = sapply(0:35, function(x) round(x * 1e-2, 2)),
  "epochs" = c(50:5000),
  "dropout" = sapply(1:4, function(x) round(0.1*x, 2)),
  "batch_norm" = c(TRUE, FALSE),
  "batch_size" = sapply(4:max_batch_num, function(x) 2^x)
)
a = 5e-4 -1
best_params_ds <- hyperparams_tuning(df_filtered,
                                  svmod.fit = train_ds,
                                  svmod.predict = predict_ds,
                                  metric = "cindex",
                                  test_drop_cols = c("time", "death_status"),
                                  is_random = TRUE,
                                  params = dsurv_params,
                                  k_folds = 5)

best_param <- list(
  "num_nodes" = c(48L, 9L, 22L, 44L),
  "activation" = "rrelu",
  "optimizer" = "adamax",
  "lr" = 0.00629,
  "frac" = 0.12,
  "epochs" = 3802,
  "dropout" = 0.1,
  "batch_norm" = TRUE,
  "batch_size" = 128
)

model_data_ds_tuned <- get_best_model_data(df_final,
                                  svmod.fit = train_ds,
                                  svmod.predict = predict_ds,
                                  param = best_param,
                                  test_drop_cols = c("time", "death_status"),
                                  time_to_predict = 60,
                                  steps_num=5,
                                  inner_steps_num = 5)

plot_roc_curve(model_data_ds_tuned$ROC)
model_data_ds_tuned$auc
ds.mod_tuned = model_data_ds$mod

concordance(Surv(time, death_status) ~ predict(ds.mod, tmp, type="risk"),
            tmp)$concordance

cindex(predict(ds.mod, tmp, type="risk"),
       tmp$time,
       formula = Surv(time, death_status) ~ predict(ds.mod, tmp, type="risk"),
       data = tmp)

cindex(predict(model_data_ds_tuned$mod, df_imputed, type="risk"),
       df_imputed$time)


roc_rf = md
roc_ds = model_data_ds_tuned$ROC

abs(trapz(md$x[1:201], md$y[1:201]))


data = data.frame(
  x = c(roc_ds$x[1:202], roc_rf$x[1:202]),
  y = c(roc_ds$y[1:202], roc_rf$y[1:202]),
  threshold = c(rep("DeepSurv", 202), rep("Random Forest", 202))
)

ggplot(data = data, aes(x = x, y = y, color=threshold)) +
  geom_line() +
  scale_color_manual(name="Model", values=c("red", "blue")) +
  labs(title = "", 
       x = "False positive rate", 
       y = "True positive rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14),
        legend.position = c(0.8,0.2))


plot_roc_curve_compared()
