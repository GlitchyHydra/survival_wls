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
library(randomForest)

setwd("E:/education/thesis")
source("scripts/commom/data_preprocessing.r")
source("evaluation_func.R")
source("scripts/plotter.r")

df <- read_wls_dataset(include_pgs_datasets=TRUE);
df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-40)

print(rf)
png(file ="randomForestRegression.png")
plot(rf)
dev.off()
#--------------------------- Correlation matrix exclusion-------------------------------
#df_final <- df_surv %>% select(selected_features) TODO remove
df_final <- df_final[, !names(df_final) %in% c("sflag", "brdyhh", "sgrade",
                                                       "swicr_t", "swicr_hn", "shnrs_t",
                                                       "shnrs", "xstat93p", "nx043red")]
#TODO place to filter_dataset after FDR
data_new_transformed <- transform_columns(data_new, features_impact)
data_new_transformed <- preprocess_dataset(data_new_transformed)


#check model
data_new_transformed[which(data_new_transformed$z_ixw11rer == -1),]$z_gm020re
data_new_transformed[which(data_new_transformed$z_ixw11rer == -1),]$death_status

kk = row.names(data_new_transformed[which(data_new_transformed$z_ixw11rer == -1),])
which(data_new_transformed[which(data_new_transformed$z_ixw11rer == -1),]$death_status == FALSE)

data_new_transformed[kk,]$surv_years
data_new_transformed[kk,]$death_status


param_str = get_param_str(colnames(data_new_transformed))

#explore multivariate model problems
sel <- c(colnames(data_new_transformed)[c(1,3)], c("death_status", "surv_years"))
param_str = get_param_str(sel)
cox.mod = coxph(formula = as.formula(paste("Surv(surv_years, death_status) ~ ",
                                           param_str)),
                data = data_new_transformed[, sel], na.action = na.omit)
summary(cox.mod)
table(data_new_transformed$z_gm020re)
cox.zph(cox.mod)

ggforest(cox.mod)

#try predict
exp(-predict(cox.mod, data_new_transformed[1:100,], type="expected"))

data_new_transformed[2,c(1,3)]
data_new_transformed[3,c(1,3)]

#try to delete full na row
data_new_transformed_fixed <- data_new_transformed[!(row.names(data_new_transformed) %in% c(4)),]

get_param_str(colnames())
data_new_transformed_fixed[2,c(1,3)]
cox.mod = coxph(formula = as.formula(paste("Surv(surv_years, death_status) ~ ",
                                           param_str)),
                data = data_new_transformed_fixed[, sel])
summary(cox.mod)


#----------------------------------------------------------
seed <- 13
set.seed(seed)

exp(-predict(cox.mod, test[2,], type="expected"))

#evaluate model
roc_data <- get_roc_curve_data(data_new_transformed)
roc_data$auc
plot_roc_curve(roc_data$data)

one_level_columns = c()
for (i in range(ncol(data_new)))
{
  l = length(unique(data_new[, i]))
  if (l <= 1)
  {
    print("some")
    col_name = colnames(data_new)[i]
    one_level_columns = c(one_level_columns, col_name)
  }
}

data_new %>% group_by()

