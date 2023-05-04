library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(haven)
library(survminer)
library(survival)
library(pROC)

setwd("E:/education/thesis")

source("data_preprocessing.R")


df_surv <- df_surv %>% select(-c("z_jn902rer", "z_jn903rer", "z_jn904rer",
                                 "z_jn905rer", "z_jn906rer", "z_jn907rer",
                                 "z_jn908rer",  "z_jn909rer",  "z_jn910rer",
                                 "z_jn911rer"))
df_types <- lapply(df_surv, class)

length(unique(df_surv$z_q1i555re))
grep("z_q1i555re", colnames(df_surv))
unique(df_surv$z_jn906rer)
table(df_surv$z_q1i565re)

col_names = colnames(df_surv)
col_names = col_names[13618:13773]


df_stats <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("feature_name", "p_value", "is_cat")
colnames(df_stats) <- x

curr_index = 1;
for (col in col_names)
{
  if (df_types[col] == "character" && length(table(df_surv[col])) > 15)
  {
    next
  }
  
  print(paste(curr_index, ": ", col))
  
  tmp_df <- df_surv %>% select(starts_with("death_status") |
                                               starts_with("surv_years") |
                                               starts_with(col))

  #remove nan
  tmp_df <- tmp_df[!is.na(tmp_df[,col]),]
  
  #distinguish categorical vs continuous
  unique_vals_len <- length(unique(tmp_df[,col]))
  is_cat = unique_vals_len < 15 
  
  #categorical
  if (is_cat)
  {
    #to factor
    tmp_df[,col] <- as.factor(tmp_df[,col])
    
    #check if some value has no dead persons
    cat_vals <- unique(tmp_df[,col])
    for (cat_val in cat_vals)
    {
      l = length(unique(tmp_df[tmp_df[,col] == cat_val,]$death_status))
      if (l != 2)
      {
        tmp_df <- tmp_df[tmp_df[,col] != cat_val,]
      }
    }
  }
  
   
  
  cox.mod_tmp <- coxph(formula = as.formula(paste("Surv(surv_years, death_status) ~ ",
                                                  col)),
                       data = df_surv)
  s <- summary(cox.mod_tmp)
  
  number_of_param = 5
  number_of_features = length(s$coefficients) / number_of_param
  p_vals_index <- number_of_features * 4
  
  if (number_of_features > 15)
  {
    next
  }
  
  for (i in  1:number_of_features)
  {
    #add to final data frame name, p_value
    #check how recieve feature name
    #may be only one feature from categorical will appear
    #check whether it true
    df_stats[curr_index,]<-c(col, s$coefficients[p_vals_index + i], is_cat)
    curr_index = curr_index + 1
  }
  
}

write.csv(df_stats, "results/features impact/cox_impact.csv", row.names=TRUE)
table(df_surv$xstat93m)
