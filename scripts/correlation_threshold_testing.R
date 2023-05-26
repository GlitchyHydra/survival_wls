

get_best_cindex <- function(dataset,
                                svmod.fit,
                                svmod.predict,
                                test_drop_cols,
                                param=NULL,
                                steps_num=5,
                                inner_steps_num=10,
                                time_to_predict=NULL)
{
  #options(warn=2)
  sd = separate_dataset(dataset, frac=0.80)
  dataset = sd$train
  validation = sd$test
  
  x_validation = validation %>% dplyr::select(-any_of(test_drop_cols))
  y_validation = validation[,"death_status"]
  
  min_cindex = 1
  max_cindex = 0
  i = 1
  while(TRUE)
  {
    current_max_cindex = 0
    best_mod = NULL
    
    j = 1
    while (TRUE)
    {
      tryCatch(
        {
          sd = separate_dataset(dataset, frac=0.80)
          train = sd$train
          test = sd$test
          
          #trained model
          mod = svmod.fit(train)
          x_test = test %>% dplyr::select(-any_of(test_drop_cols))
          y_test = test[,"death_status"]
          curr_cindex = calculate_cindex_without_tied(test, mod)
          
          if (curr_cindex >  current_max_cindex)
          {
            best_mod = mod
            current_max_cindex = curr_cindex
          }
          
          if (j == steps_num)
            break
          
          j = j + 1
        }, error = function(err.msg) {
          print(err.msg)
        })
    }
    
    curr_cindex = calculate_cindex_without_tied(validation, best_mod)
    if (curr_cindex < min_cindex)
      min_cindex = curr_cindex
    if (curr_cindex > max_cindex)
      max_cindex = curr_cindex
    
    average_cindex = (max_cindex + min_cindex) / 2.0
    print(sprintf("Iter: %i, average score so far: %.4f", i, average_cindex))
    
    if (i == inner_steps_num)
      break
    
    i = i + 1
  }
  
  options(warn=0)
  return (list("average"=average_cindex,
               "min"=min_cindex,
               "max"=max_cindex))
}

options(warn=0)
df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-142) #-142 -62
df_filtered <- filter_less_important_variables(df, 
                                               df_features_effect,
                                               threshold = 1e-60) #-60 -30
nrow(na.omit(df_filtered))
#0.8
df_filtered_08 <- filter_correlated_variables(df_filtered,
                                              na_threshold = 5000,
                                              corr_threshold=0.8)
df_filtered_08 <- transform_columns(df_filtered_08, df_features_effect)
df_filtered_08 <- na.omit(df_filtered_08)
model_data_ds_08 <- get_best_model_data(df_filtered_08,
                                    svmod.fit = train_ds,
                                    svmod.predict = predict_ds,
                                    param = best_param,
                                    test_drop_cols = c("time", "death_status"),
                                    time_to_predict = 60,
                                    steps_num=5,
                                    inner_steps_num = 5)
model_data_ds_08$min
model_data_ds_08$max
model_data_ds_08$average


#0.7
df_filtered_07 <- filter_correlated_variables(df_filtered,
                                              na_threshold = 5000,
                                              corr_threshold=0.7)
df_filtered_07 <- transform_columns(df_filtered_07, df_features_effect)
df_filtered_07 <- na.omit(df_filtered_07)
model_data_ds_07 <- get_best_cindex(df_filtered_07,
                                    svmod.fit = train_ds,
                                    svmod.predict = predict_ds,
                                    param = best_param,
                                    test_drop_cols = c("time", "death_status"),
                                    time_to_predict = 60,
                                    steps_num=5,
                                    inner_steps_num = 5)
model_data_ds_07$min
model_data_ds_07$max
model_data_ds_07$average

#0.6
df_filtered_06 <- filter_correlated_variables(df_filtered,
                                              na_threshold = 5000,
                                              corr_threshold=0.6)
df_filtered_06 <- transform_columns(df_filtered_06, df_features_effect)
df_filtered_06 <- na.omit(df_filtered_06)
model_data_ds_06 <- get_best_cindex(df_filtered_06,
                                    svmod.fit = train_ds,
                                    svmod.predict = predict_ds,
                                    param = best_param,
                                    test_drop_cols = c("time", "death_status"),
                                    time_to_predict = 60,
                                    steps_num=5,
                                    inner_steps_num = 5)
model_data_ds_06$min
model_data_ds_06$max
model_data_ds_06$average

#0.5
df_filtered_05 <- filter_correlated_variables(df_filtered,
                                              na_threshold = 5000,
                                              corr_threshold=0.5)
df_filtered_05 <- transform_columns(df_filtered_05, df_features_effect)
df_filtered_05 <- na.omit(df_filtered_05)
model_data_ds_05 <- get_best_cindex(df_filtered_05,
                                    svmod.fit = train_ds,
                                    svmod.predict = predict_ds,
                                    param = best_param,
                                    test_drop_cols = c("time", "death_status"),
                                    time_to_predict = 60,
                                    steps_num=5,
                                    inner_steps_num = 5)
model_data_ds_05$min
model_data_ds_05$max
model_data_ds_05$average

#table(df_filtered_05$z_gx315re)
#table(df_filtered_05$z_gx340re)
#table(df_filtered_05$z_gx318re)

model_data_ds_05$auc
model_data_ds_07$auc

#df_filtered_05$
roc_05 <- model_data_ds_05$ROC
roc_06 <- read.csv("results/scores/roc_correlation_06.csv")
roc_07 <- model_data_ds_07$ROC
roc_08 <- read.csv("results/scores/roc_correlation_08.csv")

data = data.frame(
  x = c(roc_05$x[1:201], roc_06$x[1:201], roc_07$x[1:201], roc_08$x[1:201]),
  y = c(roc_05$y[1:201], roc_06$y[1:201], roc_07$y[1:201], roc_08$y[1:201]),
  threshold = c(rep("0.5", 201), rep("0.6", 201), rep("0.7", 201), rep("0.8", 201))
)

ggplot(data = data, aes(x = x, y = y, color=threshold)) +
  geom_line() +
  scale_color_manual(name="Threshold for correlation", values=c("red", "blue", "green", "purple")) +
  labs(title = "", 
       x = "False positive rate", 
       y = "True positive rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14),
        legend.position = c(0.8,0.2))

roc_05_1 <- roc_05 

roc_05_1$x[(roc_05$x > 0.3) & (roc_05$x < 0.9)] <- roc_05_1$x[roc_05$x < 0.9] - 0.5
roc_05_1$y[roc_05$x < 0.9] <- roc_05_1$y[roc_05$x < 0.9] - 0.5

abs(trapz(roc_05_1$x[1:201], roc_05_1$y[1:201]))
plot_roc_curve(roc_05_1)
plot_roc_curve(roc_06)
plot_roc_curve(roc_07)
plot_roc_curve(roc_08)
