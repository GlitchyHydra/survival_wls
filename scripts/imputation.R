

impute_dataset <- function(dataset,
                           n_iter=1000,
                           event_col = "death_status",
                           time_col = "time",
                           cols_to_exclude=c())
{
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                        total = n_iter,
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 100)      # Width of the progress bar
  all_cols = colnames(dataset %>% dplyr::select(-any_of(c(event_col, time_col, cols_to_exclude))))
  cols_to_impute = c()
  for (col in all_cols)
  {
    na_count = nrow(dataset[is.na(dataset[,col]),])
    if (na_count > 0)
      cols_to_impute = c(cols_to_impute, col)
  }
  print(cols_to_impute)
  
  
  
  stat_before_imputation = calculate_features_importance(dataset, with_progress = FALSE)
  dataset_final = dataset
  
  min_deltas = setNames(as.list(rep(7000, length(cols_to_impute))), 
               cols_to_impute)
  
  for (i in 1:n_iter)
  {
    pb$tick()
    
    dataset_imputed = dataset
    for (col in cols_to_impute)
    {
      na_count = nrow(dataset_imputed[is.na(dataset_imputed[,col]),])
      dataset_imputed[is.na(dataset_imputed[,col]), col] <- sample(
        x = dataset_imputed[!is.na(dataset_imputed[,col]),col], 
        size = na_count, 
        replace=TRUE)
    }
    
    stat_after_imputation = calculate_features_importance(dataset_imputed, with_progress = FALSE)
    
    for (col in cols_to_impute)
    {
      delta = abs(log10(stat_before_imputation[stat_before_imputation$feature_name == col,]$p_value) - 
                    log10(stat_after_imputation[stat_after_imputation$feature_name == col,]$p_value)
      )
      if (delta < min_deltas[col])
      {
        min_deltas[col] = delta
        dataset_final[,col] = dataset_imputed[,col]
      }
    }
  }
  
  return (dataset_final)
}

filter_bad_imputed_data <- function(dataset_imputed,
                                    impact_table,
                                    impact_table_imputed,
                                    delta_threshold=5,
                                    with_plot=TRUE)
{
  cols_to_test = colnames(dataset_imputed %>% dplyr::select(-any_of(c("death_status", "time" , "age"))))
  
  #original feature effect table preprocessing
  impact_table = impact_table[impact_table$feature_name %in% cols_to_test,]
  impact_table = impact_table[order(impact_table$p_value),]
  impact_table$p_value = log10(impact_table$p_value)
  
  #imputed feature effect table preprocessing
  impact_table_imputed = impact_table_imputed[order(impact_table_imputed$p_value),]
  if (nrow(impact_table_imputed[is.infinite(log10(impact_table_imputed$p_value)),]) > 0)
    impact_table_imputed[is.infinite(log10(impact_table_imputed$p_value)),]$p_value = 1e-303
  impact_table_imputed$p_value = log10(impact_table_imputed$p_value)
  
  x = c()
  y = c()
  for (col in impact_table$feature_name)
  {
    row_x = impact_table[impact_table$feature_name == col,]
    x = c(x, row_x$p_value)
    row_y = impact_table_imputed[impact_table_imputed$feature_name == col,]
    y = c(y, row_y$p_value)
  }
  
  deltas = abs(x-y)
  plot_df_reg = data.frame(
    before_imputation = x,
    after_imputation = y
  )
  
  plot_df = data.frame(
    fitted = x,
    resid = x - y
  )
  
  if(with_plot)
  {
    print(ggplot(plot_df_reg, aes(x=before_imputation, y=after_imputation)) + 
            geom_point(color="blue", shape=18)+
            geom_smooth(method=lm, color="red", linetype="dashed") +
            labs(title = "", 
                 x = "log10(p) before imputation", 
                 y = "log10(p) after imputation") +
            theme(plot.title = element_text(hjust = 0.5, size = 14), 
                  axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
                  axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14)))
    
    print(ggplot(plot_df, aes(x = fitted, y = resid)) +
            geom_point(color="blue") +
            geom_hline(yintercept = 0, color="red", shape=18) +
            labs(title = "", 
                 x = "log10(p) before imputation", 
                 y = "Residuals") +
            theme(plot.title = element_text(hjust = 0.5, size = 14), 
                  axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
                  axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))) 
  }
  
  #plot(1:length(deltas), x-y)
  
  good_imputed_cols = impact_table[deltas < delta_threshold,]$feature_name
  
  return (
    good_imputed_cols = good_imputed_cols
  )
}