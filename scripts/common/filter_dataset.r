

#' Filter the less important variables from a dataset.
#' 
#' @param dataset A data frame with data.
#' @param df_features_effect A data frame with information about variables effect
#' @param method A method name for filtering.
#' @param threshold A threshold value to be used in filter
#' @returns A table.
#' @examples
#' filter_less_important_variables(df, df_fe)
filter_less_important_variables <- function(dataset,
                                  df_features_effect,
                                  method="FDR",
                                  pgs_to_include=NULL,
                                  threshold=0.00000000000000001)
{
  pvalues = data.frame(var = df_features_effect$feature_name,
                       pval = df_features_effect$p_value)
  
  pvalues = pvalues %>% distinct(var, .keep_all = TRUE)
  pvalues = pvalues[order(pvalues$pval),]
  
  feat = switch (method,
          "FDR" = FDR(pvalues = pvalues, q = threshold))

  selected_features = c(rownames(feat$select), c("death_status", "time"))
  if (!is.null(pgs_to_include))
    selected_features = c(selected_features, pgs_to_include)
  
  dataset_filtered = dataset %>% select(any_of(selected_features))
  return (dataset_filtered)
}

filter_correlated_variables <- function(dataset, 
                                        na_threshold=6000,
                                        pgs_to_ignore=NULL,
                                        corr_threshold=0.8)
{
  length_of_na_by_variable <- colSums(is.na(dataset))
  cols_count = ncol(dataset)
  most_na_variable_names = names(length_of_na_by_variable[length_of_na_by_variable > na_threshold])
  if (!is.null(pgs_to_ignore))
    most_na_variable_names = most_na_variable_names[!most_na_variable_names %in% pgs_to_ignore]
  dataset = dataset %>% select(-any_of(most_na_variable_names))
  filtered_cols_count = ncol(dataset)
  
  print(sprintf("Na filtration (selected %1.0f, excluded %1.0f (with threshold > %1.0f))",
                filtered_cols_count,
                cols_count - filtered_cols_count,
                na_threshold))
  
  cols_count = filtered_cols_count
  # Correlation 
  dataset_numeric = dplyr::select_if(dataset, is.numeric)
  dataset_numeric_omitted = na.omit(dataset_numeric)
  cor_matrix = cor(dataset_numeric_omitted, use="complete.obs")
  
  # Modify correlation matrix
  cor_matrix_rm = cor_matrix                  
  cor_matrix_rm[upper.tri(cor_matrix_rm)] = 0
  diag(cor_matrix_rm) = 0
  
  # Remove highly correlated variables
  cols_to_filter = colnames(dataset_numeric[ , apply(cor_matrix_rm,    
                                                      2,
                                                      function(x) any(x > corr_threshold, na.rm=TRUE))])
  cols_to_filter = c(cols_to_filter, colnames(dataset_numeric[ , apply(cor_matrix_rm,    
                                                                        2,
                                                                        function(x) any(x < -corr_threshold, na.rm=TRUE))]))
  
  dataset = dataset %>% select(-any_of(cols_to_filter))
  filtered_cols_count = ncol(dataset)
  print(sprintf("Correlation filtration (selected %1.0f, excluded %1.0f (with threshold > %.1f))",
                filtered_cols_count,
                cols_count - filtered_cols_count,
                corr_threshold))
  
  return (dataset)
}