

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
                                  threshold=0.00000000000000001)
{
  pvalues = data.frame(var = df_features_effect$feature_name,
                       pval = df_features_effect$p_value)
  
  pvalues <- pvalues %>% distinct(var, .keep_all = TRUE)
  pvalues <- pvalues[order(pvalues$pval),]
  
  feat <- switch (method,
          "FDR" = FDR(pvalues = pvalues, q = threshold))

  selected_features <- c(rownames(feat$select), c("death_status", "surv_years"))
  dataset_filtered <- dataset %>% select(any_of(selected_features))
  return (dataset_filtered)
}

#correlation filtration
filter_correlated_variables <- function(dataset)
{
  dataset_numeric = dplyr::select_if(dataset, is.numeric)
  # Correlation matrix
  cor_matrix <- cor(dataset_numeric, use="na.or.complete")
  
  # Modify correlation matrix
  cor_matrix_rm <- cor_matrix                  
  cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
  diag(cor_matrix_rm) <- 0
  
  # Remove highly correlated variables
  cols_to_filter <- colnames(dataset_numeric[ , apply(cor_matrix_rm,    
                                 2,
                                 function(x) any(!is.na(x) & x > 0.8))])
  return (dataset %>% select(!(cols_to_filter)))
}

filter_correlated_variables <- function(dataset, na_threshold=6000)
{
  length_of_na_by_variable <- colSums(is.na(dataset))
  most_na_variable_names = names(length_of_na_by_variable[length_of_na_by_variable > na_threshold])
  dataset = dataset %>% select(-any_of(most_na_variable_names))
  
  # Correlation 
  dataset_numeric = dplyr::select_if(dataset, is.numeric)
  dataset_numeric_omitted = na.omit(dataset_numeric)
  cor_matrix <- cor(dataset_numeric_omitted, use="complete.obs")
  
  # Modify correlation matrix
  cor_matrix_rm <- cor_matrix                  
  cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
  diag(cor_matrix_rm) <- 0
  
  # Remove highly correlated variables
  cols_to_filter <- colnames(dataset_numeric[ , apply(cor_matrix_rm,    
                                                      2,
                                                      function(x) any(x > 0.8, na.rm=TRUE))])
  cols_to_filter <- c(cols_to_filter, colnames(dataset_numeric[ , apply(cor_matrix_rm,    
                                                                        2,
                                                                        function(x) any(x < -0.8, na.rm=TRUE))]))
  return (dataset %>% select(-any_of(cols_to_filter)))
}