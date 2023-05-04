
#extract hazard coefficients for every column
extract_coeff_data <- function(dataset)
{
  #TODO if surv_years, death_status not at the end of dataset
  l = length(colnames(dataset)) - 2
  cols = colnames(dataset)[1:l]
  total_coeffs = c()
  feature_names = c()
  
  for (col in cols)
  {
    cox.mod = coxph(formula = as.formula(paste("Surv(surv_years, death_status) ~ ",
                                               col)),
                    data = dataset)
    
    if (!is.factor(dataset[,col]))
    {
      total_coeffs = c(total_coeffs, summary(cox.mod)$coefficients[2])
      feature_names = c(feature_names, col)
      next
    }
    
    levels_length = length(levels(dataset[,col])) - 1
    
    coeffs = summary(cox.mod)$coefficients[(levels_length + 1):(levels_length * 2)]
    coeffs = c(1, coeffs)
    total_coeffs = c(total_coeffs, coeffs)
    
    for (lvl in levels(dataset[,col]))
    {
      feature_names = c(feature_names, paste(col, lvl, sep=""))
    }
  }
  
  coeff_df = data.frame(
    name = feature_names,
    coeff = total_coeffs 
  )
  return (coeff_df)
}

transform_by_hazards <- function(dataset, hazard_df)
{
  l = length(colnames(dataset)) -2
  cols = colnames(dataset)[1:l]
  for (col in cols)
  {
    
    if (!is.factor(dataset[,col]))
    {
      dataset[,col] = hazard_df[hazard_df$name == col,]$coeff
      next
    }
    
    lvls = levels(dataset[,col])
    dataset[,col] = as.numeric(dataset[,col])
    
    for (lvl in lvls)
    {
      feature_name = paste(col, lvl, sep="")
      dataset[which(dataset[,col] == lvl), col] = hazard_df[hazard_df$name == feature_name,]$coeff
    }
  }
  
  dataset$death_status = as.numeric(dataset$death_status + 0) 
  
  return (dataset)
}

