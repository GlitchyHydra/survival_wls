library(dplyr)
library(progress)
library(survival)

setwd("E:/education/thesis")

source("scripts/common/data_preprocessing.r")

calculate_features_importance <- function(df_surv, save_name=NULL, with_progress=TRUE)
{
  df_types <- lapply(df_surv, class)
  cols <- colnames(df_surv %>% dplyr::select(-any_of(c("death_status", "time"))))
  
  if (with_progress)
  {
    pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                          total = length(cols),
                          complete = "=",   # Completion bar character
                          incomplete = "-", # Incomplete bar character
                          current = ">",    # Current bar character
                          clear = FALSE,    # If TRUE, clears the bar when finish
                          width = 100)      # Width of the progress bar
  }
  
  var_names = c()
  is_factor = c()
  pvals = c()
  
  for (col in cols)
  {
    if (with_progress)
      pb$tick()
    
    if (df_types[col] == "character" && length(table(df_surv[col])) > 15)
    {
      next
    }
    
    df_tmp = df_surv %>% select(any_of(c(col, "death_status", "time")))
    
    #distinguish factor vs numeric
    unique_vals_len <- length(unique(df_tmp[!is.na(df_tmp[,col]),col]))
    is_cat = unique_vals_len < 15 
    
    if (is_cat)
    {
      #to factor
      df_tmp[,col] <- as.factor(df_tmp[,col])
      
      #check if some value has no dead persons
      lvls <- unique(df_tmp[!is.na(df_tmp[,col]), col])
      for (l in lvls)
      {
        death_num = nrow(df_tmp[which((df_tmp[,col] == l) & (df_tmp$death_status == 1)),])
        death_lvl_length = length(unique(df_tmp[which(df_tmp[,col] == l),]$death_status))
        if (death_num < 4 || death_lvl_length < 2)
          df_tmp = df_tmp[which((df_tmp[,col] != l)),]
      }
      
      df_tmp = droplevels(df_tmp)
    }
    
    if (is_cat && length(levels(df_tmp[,col])) < 2)
      next
    
    was_error = FALSE
    tryCatch(
      {
        cox.mod_tmp <- coxph(formula = as.formula(paste("Surv(time, death_status) ~ ",
                                                        col)),
                             data = df_tmp)
        s <- summary(cox.mod_tmp)
        number_of_features = length(s$coefficients) / 5
        p_start_index <- number_of_features * 4 + 1
        p_end_index <- number_of_features * 5
        p_value = min(s$coefficients[p_start_index:p_end_index])
        var_names = c(var_names, col)
        is_factor = c(is_factor, is_cat)
        pvals = c(pvals, p_value)
      }, error = function(err.msg) {
        print(err.msg)
      })
  }
  
  df_stats <- data.frame(
    feature_name = var_names,
    p_value = pvals,
    is_cat = is_factor
  )
  if (!is.null(save_name))
    write.csv(df_stats, paste("results/features impact/", save_name, sep=""), row.names=TRUE)
  
  return(df_stats)
}

filename = "cox_impact_imputed3.csv"
df <- read.csv("results/imputed/imputed.csv", header=TRUE)
df_imputed <- read.csv("results/imputed/imputed2.csv", header=TRUE)
options(warn=2)
calculate_features_importance(df_imputed, filename)

imp_by_sampling <- function(dataset, col)
{
  samp_size <- nrow(dataset[is.na(dataset[,col]),])
  imp_data <- sample(dataset[!is.na(dataset[,col]), col], size = samp_size )
  dataset[is.na(dataset[,col]), col] <- imp_data
  return (dataset[, col])
}

dd = df[,c("z_ocmzu","z_rb028rec","z_rb029re","z_rb036re","z_ra016rem",
"z_rb039re", "z_rd00715", "z_rd00716", "z_rd00717", "z_rd00718",
"z_iqsource", "z_rb038re", "z_statcdna", "z_rb037re", "z_rb035re",
"z_rb034re", "z_rd003sk", "z_hhstat75", "z_rd00710", "z_rd00709",
"z_rd00708", "z_rd00711", "z_rd00713", "z_rd00712",
"z_rd00707","z_rb027re",
"z_statc11",
"z_rc036sp",
"z_gj002sk",
"z_gd01101",
"z_gx315re",
"z_gx317re",
"z_gd01505",
"z_gd01506",
"z_my008re",
"z_gx340re",
"z_gx346re",
"z_gx318re",
"z_xcat53_1",
"z_xcat60_1",
"z_xcat47_1",
"z_xcat40_1",
"z_xcat55_1")]

for (d in colnames(dd))
{
  df_imputed[,d] <- as.factor(df_imputed[,d])
  df_imputed[,d] <- imp_by_sampling(dd, d)
}

nrow(na.omit(df_imputed))

imp_sampled_data <- imp_by_sampling(dd, "z_xcat55_1")



data = data.frame(
  data = c(df_imputed$z_xcat55_1, imp_sampled_data),#imp2$ximp$z_gj002sk, imp_sampled_data),
  type = c(rep("current", each=nrow(df_imputed)),
           #rep("forest", each=nrow(df_imputed)),
           rep("sampled", each=nrow(df_imputed))
  )
)

ggplot(data,
       aes(x=data, fill = as.factor(type))) +
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
  #geom_density(adjust=0.1, alpha=.0)  +
  labs(title = "density of pgs", 
       x = "pgs risk score", 
       y = "density") +
  scale_color_discrete(name = "depression status") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))

z_gx


function()
{
  filename = "cox_impact.csv"
  df_surv <- read_wls_dataset(include_pgs_datasets = TRUE)
  df <- df %>% dplyr::select(-any_of(c("srel1", "srel2", "srel3", "srel4")))
  options(warn=2)
  calculate_features_importance(df, filename)
}


df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-142)

df_features_effect_imputed <- read.csv("results/features impact/cox_impact_imputed.csv")
df_features_effect_imputed <- (df_features_effect_imputed[order(df_features_effect_imputed$p_value, decreasing=FALSE),])

df$z_gx343re
table(df$z_gx346re)
df_features_effect_imputed[df_features_effect_imputed$feature_name == "z_gx346re",]$p_value
df_features_effect[df_features_effect$feature_name == "z_gx346re",]$p_value

total_error = c()
for (feature_name in df_features_effect_imputed$feature_name)
{
  p_value_was = df_features_effect[df_features_effect$feature_name == feature_name,]$p_value  
  p_value_became = df_features_effect_imputed[df_features_effect_imputed$feature_name == feature_name,]$p_value
  p_value_delta = abs(p_value_became - p_value_was)
  total_error = c(total_error, p_value_delta**2)
}
sum(total_error)/93

m = mean(total_error)

