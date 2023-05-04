library(haven)
library(progress)
library(dplyr)


#table(df_surv[df_surv$start_year == -1,]$z_livgrad)
#table(df_surv[df_surv$start_year == -1,]$rtype)
#table(df_surv[df_surv$start_year == -1,]$z_wrwoud)
#table(df_surv[df_surv$start_year == -1,]$z_age75)
#table(df_surv[df_surv$start_year == -1,]$z_ra016rey)
#table(df_surv[df_surv$start_year == -1,]$z_ga003re)
#table(df_surv[df_surv$start_year == -1,]$z_ha003re)
#table(df_surv[df_surv$start_year == -1,]$z_q1a003re)
#table(df_surv[df_surv$time == 0,]$start_year)
#table(df_surv[df_surv$time == 0,]$z_livgrad)
#table(df_surv[df_surv$time == 1,]$z_livgrad)
#table(df_surv[df_surv$time == 2,]$z_livgrad)
#table(df_surv[df_surv$time == 3,]$z_livgrad)
#table(df_surv[df_surv$time == 4,]$z_livgrad)
#table(df_surv[df_surv$time == 5,]$z_livgrad)
#table(df_surv[df_surv$time == 6,]$z_livgrad)

#table(df_surv[df_surv$time == 1,]$start_year)
#11 have died at the same year of the first interview,
#however, the rest is alive, but the 

#table(df_surv[(df_surv$time == 0) & (df_surv$start_year == 1976),]$z_jbcrsa)
#table(df_surv[(df_surv$time == 0) & (df_surv$start_year > 1994),]$z_gx356re)

#' Read the table with preprocessed WLS dataset for survival analysis
#' 
#' @param filename A name of table.
#' @param include_pgs_datasets Should merge with PGS datasets?.
#' @returns A table.
#' @examples
#' read_wls_dataset("data/wls_bl_14.01.sas/wls_bl_14_01.sas7bdat", upper_threshold=1e-90)
read_wls_dataset <- function(filename="wls_bl_14.01.sas/wls_bl_14_01.sas7bdat",
                             include_pgs_datasets=TRUE)
{
  setwd("E:/education/thesis")
  
  df = read_sas("data/wls_bl_14.01.sas/wls_bl_14_01.sas7bdat")
  
  #merge pgs with epidemiological data
  df_surv = df
  if (include_pgs_datasets)
  {
    #read turley pgs
    turley_df = read.csv(file="data/polygenic scores/Turley/Turley_idpub_shuffled.txt", sep="\t")
    #read Lee pgs
    lee_df = read.csv(file="data/polygenic scores/Lee/Lee_idpub_shuffled.txt", sep="\t")
    
    df_surv = merge(x=df_surv, y=turley_df, by=c("idpub","rtype"), all.x=TRUE)
    df_surv = merge(x=df_surv, y=lee_df, by=c("idpub","rtype"), all.x=TRUE)
  }
  
  #19046
  #table(df_surv$z_livgrad)
  #12719 - alive
  #6327 - dead
  
  #remove who has not birth date
  df_surv = df_surv[df_surv$z_brdxdy > -3,]
  #18432 left
  #table(df_surv$z_livgrad)
  #12351 - alive
  #6081 - dead
  
  #outliers in z_deatyr, who has dead, but there no exact year
  #9971	DK, last contact in 1975 graduate interview	3	4	7
  #9973	DK, last contact in 1992-93 graduate interview	2	2	4
  #9975	DK, last contact in 2003-05 graduate interview	0	1	1
  #9993	DK, before 1964	13	5	18
  #9994	DK, after 1964, before 1975	10	10	20
  #9995	DK, before 1975 interview	2	6	8
  #9996	DK, before 1992-93 graduate interview	0	3	3
  #84 outliers (9 graduates)
  #nrow(df_surv[(df_surv$z_deatyr > 2023) & (df_surv$rtype=="g"),])
  df_surv = df_surv[df_surv$z_deatyr <= 2023,]
  #table(df_surv$z_livgrad)
  #18402 left
  #6051 dead
  #12351 alive
  
  #-----------------------end year--------------------------------
  df_surv$end_year = -1
  df_surv[df_surv$z_deatyr > -2,]$end_year = df_surv[df_surv$z_deatyr > -2,]$z_deatyr
  
  predicat_end = (df_surv$z_deatyr <= -2) & (df_surv$z_age75 > -2)
  df_surv[predicat_end,]$end_year = 
    (1900 + df_surv[predicat_end,]$z_brdxdy + df_surv[predicat_end,]$z_age75)
  
  predicat_end = (df_surv$z_deatyr <= -2) & !is.na(df_surv$z_ra016rey) & (df_surv$z_ra016rey > -2)
  df_surv[predicat_end,]$end_year = 
    (1900 + df_surv[predicat_end,]$z_ra016rey)
  
  predicat_end = (df_surv$z_deatyr <= -2) & !is.na(df_surv$z_ga003re)
  df_surv[predicat_end,]$end_year = 
    (1900 + df_surv[predicat_end,]$z_brdxdy + df_surv[predicat_end,]$z_ga003re)
  
  predicat_end = (df_surv$z_deatyr <= -2) & !is.na(df_surv$z_ha003re)
  df_surv[predicat_end,]$end_year = 
    (1900 + df_surv[predicat_end,]$z_brdxdy + df_surv[predicat_end,]$z_ha003re)
  
  predicat_end = (df_surv$z_deatyr <= -2) & !is.na(df_surv$z_q1a003re)
  df_surv[predicat_end,]$end_year = 
    (1900 + df_surv[predicat_end,]$z_brdxdy + df_surv[predicat_end,]$z_q1a003re)
  
  #all who has not end_year is not dead and does not have any variables at whole range (censored?)
  #will be removed
  df_surv <- df_surv[df_surv$end_year != -1,]
  #16322 left
  #table(df$z_livgrad)
  #10271 alive
  #6052 dead
  
  #-----------------------start year--------------------------------
  df_surv$start_year = -1
  #R7
  predicat_start = !is.na(df_surv$z_q1a003re)
  df_surv[predicat_start,]$start_year =
    (1900 + df_surv[predicat_start,]$z_brdxdy + df_surv[predicat_start,]$z_q1a003re)
  #R6
  predicat_start = !is.na(df_surv$z_ha003re)
  df_surv[predicat_start,]$start_year = 
    (1900 + df_surv[predicat_start,]$z_brdxdy + df_surv[predicat_start,]$z_ha003re)
  #R5
  predicat_start = !is.na(df_surv$z_ga003re)
  df_surv[predicat_start,]$start_year = 
    (1900 + df_surv[predicat_start,]$z_brdxdy + df_surv[predicat_start,]$z_ga003re)
  #R4
  predicat_start = !is.na(df_surv$z_ra016rey) & (df_surv$z_ra016rey > -2)
  df_surv[predicat_start,]$start_year = 
    (1900 + df_surv[predicat_start,]$z_ra016rey)
  #R3
  predicat_start = (df_surv$z_age75 > -2)
  df_surv[predicat_start,]$start_year = 
    (1900 + df_surv[predicat_start,]$z_brdxdy + df_surv[predicat_start,]$z_age75)
  
  df_surv[df_surv$rtype == "g",]$start_year = 1957
  
  #table(df_surv$start_year)
  
  #all who has not start_year are siblings and are dead
  #the start year can be chosen as 1977
  #however there are no useful information for such candidate thus it can be removed
  #because it is useless data
  
  df_surv = df_surv[df_surv$start_year != -1,]
  #15523 left
  #table(df_surv$z_livgrad)
  #10271 alive
  #5252 dead
  
  #time in survey 
  df_surv$time = df_surv$end_year - df_surv$start_year
  
  #age
  df_surv$age = df_surv$end_year - (1900 + df_surv$z_brdxdy)
  
  #get death_status from z_livgrad
  df_surv$death_status = df_surv$z_livgrad - 1
  
  #remove who had died the same year (457 individuals) 
  df_surv = df_surv[df_surv$time != 0,]
  
  #15066 left
  #table(df_surv$z_livgrad)
  #9825 alive
  #5241 dead
  
  #remove additional columns
  df_surv = df_surv %>% dplyr::select(-any_of(c("start_year", "end_year", "z_livgrad")))
  
  return (df_surv)
}

#' Read the table with information about variables effect.
#' 
#' @param filename A name of table.
#' @param upper_threshold A threshold to remove highly correlated variables.
#' @param remove_highly_correlated Should highly correlated variables be removed?.
#' @returns A table.
#' @examples
#' read_features_effect("cox_impact.csv", upper_threshold=1e-90)
read_features_effect <- function(filename,
                                 upper_threshold=1e-90,
                                 remove_highly_correlated=TRUE)
{
  path = paste("results/features impact/", filename, sep="")
  features_impact <- read.csv(path)
  
  #remove highly correlated variables, that can predict death case
  #with approximate 100% accuracy
  features_impact <- features_impact[features_impact$p_value > 0,]
  
  if (remove_highly_correlated)
    features_impact <- features_impact[features_impact$p_value > upper_threshold,]
  
  features_impact <- features_impact[!is.na(features_impact$feature_name),]
  return (features_impact[order(features_impact$p_value, decreasing=FALSE),])
}

#remove all factor levels that do not have death occurred 
preprocess_dataset <- function(dataset)
{
  columns_to_choose = colnames(dataset)
  dataset$row_id = row.names(dataset) 
  #print(paste("Length: ", length(dataset)))
  
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                        total = length(columns_to_choose),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 100)      # Width of the progress bar
  
  row_names_to_drop = c()
  col_names_to_droplevel = c()
  
  for (col in columns_to_choose)
  {
    pb$tick()
    
    if (col %in% c("death_status", "surv_years") || !is.factor(dataset[,col]))
      next
    
    lvls = levels(dataset[,col])
    is_need_to_droplevels = FALSE
    
    for (l in lvls)
    {
      num_of_lvls_death = length(which(dataset[which(dataset[,col] == l),]$death_status == TRUE))
      num_of_lvls_survival = length(which(dataset[which(dataset[,col] == l),]$death_status == FALSE))
      if (num_of_lvls_death > 1 && num_of_lvls_survival > 1)
        next
      
      row_names = row.names(dataset[which(dataset[,col] == l),])
      row_names_to_drop = c(row_names_to_drop, row_names)
      is_need_to_droplevels = TRUE
    }
    
    if (is_need_to_droplevels)
      col_names_to_droplevel = c(col_names_to_droplevel, col)
    
  }
  #print(col_names_to_droplevel)
  dataset <- dataset[!dataset$row_id %in% row_names_to_drop, ]
  dataset[,col_names_to_droplevel] <- droplevels(dataset[,col_names_to_droplevel])
  
  dataset <- dataset[,!names(dataset) %in% c("row_id")]
  
  return(dataset)
}

transform_columns <- function(dataset, df_features_effect)
{
  for (col in colnames(dataset))
  {
    if (col == "death_status" | col == "surv_years")
      next
    
    row = df_features_effect[df_features_effect$feature_name == col,]
    row = row[1,]
    if (nrow(row) == 1 && row$is_cat)
    {
      dataset[,row$feature_name] <- as.factor(dataset[,row$feature_name])
    }
  }
  return (dataset)
}