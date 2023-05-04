library(svMisc)
library(progress)
library(pracma)

get_all_by_level <- function(dataset, col, lvl)
{
  tmp = dataset[!is.na(dataset[,col]),]
  return (tmp[tmp[,col] == lvl,])
}

remove_rows_by_level <- function(dataset, col, lvl)
{
  dataset$row_id = row.names(dataset)
  tmp = dataset[!is.na(dataset[,col]),]
  row_names = row.names(tmp[tmp[,col] == lvl,])
  dataset <- dataset[!dataset$row_id %in% row_names, ]
  dataset[,col] <- droplevels(dataset[,col])
  return (dataset)
}

#validate that train/test dataset separate
#that death are preserved
validate_separation <- function(train, test)
{
  cols = colnames(train)
  train$row_id = row.names(train)
  test$row_id = row.names(test)
  
  row_names_train_to_drop = c()
  row_names_test_to_drop = c()
  col_names_to_droplevel = c()
  
  for (col in cols)
  {
    if (col %in% c("death_status", "surv_years") || !is.factor(train[,col]))
      next
    
    is_need_to_droplevels = FALSE
    lvls = levels(train[,col])
    
    #preprocess factor var
    for (l in lvls)
    {
      #if death occurences is more than 1
      num_of_lvls_death = length(which(train[which(train[,col] == l),]$death_status == TRUE))
      if (num_of_lvls_death > 1)
        next
      
      row_names = row.names(train[which(train[,col] == l),])
      row_names_train_to_drop = c(row_names_train_to_drop, row_names)
      
      row_names = row.names(test[which(test[,col] == l),])
      row_names_test_to_drop = c(row_names_test_to_drop, row_names)
      
      is_need_to_droplevels = TRUE
    }
    
    if (is_need_to_droplevels)
      col_names_to_droplevel = append(col_names_to_droplevel, col)
  }
  
  train <- train[!train$row_id %in% row_names_train_to_drop, ]
  train[,col_names_to_droplevel] <- droplevels(train[,col_names_to_droplevel])
  
  test <- test[!test$row_id %in% row_names_test_to_drop, ]
  test[,col_names_to_droplevel] <- droplevels(test[,col_names_to_droplevel])
  
  return (list("train" = train, "test" = test))
}

#separate randomly
separate_dataset <- function(dataset,
                             frac=0.70,
                             validate_separation_method=NA)
{
  #create ID column
  dataset$id = 1:nrow(dataset)
  
  train = dataset %>% dplyr::sample_frac(frac)
  test  = dplyr::anti_join(dataset, train, by = 'id')
  
  if (!is.na(validate_separation_method))
  {
    validate_sep = validate_separation(train, test)
    train = validate_sep$train
    test = validate_sep$test
  }
  
  train = (train %>% select(!(c("id"))))
  test = (test %>% select(!(c("id"))))
  
  separated_datasets = list("train" = train, "test" = test)
  
  return (separated_datasets)
}

get_param_str <- function(cols)
{
  param_str = ""
  cols <- cols[!cols %in% c("death_status", "surv_years")]
  
  for (i in 1:(length(cols) -1))
  {
    col = cols[i]
    param_str = paste(param_str, col, sep="")
    param_str = paste(param_str, " + ", sep="")
  }
  param_str = paste(param_str, tail(cols, n=1), sep="")
  return (param_str)
}

get_roc_curve_data <- function(dataset)
{
  param_str = get_param_str(colnames(dataset))
  
  min_auc = 1
  max_auc = 0
  
  min_fpr = list()
  min_tpr = list()
  
  max_fpr = list()
  max_tpr = list()
  
  for (i in c(1:10))
  {
    sep_data = separate_dataset(dataset)
    train <- sep_data$train
    test <- sep_data$test
    
    #train the model
    cox.mod = coxph(formula = as.formula(paste("Surv(surv_years, death_status) ~ ",
                                               param_str)),
                    data = train)
    
    p_vals <- seq(0.00, 1.0, by=0.005)
    
    tpr <- list()
    fpr <- list()
    
    for (p in p_vals)
    {
      true_vals <- test$death_status +0 
      
      probs <- exp(-predict(cox.mod, test, type="expected"))
      pred <- (probs < p) +0
      
      #true_vals <- true_vals[!is.na(pred)]
      #pred <- pred[!is.na(pred)]
      
      true_predicted <- pred[(pred == true_vals)]
      false_predicted <- pred[(pred != true_vals)]
      
      tp <- sum(true_predicted == 1, na.rm = TRUE)
      fp <- sum(false_predicted == 1, na.rm = TRUE)
      
      tn <- sum(true_predicted == 0, na.rm = TRUE)
      fn <- sum(false_predicted == 0, na.rm = TRUE)
      
      tpr <- append(tpr, tp / (tp + fn))
      fpr <- append(fpr, fp / (fp + tn))
    }
    
    curr_auc = trapz(unlist(fpr), unlist(tpr))#auc(true_vals, pred)
    if (curr_auc < min_auc)
    {
      min_auc = curr_auc
      min_fpr = fpr
      min_tpr = tpr
    }
    
    if (curr_auc > max_auc)
    {
      max_auc = curr_auc
      max_fpr = fpr
      max_tpr = tpr
    }
  }
  
  average_fpr <- list()
  average_tpr <- list()
  
  max_fpr <- unlist(max_fpr)
  min_fpr <- unlist(min_fpr)
  max_tpr <- unlist(max_tpr)
  min_tpr <- unlist(min_tpr)
  
  max_fpr[1] + min_fpr[1]
  
  for (i in 1:length(max_fpr))
  {
    average_fpr <- append(average_fpr, (max_fpr[i] + min_fpr[i])/2.0)
    average_tpr <- append(average_tpr, (max_tpr[i] + min_tpr[i])/2.0)
  }
  
  data <- data.frame(
    x = unlist(average_fpr),
    y = unlist(average_tpr),
    lt = "roc"
  )
  data <- rbind(data, data.frame(
    x = unlist(max_fpr),
    y = unlist(max_tpr),
    lt = "upper"
  ))
  data <- rbind(data, data.frame(
    x = unlist(min_fpr),
    y = unlist(min_tpr),
    lt = "lower"
  ))
  
  
  return (list("data"=data, "auc"=((max_auc + min_auc) / 2.0)))
}


train_rf <- function(train, rf_params=NULL, omit_statistics=TRUE)
{
  x_train = train[,!names(train) %in% c("death_status")]
  y_train = train[,"death_status"]
  
  if (is.null(rf_params))
    return (
      randomForest(x = x_train,
                   y = y_train,
                         
                   importance = !omit_statistics,
                   proximity = !omit_statistics,
                   na.action=na.omit)
      )
  else
    return (
      randomForest(x = x_train,
                   y = y_train,
                   ntree = rf_params["ntree"],
                   mtry = rf_params["mtry"],
                   replace = rf_params["replace"],
                   sampsize = rf_params["sampsize"],
                   maxnodes = rf_params["maxnodes"],
                   
                   importance = !omit_statistics,
                   proximity = !omit_statistics,
                   na.action=na.omit)
    )
}

preproc_test_rf <- function(test)
{
  predicat_not_dead = ((test$surv_years > age_to_predict) & (test$death_status==1))
  test[predicat_not_dead,]$death_status <- 0
  return (test)
}

#predict function for random forest on hazards
predict_rf <- function(x_test, rf_mod, p, y_age)
{
  x_test[,"surv_years"] = y_age
  probs = predict(rf_mod, x_test)
  pred = (probs > p) + 0
  return (pred)
}

train_ds <- function(train, ds_params=NULL, omit_statistics=TRUE)
{
  if (is.null(ds_params))
  {
    return (
      deepsurv(formula = as.formula(Surv(surv_years, death_status) ~ .),
               data = train,
               frac = 0.3,
               activation = "relu",
               num_nodes = c(4L, 8L, 4L, 2L),
               dropout = 0.1,
               early_stopping = TRUE,
               epochs = 100L,
               batch_size = 32L)
    )
  } else {
    return (
      deepsurv(formula = as.formula(Surv(surv_years, death_status) ~ .),
               data = train,
               frac = ds_params["frac"],
               activation = ds_params["activation"],
               num_nodes = ds_params["num_nodes"],
               dropout = ds_params["dropout"],
               early_stopping = TRUE,
               epochs = ds_params["epochs"],
               batch_size = ds_params["batch_size"])
    )
  }
  
}

predict_ds <- function(x_test, ds.mod, p, y_age)
{
  probs = predict(ds.mod, x_test, type="survival")
  ind = match(y_age, colnames(probs))
  probs = unname(probs[,ind])
  pred = (probs < p) + 0
  return (pred)
}

#' Get all possible combination of parameters
#' 
#' @param params A parameters for a model
#' @param i A index from which to start collecting a combination
#' @returns All possible combination of parameters.
#' @examples
#' all_params_combn(params, 1)
all_params_combn <- function(params, i, param_combs=list(), prev_comb=list())
{
  n = length(params[[i]])
  values = params[[i]]
  
  for (j in 1:n)
  {
    param_value = values[j]
    curr_comb = append(prev_comb, param_value)
    
    if (i < length(params))
      param_combs = all_params_combn(params, i + 1, param_combs, curr_comb)
    else
      param_combs = append(param_combs, list(curr_comb))
  }
  
  i < length(params)
  
  return (param_combs)
}

calculate_roc_scores <- function(x_test, y_test, 
                                 svmod, svmod.predict,
                                 y_age=NULL)
{
  p_vals <- seq(0.00, 1.0, by=0.005)
  
  tpr = c()
  fpr = c()
  
  for (p in p_vals)
  {
    pred = svmod.predict(x_test, svmod, p, y_age)
    true_predicted = pred[(pred == y_test)]
    false_predicted = pred[(pred != y_test)]
    
    tp = sum(true_predicted == 1, na.rm = TRUE)
    fp = sum(false_predicted == 1, na.rm = TRUE)
    tn = sum(true_predicted == 0, na.rm = TRUE)
    fn = sum(false_predicted == 0, na.rm = TRUE)
    
    tpr = c(tpr, tp / (tp + fn))
    fpr = c(fpr, fp / (fp + tn))
  }
  
  auc = abs(trapz(fpr, tpr))
  
  return (
    list(
    "tpr"=tpr,
    "fpr"=fpr,
    "auc"=auc
    )
  )
}

#' Get the best hyperparameters for a model
#' 
#' @param dataset A preprocessed dataset, ready to train a model
#' @param svmod.fit A function to train a model, should a model itself
#' @param svmod.predict A function to make a prediction with a model
#' @param params A parameters from which all possible combination will be calculated
#' @returns A hyperparameters list
#' @examples
#' hyperparams_tuning(dataset, train_rf, predict_rf, params, 5)
hyperparams_tuning <- function(dataset,
                               svmod.fit,
                               svmod.predict,
                               test_drop_cols,
                               svmod.test.preproc=NULL,
                               params,
                               k_folds=10, is_random=FALSE)
{
  param_combs = all_params_combn(params, 1)
  
  #k-fold cv 
  #??first approach (stratification supported). ???
  #require(caret)
  #flds <- createFolds(y, k = n_folds, list = TRUE, returnTrain = FALSE)
  #names(flds)[1] <- "train"
  
  
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                        total = length(param_combs),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 100)      # Width of the progress bar
  
  #Randomly shuffle the data
  dataset_shuffled = dataset[sample(nrow(dataset)),]
  
  #Create k equally size folds
  folds <- cut(seq(1,nrow(dataset_shuffled)), breaks=k_folds, labels=FALSE)
  
  best_auc = 0
  best_params = list()
  
  for (i in 1:length(param_combs))
  {
    pb$tick()
    
    #Perform k-fold cross validation
    auc_vector = c()
    for(j in 1:k_folds)
    {
      testIndexes <- which(folds==j,arr.ind=TRUE)
      test_dataset <- dataset_shuffled[testIndexes, ]
      train_dataset <- dataset_shuffled[-testIndexes, ]
      
      mod = svmod.fit(train_dataset)
      
      x_test = test_dataset %>% dplyr::select(-any_of(test_drop_cols))
      y_test = test_dataset[,"death_status"]
      #??c-index (concordance) 
      auc = calculate_roc_scores(x_test, y_test, mod, svmod.predict, y_age)$auc
      auc_vector = c(auc_vector, auc)
    }
    
    average_auc = mean(auc_vector)
    #
    if (average_auc > best_auc)
    {
      best_auc = average_auc
      best_params = param_combs[[i]]
    }
  }
  
  return (best_params)
}

get_best_model_data <- function(dataset,
                                svmod.fit,
                                svmod.predict,
                                test_drop_cols,
                                svmod.test.preproc=NULL,
                                steps_num=10,
                                y_age=NULL)
{
  min_auc = 1
  max_auc = 0
  min_fpr = c()
  min_tpr = c()
  max_fpr = c()
  max_tpr = c()
  
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                        total = steps_num,
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 100)      # Width of the progress bar
  
  df_train_vector = c()
  auc_vector = c()
  min_mod = NULL
  min_test = NULL
  
  for (i in c(1:steps_num))
  {
    pb$tick()
    
    sd = separate_dataset(dataset)
    train = sd$train
    test = sd$test
    
    df_train_vector = c(df_train_vector, list(train))
    
    #trained model
    mod = svmod.fit(train)
    
    if (!is.null(svmod.test.preproc))
      test = svmod.test.preproc(test)
    x_test = test %>% dplyr::select(-any_of(test_drop_cols))
    y_test = test[,"death_status"]
    
    roc_scores = calculate_roc_scores(x_test, y_test,
                                      mod, svmod.predict,
                                      y_age)
    
    curr_auc = roc_scores$auc
    auc_vector = c(auc_vector, curr_auc)
    
    if (curr_auc < min_auc)
    {
      min_auc = curr_auc
      min_fpr = roc_scores$fpr
      min_tpr = roc_scores$tpr
    }
    
    if (curr_auc > max_auc)
    {
      max_auc = curr_auc
      max_fpr = roc_scores$fpr
      max_tpr = roc_scores$tpr
    }
  }
  
  average_auc = (max_auc + min_auc) / 2.0
  
  min_dist = .Machine$double.xmax
  min_index = 0
  for (i in 1:length(auc_vector))
  {
    dist = abs(average_auc - auc_vector[i])
    if (min_dist > dist)
    {
      min_dist = dist
      min_index = i
    }
  }
    
  average_mod = svmod.fit(df_train_vector[[min_index]],
                          omit_statistics=FALSE)
  
  average_fpr = c()
  average_tpr = c()
  
  for (i in 1:length(max_fpr))
  {
    average_fpr = c(average_fpr, (max_fpr[i] + min_fpr[i])/2.0)
    average_tpr = c(average_tpr, (max_tpr[i] + min_tpr[i])/2.0)
  }
  
  roc_data <- data.frame(
    x = average_fpr,
    y = average_tpr,
    lt = "roc"
  )
  roc_data <- rbind(roc_data, data.frame(
    x = c(max_fpr),
    y = c(max_tpr),
    lt = "upper"
  ))
  roc_data <- rbind(roc_data, data.frame(
    x = c(min_fpr),
    y = c(min_tpr),
    lt = "lower"
  ))
  
  return (list("ROC"=roc_data,
               "mod"=average_mod,
               "auc"=average_auc))
}