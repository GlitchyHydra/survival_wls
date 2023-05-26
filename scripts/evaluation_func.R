library(svMisc)
library(progress)
library(pracma)


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
    if (col %in% c("death_status", "time") || !is.factor(train[,col]))
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
  cols <- cols[!cols %in% c("death_status", "time")]
  
  for (i in 1:(length(cols) -1))
  {
    col = cols[i]
    param_str = paste(param_str, col, sep="")
    param_str = paste(param_str, " + ", sep="")
  }
  param_str = paste(param_str, tail(cols, n=1), sep="")
  return (param_str)
}

train_coxph <- function(train)
{
  #train the model
  cox.mod = coxph(formula = as.formula(paste("Surv(time, death_status) ~ ",
                                             param_str)),
                  data = train)
  return (cox.mod)
}

pred_coxph <- function(test)
{
  probs <- exp(-predict(cox.mod, test, type="expected"))
  pred <- (probs < p) + 0
  return (pred)
}

train_rf <- function(train, rf_param=NULL, omit_statistics=TRUE)
{
  if (is.null(rf_param))
  {
    return (
      randomForest(death_status ~ .,
                   data=train,
                   
                   importance = !omit_statistics,
                   proximity = !omit_statistics)
    )
  }
  else {
    return (
      randomForest(death_status ~ .,
                   data=train,
                   
                   ntree = rf_param$ntree,
                   mtry = rf_param$mtry,
                   replace = rf_param$replace,
                   max.depth = rf_param$max.depth,
                   
                   importance = !omit_statistics,
                   proximity = !omit_statistics,
                   na.action=na.omit)
    )
  }
}

preproc_test_data <- function(test, time_to_predict)
{
  test = test[!((test$time < time_to_predict) & 
                          (test$death_status == 0)),]
  
  
  predicat_not_dead = ((test$time > time_to_predict) & (test$death_status==1))
  test[predicat_not_dead,]$death_status <- 0
  return (test)
}

#predict function for random forest on hazards
predict_rf <- function(x_test, rf_mod, p, time_to_predict)
{
  x_test[,"time"] = time_to_predict
  probs = predict(rf_mod, x_test)
  pred = (probs > p) + 0
  return (pred)
}

train_ds <- function(train, ds_params=NULL, omit_statistics=TRUE)
{
  if (is.null(ds_params))
  {
    return (
      deepsurv(formula = as.formula(Surv(time, death_status) ~ .),
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
      deepsurv(formula = as.formula(Surv(time, death_status) ~ .),
               data = train,
               frac = ds_params$frac,
               activation = ds_params$activation,
               optimizer = ds_params$optimizer,
               num_nodes = ds_params$num_nodes,
               batch_norm = ds_params$batch_norm,
               dropout = ds_params$dropout,
               batch_size = ds_params$batch_size,
               epochs = ds_params$epochs,
               shuffle = FALSE)
    )
  }
  
}

predict_ds <- function(x_test, ds.mod, p, time_to_predict)
{
  probs = predict(ds.mod, x_test, type="survival")
  ind = match(time_to_predict, colnames(probs))
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
                                 time_to_predict=NULL)
{
  p_vals <- c(seq(0.00, 1.0, by=0.005), 1.00001)
  
  tpr = c()
  fpr = c()
  
  for (p in p_vals)
  {
    pred = svmod.predict(x_test, svmod, p, time_to_predict)
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


calculate_cindex <- function(test, svmod)
{
  cind = concordance(Surv(time, death_status) ~ predict(svmod, test, type="risk"),
                     test)$concordance
  return (cind)
}

calculate_cindex_without_tied <- function(test, svmod)
{
  cind = concordance(Surv(time, death_status) ~ predict(svmod, test, type="risk"),
                     test)$concordance
  return (cind)
}

get_max_batch_size <- function(dataset, k_fold)
{
  current_batch_size = 16
  max_batch_num = 4
  train_set_size = floor(nrow(dataset) / k_fold)
  while(TRUE)
  {
    current_batch_size = current_batch_size * 2
    if (current_batch_size <= train_set_size)
      max_batch_num = max_batch_num + 1
    else
      break
  }
  return (max_batch_num)
}

get_random_param <- function(params)
{
  param = list()
  for (key in names(params))
  {
    if (key == "num_nodes")
    {
      num_layers = sample(params$num_nodes$num_layers, size=1)
      num_nodes = sample(params$num_nodes$num_units,
                         size = num_layers,
                         replace=TRUE)
      param$num_nodes = num_nodes
    }
    else
      param[key] = sample(params[[key]],
                          size=1,
                          replace=TRUE)
  }
  return (param)
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
                               metric,
                               test_drop_cols,
                               params,
                               svmod.test.preproc=NULL,
                               k_folds=5,
                               iter_num = 1000,
                               is_random=FALSE)
{
  #k-fold cv 
  #??first approach (stratification supported). ???
  #require(caret)
  #flds <- createFolds(y, k = n_folds, list = TRUE, returnTrain = FALSE)
  #names(flds)[1] <- "train"
  
  if (!is_random)
  {
    param_combs = all_params_combn(params, 1)
    iter_num = length(param_combs)
  }
  
  
  #pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
  #                      total = iter_num,
  #                      complete = "=",   # Completion bar character
  #                      incomplete = "-", # Incomplete bar character
  #                      current = ">",    # Current bar character
  #                      clear = FALSE,    # If TRUE, clears the bar when finish
  #                      width = 100)      # Width of the progress bar
  
  best_score = 0
  best_param = list()
  tuning_data = list("iteration" = c(), "score" = c(), "param" = c())
  
  i = 1
  while (TRUE)
  {
    #Randomly shuffle the data
    dataset_shuffled = dataset[sample(nrow(dataset)),]
    
    #Create k equally size folds
    folds <- cut(seq(1,nrow(dataset_shuffled)), breaks=k_folds, labels=FALSE)
    
    if (is_random)
    {
      param = get_random_param(params)
    }
    else
      param = param_combs[[i]]
    
    #Perform k-fold cross validation
    iter_score_vector = c()
    for(j in 1:k_folds)
    {
      testIndexes <- which(folds==j,arr.ind=TRUE)
      test_dataset <- dataset_shuffled[testIndexes, ]
      train_dataset <- dataset_shuffled[-testIndexes, ]
      
      tryCatch(
        {
          mod = svmod.fit(train_dataset, param)
          
          if (metric == "AUC")
            preproc_test_data(test_dataset, time_to_predict)
          
          x_test = test_dataset %>% dplyr::select(-any_of(test_drop_cols))
          y_event_test = test_dataset[,"death_status"]
          y_time_test = test_dataset[,"time"]
          
          if (metric == "AUC")
          {
            auc = calculate_roc_scores(x_test, y_event_test, mod, svmod.predict, time_to_predict)$auc
            iter_score_vector = c(iter_score_vector, auc)
          }
          else {
            cind = calculate_cindex_without_tied(test_dataset, mod)
            iter_score_vector = c(iter_score_vector, cind)
          }
        }, error = function(err.msg) {
          #print(err.msg)
        })
    }
    
    if (length(iter_score_vector) != k_folds)
      next
    
    average_score = mean(iter_score_vector)
    if (average_score > best_score)
    {
      best_score = average_score
      best_param = param
    }
    print(sprintf("Iter: %i, best score so far: %.4f", i, best_score))
    tuning_data = append(tuning_data, 
                         list("iteration" = i,
                              "score" = average_score,
                              "param" = param
                         ))
    if (i == iter_num)
      break
    i = i + 1
    #pb$tick()
  }
  
  return (list("best_param" = best_param,
               "best_score" = best_score,
               "tuning_data" = tuning_data))
}

get_best_model_data <- function(dataset,
                                svmod.fit,
                                svmod.predict,
                                test_drop_cols,
                                param=NULL,
                                steps_num=5,
                                inner_steps_num=10,
                                time_to_predict=NULL)
{
  #options(warn=2)
  sd = separate_dataset(dataset, frac=0.85)
  dataset = sd$train
  validation = sd$test
  
  validation = preproc_test_data(validation, time_to_predict)
  x_validation = validation %>% dplyr::select(-any_of(test_drop_cols))
  y_validation = validation[,"death_status"]
  
  min_auc = 1
  max_auc = 0
  min_fpr = c()
  min_tpr = c()
  max_fpr = c()
  max_tpr = c()
  df_train_vector = c()
  auc_vector = c()
  
  i = 1
  while(TRUE)
  {
    #pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    #                      total = steps_num,
    #                      complete = "=",   # Completion bar character
    #                      incomplete = "-", # Incomplete bar character
    #                      current = ">",    # Current bar character
    #                      clear = FALSE,    # If TRUE, clears the bar when finish
    #                      width = 100)      # Width of the progress bar
    
    current_max_auc = 0
    best_mod = NULL
    current_best_train = NULL
    
    j = 1
    while (TRUE)
    {
      #pb$tick()
      
      tryCatch(
        {
          
          sd = separate_dataset(dataset, frac=0.85)
          train = sd$train
          test = sd$test
          
          #trained model
          mod = svmod.fit(train)
          test = preproc_test_data(test, time_to_predict)
          x_test = test %>% dplyr::select(-any_of(test_drop_cols))
          y_test = test[,"death_status"]
          roc_scores = calculate_roc_scores(x_test, y_test,
                                            mod, svmod.predict,
                                            time_to_predict)
          curr_auc = roc_scores$auc
          if (curr_auc >  current_max_auc)
          {
            best_mod = mod
            current_best_train = train
            current_max_auc = curr_auc
          }
          
          if (j == steps_num)
            break
          
          j = j + 1
        }, error = function(err.msg) {
          #print(err.msg)
        })
    }
    
    df_train_vector = c(df_train_vector, list(current_best_train))
    
    roc_scores = calculate_roc_scores(x_validation, y_validation,
                                      best_mod, svmod.predict,
                                      time_to_predict)
    curr_auc = roc_scores$auc
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
    
    average_auc = (max_auc + min_auc) / 2.0
    auc_vector = c(auc_vector, average_auc)
    
    print(sprintf("Iter: %i, average score so far: %.4f", i, average_auc))
    
    if (i == inner_steps_num)
      break
    
    i = i + 1
  }
  
  min_dist = .Machine$double.xmax
  min_index = 0
  for (j in 1:length(auc_vector))
  {
    dist = abs(average_auc - auc_vector[j])
    if (min_dist > dist)
    {
      min_dist = dist
      min_index = j
    }
  }
  
  average_mod = svmod.fit(df_train_vector[[min_index]],
                          omit_statistics=FALSE)
  
  average_fpr = c()
  average_tpr = c()
  
  for (j in 1:length(max_fpr))
  {
    average_fpr = c(average_fpr, (max_fpr[j] + min_fpr[j])/2.0)
    average_tpr = c(average_tpr, (max_tpr[j] + min_tpr[j])/2.0)
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
  
  options(warn=0)
  return (list("ROC"=roc_data,
               "mod"=average_mod,
               "auc"=average_auc))
}
