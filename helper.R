

xgb_cv_grid <- function(data, 
                        list_of_params, 
                        objective = "reg:tweedie",
                        seed, 
                        n_rounds = 50, # к-сть ітерацій
                        n_fold = 4,    # к-сть фолдів для крос-валідації
                        metric_name = c("rmse") ) {
  
  
  xgb_model_cv <- xgboost::xgb.cv(
    params = list_of_params,
    objective = objective,
    data = data,
    nrounds = n_rounds,
    nfold = n_fold,
    metrics = metric_name,
    stratified = TRUE,
    verbose = FALSE,
    print_every_n = 20
    
  )
  
  return(xgb_model_cv)
  
}

find_optimal_params <- function(dtrain, objective = "reg:tweedie") {
  xgb_expand_grid <- expand.grid(
    eta = c(0.1, 0.3, 0.5, 0.7),
    max_depth = c(3, 5, 7),
    gamma = c(.1, .3, .5),
    colsample_bytree = c(1),
    iter =  c(1, 2, 4),
    min_child_weight = 1,
    subsample = c(0.75, 1)
  ) %>%
    dplyr::as_data_frame() %>%
    dplyr::mutate(model_id = 1:nrow(.)) %>%
    dplyr::mutate(seed_number = sample.int(n = 10**5, size = nrow(.), replace = F))


  xgb_cv_grid <- function(data,
                            list_of_params,
                            seed,
                            n_rounds = 50,
                            n_fold = 4,
                            metric_name = c("rmse")) {
    xgb_model_cv <- xgboost::xgb.cv(
      params = list_of_params,
      objective = objective,
      data = data,
      nrounds = n_rounds,
      nfold = n_fold,
      metrics = metric_name,
      stratified = TRUE,
      verbose = FALSE,
      print_every_n = 20
    )

    return(xgb_model_cv)
  }

  xgb_expand_grid_result <- xgb_expand_grid %>%
    dplyr::group_by(model_id, seed_number) %>%
    tidyr::nest(.key = params_df) %>%
    dplyr::mutate(params_list = purrr::map(params_df, purrr::flatten)) %>%
    dplyr::mutate(
      xgb_cv_model = purrr::map2(
        params_list,
        seed_number,
        ~ xgb_cv_grid(
          data = dtrain,
          list_of_params = ..1,
          seed = ..2
        )
      )
    )

  xgb_expand_grid_result_best <- xgb_expand_grid_result %>%
    dplyr::mutate(
      xgb_cv_model_best_result = purrr::map(
        .x = xgb_cv_model,
        .f = ~ {
          .x$evaluation_log %>%
            dplyr::filter_at(
              .vars = vars("test_rmse_mean"),
              .vars_predicate = any_vars(. == max(.))
            ) %>%
            dplyr::select_at(.vars = vars("iter", "train_rmse_mean", "test_rmse_mean"))
        }
      )
    ) %>%
    dplyr::select(model_id, seed_number, params_df, xgb_cv_model_best_result) %>%
    tidyr::unnest()

  best <- xgb_expand_grid_result_best %>%
    top_n(1, -test_rmse_mean)
  return(best)
}


# res <- readr::read_csv("data/motorbike_ambulance_calls.csv") %>% 
#   mutate(date = lubridate::mdy(date),
#          dttm = glue::glue('{date} {hr}') %>% lubridate::ymd_h(.)) %>% 
#   mutate_at(vars(season,holiday,weekday,weathersit), as.factor) %>% 
#   mutate_at(vars(yr,mnth,hr),as.factor) %>% 
#   select(-c(index,date,dttm))
# 
# 
# best_vals_workdays <-  res %>% 
#   group_by(workingday) %>% 
#   nest %>% 
#   mutate( split = map(data, initial_split),
#           training_data = map(split, training),
#           testing_data  = map(split, testing),
#           rec = map(training_data,
#                     ~(recipe(cnt~.,data = ..1) %>% 
#                         step_dummy(all_predictors(), -all_numeric()) %>% 
#                         step_center(all_predictors())  %>%
#                         step_scale(all_predictors()) %>% 
#                         step_zv(all_predictors(), -all_outcomes())%>%
#                         prep(data = ..1))),
#           x_train = map2(training_data,rec, ~ (bake(..2, new_data = ..1))),
#           x_test = map2(testing_data,rec, ~ (bake(..2, new_data = ..1))),
#           
#           to_predict = map(x_train, ~( xgboost::xgb.DMatrix(data=..1 %>% select(-cnt) %>%  as.matrix(), label = ..1$cnt  )  )),
#           to_valid = map(x_test, ~( xgboost::xgb.DMatrix(data=..1 %>% select(-cnt) %>%  as.matrix(), label = ..1$cnt  )  )),
#           best_mod = map(to_predict,~(find_optimal_params(..1)))
#   )
# 
# saveRDS(best_vals_workdays, "data/best_vals_workdays_fin.rds")
# 
# best_vals_workdays %>% 
#   select(workingday,best_mod) %>% unnest()
