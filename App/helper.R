model <- xgboost::xgb.load("data/xgboost")
rec_obj <- readRDS("data/recipe.rds")

readCity <- function(city){
  out <- tryCatch({
    get_forecast(city, units = "metric") %>%
      owmr_as_tibble()
  },
  error = function(cond){
    message("Wrong city name")
  }
  )
  return(out)
}

prepare_data <- function(df, hourly_coefs = readRDS("data/coefs.rds")){
  suppressMessages({ df %>% 
    mutate(date = as_date(dt_txt),
           hr = dt_txt %>% lubridate::hour(.) %>% as.factor) %>%
    tidyr::complete(date,hr=as.factor(c(0:23))) %>% 
    mutate(dt_txt = glue::glue('{date} {hr}') %>% ymd_h) %>% 
    filter(dt_txt> now()) %>% 
    fill(weather_id,weather_main,weather_description) %>% 
    left_join(hourly_coefs %>% rename_at(vars(temp:windspeed),~(paste0(.,"_cof"))),"hr") %>% 
    mutate(temp = ifelse(is.na(temp), lag(temp)/lag(temp_cof)*temp_cof , temp)) %>% 
    mutate(temp = ifelse(is.na(temp), lag(temp)/lag(temp_cof)*temp_cof , temp)) %>% 
    mutate(humidity = ifelse(is.na(humidity), lag(humidity)/lag(hum_cof)*hum_cof , humidity)) %>% 
    mutate(humidity = ifelse(is.na(humidity), lag(humidity)/lag(hum_cof)*hum_cof , humidity)) %>% 
    mutate(wind_speed = ifelse(is.na(wind_speed), lag(wind_speed)/lag(windspeed_cof)*windspeed_cof , wind_speed)) %>% 
    mutate(wind_speed = ifelse(is.na(wind_speed), lag(wind_speed)/lag(windspeed_cof)*windspeed_cof , wind_speed)) %>% 
    mutate(date = as_date(dt_txt),
           hr = dt_txt %>% lubridate::hour(.),
           weekday = dt_txt %>% lubridate::wday(.,week_start = getOption("lubridate.week.start", 1)),
           holiday = ifelse(weekday %in% c(6,7),1,0),
           workingday = ifelse(weekday %in% c(6,7),1,0),
           mnth = date %>% lubridate::month(.),
           season = case_when( mnth %in% c(12,1,2) ~ "winter",
                               mnth %in% c(3:5) ~ "spring",
                               mnth %in% c(6:8) ~ "summer",
                               mnth %in% c(9:11) ~ "autumn")) %>% 
    mutate(weathersit = case_when(
      weather_id %in% c(800:802) ~ 1,
      weather_id %in% c(803:804, 300:301,500:502, 700:711) ~ 2,
      weather_id %in% c(302:313, 200:210, 503:521, 600:616, 721:761) ~ 3,
      weather_id %in% c(522:531, 620:622, 762, 771, 781, 211:232, 314:321) ~ 4,
      T ~ 2
    )) %>% 
    drop_na(temp) %>% 
    mutate_at(vars(hr,weather_id:weather_description, weekday:weathersit),as.factor) %>% 
    mutate(temp = temp/41,
           atemp = temp * runif(1, min = 1, max = 1.1),
           hum = humidity/100,
           windspeed = wind_speed/67) %>% 
    mutate(workingday = as.numeric(workingday)) 
  })
}


prepare_forecast <- function(df){
  load_data <- df %>% prepare_data
  loc_fore <- load_data %>%
    select(season, mnth, hr, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed) %>% 
    bake(rec_obj, new_data = .) 
  
  load_data_res <- load_data %>% 
    add_column(cnt = xgboost::xgb.DMatrix( data=loc_fore %>% as.matrix()) %>% predict(model,.) %>% round(.,1)) %>% 
    select(dt_txt, temp,  atemp, hum, windspeed,weather_main,weather_description,  cnt) %>% 
    mutate(dt = datetime_to_timestamp(dt_txt),
           temp = temp*41,
           atemp = atemp*50,
           hum = hum*100,
           windspeed = windspeed*67) %>% 
    mutate_at(vars(temp:windspeed),~(round(.,1))) %>% 
    mutate(dt = datetime_to_timestamp(dt_txt)) %>% 
    mutate(time = stringr::str_sub(dt_txt,12,20))
  
  return(load_data_res)
}

#readCity("Kiev") %>% prepare_forecast
# 
# load_data_res %>% 
#   select(dt_txt, temp,  atemp, hum, windspeed,weather_main,weather_description,  cnt) %>% 
#   mutate(dt = datetime_to_timestamp(dt_txt),
#          temp = temp*41,
#          atemp = atemp*50,
#          hum = hum*100,
#          windspeed = windspeed*67) %>% 
#   mutate_at(vars(temp:windspeed),~(round(.,1)))
# 
# load_data <- readCity("London") %>% prepare_data
# loc_fore <- load_data %>%
#   select(season, mnth, hr, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed) %>% 
#   bake(rec_obj, new_data = .) 
# 
# load_data_res <- load_data %>% 
#   add_column(cnt = xgboost::xgb.DMatrix( data=loc_fore %>% as.matrix()) %>% predict(model,.) %>% round(.,1)) %>% 
#   select()
#   
# 
# 
# 
# 
# dtrain<-xgboost::xgb.DMatrix( data=x_tbl %>% as.matrix())
# 
# predict(model,dtrain)