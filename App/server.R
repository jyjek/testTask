library(owmr)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(leaflet)
library(xgboost)
library(recipes)
library(leaflet.extras)
library(lubridate)
library(highcharter)
library(viridis)

Sys.setenv(OWM_API_KEY = "0312b239642c3a198b387169703668a2")

source("helper.R", encoding = "UTF-8")

dat <- readr::read_csv("data/motorbike_ambulance_calls.csv") %>%
  dplyr::mutate(
    date = lubridate::mdy(date),
    dttm = glue::glue("{date} {hr}") %>% lubridate::ymd_h(.)
  ) %>%
  dplyr::mutate_at(vars(season, holiday, weekday, weathersit, hr), as.factor)

server <- function(input, output) {
  vals <- shiny::reactiveValues()

  output$start <- renderUI({
    req(input$city)
    shinyWidgets::actionBttn("go", "Start processing", color = "primary", style = "fill")
  })

  observeEvent(input$go, {
    vals$near_choosed <- NULL
    vals$cities_list <- owmr::search_city_list(input$city) %>% dplyr::filter(nm != "Kievka")
  })

  output$city_table <- DT::renderDataTable({
    DT::datatable(vals$cities_list,
      escape = F, rownames = F, class = "cell-border compact",
      options = list(ordering = T, autowidth = F, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = "_all"))),
      selection = list(mode = "single", target = "row"),
      colnames = c("City ID", "City", "lat", "lon", "Cuntry Code")
    )
  })

  observeEvent(input$city_table_rows_selected, {
    vals$city <- vals$cities_list$nm[input$city_table_rows_selected]
    vals$town <- vals$city
    vals$near_choosed <- readCity(vals$city) %>% prepare_forecast()
  })

  output$messsage <- renderPrint({
    validate(need(is.null(input$calendat_clicked_data$value), ''))
   # validate(need(is.null(vals$city),""))
    span(h5( "Оберіть дату в календарі",style = "text-align: left; color: red"),hr())
  })

  output$bs <- renderUI({
    validate(need(vals$near_choosed, ""))
    bs4Card(
      closable = F,
      width = 12,
      status = "dark",
      title = "Data",
      bs4TabSetPanel(
        id = "predict", side = "left",
        bs4TabPanel(
          tabName = "Plot",
          active = TRUE,
          highcharter::highchartOutput("call_predict")
        ),
        bs4TabPanel(
          tabName = "Table",
          active = F,
          DT::dataTableOutput("DT")
        )
      )
    )
  })

  output$DT <- DT::renderDataTable({
    validate(need(vals$near_choosed, ""))
    DT::datatable(vals$near_choosed,
      extensions = c("Buttons"), # server=FALSE,
      options = list(
        dom = "Blfrtip",
        buttons = list(list(extend = c("excel"), filename = glue::glue("data_geo_{lubridate::today()}"))),
        columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all"))
      ),
      colnames = c(
        "datetime", "Temperature", "Feeling Temperature", "Humidity", "Wind Speed", "Weather Main",
        "Weather Description", "Call's prediction", "Parsed Time", "Time"
      )
    )
  })

  output$calendat <- echarts4r::renderEcharts4r({
    dat %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(values = sum(cnt)) %>%
      dplyr::mutate(year = format(date, "%Y")) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(year) %>%
      echarts4r::e_charts(date) %>%
      echarts4r::e_calendar(range = "2011", top = "40") %>%
      echarts4r::e_calendar(range = "2012", top = "260") %>%
      echarts4r::e_heatmap(values, coord_system = "calendar", left = 100) %>%
      echarts4r::e_visual_map(max = 8500) %>%
      echarts4r::e_title("Calendar", "Heatmap") %>%
      echarts4r::e_tooltip("item")
  })

  output$heat <- highcharter::renderHighchart({
    dat %>%
      dplyr::mutate(wd = lubridate::wday(date, label = T, abbr = T)) %>%
      dplyr::group_by(wd, hr) %>%
      dplyr::summarise(value = mean(cnt)) %>%
      dplyr::ungroup() %>%
      hchart("heatmap", hcaes(x = hr, y = wd, value = value)) %>%
      hc_colorAxis(
        stops = color_stops(10, rev(inferno(10))),
        type = "logarithmic"
      ) %>%
      hc_yAxis(
        reversed = TRUE, offset = 0, tickLength = 0, title = list(text = "День тижня"),
        gridLineWidth = 0, minorGridLineWidth = 0,
        labels = list(style = list(fontSize = "8px"))
      ) %>%
      hc_tooltip(formatter = JS("function(){
  return   this.series.yAxis.categories[this.point.y] + ' ' + this.point.x + ' година' +':<br>' +
  Highcharts.numberFormat(this.point.value, 2) + ' викликів';}")) %>%
      hc_title(text = "Weekly Motobike Ambulance Calls") %>%
      hc_xAxis(title = list(text = "Година"))
  })

  output$date <- renderText({
    validate(need(input$calendat_clicked_data$value, ""))
    glue::glue("Обрано {input$calendat_clicked_data$value[1]}")
  })

  output$day <- highcharter::renderHighchart({
    validate(need(input$calendat_clicked_data$value, ''))

    loc_df <- dat %>%
      dplyr::filter(date == input$calendat_clicked_data$value[1]) %>%
      dplyr::mutate(
        dt = datetime_to_timestamp(dttm),
        temp = temp * 41,
        atemp = atemp * 50,
        hum = hum * 100,
        windspeed = windspeed * 67
      ) %>%
      dplyr::mutate_at(vars(temp:windspeed), ~ (round(., 1)))

    highchart() %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = "%e. %b %H:%M")) %>%
      hc_add_series(loc_df, "column", hcaes(dt, cnt), color = "green", name = "Ambulance Calls") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "<table>",
        pointFormat = paste(
          "<tr><th>Ambulance Calls</th><td>{point.cnt} </td></tr>",
          "<tr><th>Temperature</th><td>{point.temp} C</td></tr>",
          "<tr><th>Feeling Temperature</th><td>{point.atemp} C</td></tr>",
          "<tr><th>Humidity</th><td>{point.hum} %</td></tr>",
          "<tr><th>Wind speed</th><td>{point.windspeed} km/h</td></tr>"
        ),
        footerFormat = "</table>"
      )
  })

  output$map <- renderLeaflet({
    leaflet::leaflet("map") %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -0.118092, lat = 51.509865, zoom = 11)
  })

  observeEvent(input$map_click, {
    vals$near <- owmr::find_cities_by_geo_point(
      lat = input$map_click$lat,
      lon = input$map_click$lng,
      cnt = 5,
      units = "metric"
    ) %>% owmr::owmr_as_tibble()

    map <- leaflet::leafletProxy("map")
    map %>%
      clearPopups() %>%
      addPulseMarkers(
        lng = input$map_click$lng, lat = input$map_click$lat,
        label = "",
        icon = leaflet.extras::makePulseIcon(heartbeat = 1, color = "#0000ff")
      )
  })

  output$ct <- DT::renderDataTable({
    vals$near_choosed
  })

  output$bttns <- renderUI({
    req(vals$near)
    radioGroupButtons(
      inputId = "choose_city",
      label = "Оберіть місто",
      choices = vals$near$name %>% unique(),
      status = "danger",
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon"),
        no = icon("remove", lib = "glyphicon")
      )
    )
  })

  observeEvent(input$choose_city, {
    vals$town <- input$choose_city
    vals$near_choosed <- readCity(input$choose_city) %>% prepare_forecast()
  })

  output$call_predict <- renderHighchart({
    req(vals$near_choosed)
    highchart() %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = "%e. %b %H:%M")) %>%
      hc_add_series(vals$near_choosed, "column", hcaes(dt, cnt), color = "red", name = "Predict Ambulance Calls") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = glue::glue('Calls Prediction for {vals$town}')) %>% 
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "<table>",
        pointFormat = paste(
          "<tr><th>Predict Ambulance Calls</th><td>{point.cnt} </td></tr>",
          "<tr><th>Time</th><td>{point.time} </td></tr>",
          "<tr><th>Weather</th><td>{point.weather_main}- {point.weather_description} </td></tr>",
          "<tr><th>Temperature</th><td>{point.temp} C</td></tr>",
          "<tr><th>Feeling Temperature</th><td>{point.atemp} C</td></tr>",
          "<tr><th>Humidity</th><td>{point.hum} %</td></tr>",
          "<tr><th>Wind speed</th><td>{point.windspeed} km/h</td></tr>"
        ),
        footerFormat = "</table>"
      )
  })
  
 output$cur_temp <-  renderbs4InfoBox({
   validate(need(vals$near_choosed,''))
   bs4InfoBox(
     title = "Нинішня температура",width = 12,
     gradientColor = "success",#value=10,
     value = glue::glue('{vals$near_choosed %>% 
       dplyr::slice(1) %>% pull(temp)} C'),
     icon = "thermometer-empty",
     elevation = 5
   ) })
 
 output$weather <-  renderbs4InfoBox({
   validate(need(vals$near_choosed,''))
   bs4InfoBox(
     title = "Нинішня погода",width = 12,
     gradientColor = "success",#value=10,
     value = vals$near_choosed %>% 
       dplyr::slice(1) %>% pull(weather_main),
     icon = "sun",
     elevation = 5
   ) })
 
 output$pred <-  renderbs4InfoBox({
   validate(need(vals$near_choosed,''))
   bs4InfoBox(
     title = "Прогнозованi дзвінки",width = 12,
     gradientColor = "danger",#value=10,
     value = vals$near_choosed %>% 
       dplyr::slice(1) %>% pull(cnt),
     icon = "motorcycle",
     elevation = 5
   ) })

  # output$map_click <- renderPrint({input$choose_city})
  # output$val <- renderPrint({vals$near_choosed})
}