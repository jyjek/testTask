library(shiny)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(highcharter)

dat <- readr::read_csv("data/motorbike_ambulance_calls.csv") %>% 
  mutate(date = lubridate::mdy(date),
         dttm = glue::glue('{date} {hr}') %>% lubridate::ymd_h(.)) %>% 
  mutate_at(vars(season,holiday,weekday,weathersit,hr), as.factor)

ui = bs4DashPage(
    old_school = FALSE,
    sidebar_collapsed = T,
    controlbar_collapsed = T,
    title = "Shpiruk's App",
    navbar = bs4DashNavbar(),
    sidebar =bs4DashSidebar(
     skin = "dark",
      status = "danger",
      title = "Ambulance Calls",
      brandColor = "dark",
      url = "https://github.com/jyjek",
      #src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      elevation = 3,
      opacity = 0.8,
     # bs4SidebarUserPanel(
     #   img = "https://image.flaticon.com/icons/svg/1149/1149168.svg", 
     #   text = "Ambulance Calls"
     # ),
      bs4SidebarMenu(
       # bs4SidebarHeader("Header 1"),
        bs4SidebarMenuItem(
          "Motorbike issues",
          tabName = "viz",
          icon = "ambulance"
        ),
        bs4SidebarMenuItem(
          "Prediction by town",
          tabName = "cities",
          icon = "city"
        ),
        bs4SidebarMenuItem(
          "About",
          tabName = "about",
          icon = "exclamation-circle"
        ))
    ),
    controlbar = bs4DashControlbar(),
    bs4DashFooter(
      copyrights = a(
        href = "https://rinterface.github.io/bs4Dash",
        target = "_blank", "Powered with bs4Dash from DivadNojnarg"
      ),
      right_text = a(
          href = "https://www.linkedin.com/in/dmytro-shpiruk/", 
          target = "_blank", "Shpiruk Dmytro, 2019"
        )
    ),
    body = bs4DashBody(
      tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #f65261b3 !important;}')),
      bs4TabItems(
      bs4TabItem(
        tabName = "viz",
        bs4Card(
          closable = F,
          width = 12,
          status = "dark",
          title = "History overview",
          bs4TabSetPanel( id = "viz_tabs", side = "left",
          bs4TabPanel(
            tabName = "Calendar",
            active = TRUE,
            echarts4rOutput("calendat"),br(),
            uiOutput("messsage"),
            span(h5(textOutput("date")),style = "color: green;text-align: left;"),
            highchartOutput("day")
          ),
          bs4TabPanel(
            tabName = "Heatmap",
            active = F,
            highchartOutput("heat")
          )
          )
        )
      ),
      bs4TabItem(
        tabName = "cities",
        bs4TabSetPanel( id = "predict", side = "left",
                        bs4TabPanel(
                          tabName = "Cities",
                          active = TRUE,
                          bs4Card(
                            closable = F,
                            width = 12,
                            status = "dark",
                            title = "Choose city",
                            textInput("city","Оберіть місто (Kyiv, Paris, etc.)"),
                            uiOutput("start"),br(),
                            DT::dataTableOutput("city_table"))
                        ),
                        bs4TabPanel(
                          tabName = "Map",
                          active = F,
                          bs4Card(
                            closable = F,
                            width = 12,
                            status = "dark",
                            title = "Map",
                          fluidRow(
                          column(10,
                          leafletOutput("map",height = 500)),
                          column(2,bs4InfoBoxOutput("cur_temp",width = 12),
                                   bs4InfoBoxOutput("weather",width = 12),
                                   bs4InfoBoxOutput("pred",width = 12)))),
                          uiOutput("bttns")
                        )),
        uiOutput("bs")
      ),
      bs4TabItem(
      
        tabName = "about",
        bs4Card(
          closable = F,
          width = 12,
          status = "dark",
          title = "FAQ",
        span(h4('Motorbike Ambulance Calls ShinyApp',style = "text-align: center"),hr()),
        span(h5('Shiny App для тестового завдання',style = "text-align: center")),br(),
        p("У вкладці `Motorbike issues` знаходяться візуалізації для історичного датафрейму. 
           У якому можна переглянути частотні характеристики викликів."),
        p("Натиснувши на дату у календарі - відкриється погодинний розподіл викликів."),
        p("У вкладці `Predict by town` - побудова прогнозу викликів швидкої допомоги на нинішню дату, 
          відповідно до погодніх умов обраного міста. 
          Обрати місто вижливо двома способами:"),
        p("1. Ввівши паттерн назви у поле `Choose city` та обравши у таблиці потрібний варіант"),
        p("2. Натиснувши на потрібну точку на карті та обравши серед найближчих населених пунктів."),
        p("Прогноз погоди витягується за допомогою сервісу"),tags$a(href="https://openweathermap.org", "openweathermap.org")
   
      ))
      
    )
    )
  )
