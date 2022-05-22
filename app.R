# AMNC homepage app
# Version 1.1.0
# This dashboard was built by Owen O'Connor and Tributary Data LLC 
# Questions? Email tributarydata@gmail.com

library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(openair)
library(plotly)
library(lattice)


#### nowcast functions ----------------------------------------------------------------------------

# hour_munge function


#takes the last 12 hours of data and creates 12 hourly concentration averages
hour_munge <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5,
  #third column PM10
) {
  
  most_recent_time <- max(df$time) # calculate most recent time in dataset
  twelve_hours_ago <- most_recent_time - hours(12) # calculate 12 hours before that
  
  df <- df %>%
    filter(
      time >= twelve_hours_ago
    ) %>%
    mutate(
      time_from_recent = floor(as.numeric(as.duration(most_recent_time-time), "hours"))
    )
  
  #Round values on the edge that are 12 hours down into the "11th hour"
  df$time_from_recent[df$time_from_recent == 12] <- 11
  
  hourly_avgs <- df %>%
    group_by(
      time_from_recent
    ) %>%
    summarise(
      PM2.5 = mean(PM2.5,na.rm = TRUE),
      PM10 = mean(PM10,na.rm = TRUE),
    )
  
  return(hourly_avgs)
}

# nowcast function

#' @export
nowcast <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5
) {
  
  hourly_avgs <- hour_munge(df) #takes the last 12 hours of data and creates 12 hourly concentration averages
  
  range <- max(hourly_avgs$PM2.5) - min(hourly_avgs$PM2.5)
  scaled_rate_of_change <- range / max(hourly_avgs$PM2.5)
  weight_factor <- 1 - scaled_rate_of_change
  if (weight_factor < .5)
    weight_factor <- .5
  
  
  hourly_avgs_weighted <- hourly_avgs %>%
    mutate(
      PM2.5 = PM2.5 * (weight_factor ^ time_from_recent),
      weights = (weight_factor ^ time_from_recent)
    )
  
  nowcast_num <- sum(hourly_avgs_weighted$PM2.5) / sum(hourly_avgs_weighted$weights)
  
  nowcast_num <-  trunc(nowcast_num*10^2)/10^2 # truncate to 2 decimal places
  
  
  return(nowcast_num)
  
}


#' @export
nowcast10 <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5
) {
  
  hourly_avgs <- hour_munge(df) #takes the last 12 hours of data and creates 12 hourly concentration averages
  
  range <- max(hourly_avgs$PM10) - min(hourly_avgs$PM10)
  scaled_rate_of_change <- range / max(hourly_avgs$PM10)
  weight_factor <- 1 - scaled_rate_of_change
  if (weight_factor < .5)
    weight_factor <- .5
  
  
  hourly_avgs_weighted <- hourly_avgs %>%
    mutate(
      PM10 = PM10 * (weight_factor ^ time_from_recent),
      weights = (weight_factor ^ time_from_recent)
    )
  
  nowcast_num10 <- sum(hourly_avgs_weighted$PM10) / sum(hourly_avgs_weighted$weights)
  
  nowcast_num10 <-  trunc(nowcast_num10*10^2)/10^2 # truncate to 2 decimal places
  
  
  return(nowcast_num10)
  
}


#### API Calls -------------------------------------------------------------------------

## QuantAQ API:

api_key = 'C2TSVOC7S40ONPN6ZTXMLP0L'
serial_number = 'MOD-PM-00044' # Specific to this monitoring device

base_url = "https://api.quant-aq.com/device-api/v1"
accounts_endpoint = '/account'
# data_endpoint = paste0('/',serial_number,'/data')
data_endpoint = '/data'
raw_data_endpoint = '/data/raw'


get_request = function(url, api_key, params = NULL){
  response = httr::GET(
    url = url,
    authenticate(
      user = api_key, password = api_key, type = 'basic'),
    query = params
  )
  return(response)
}


# data_points=5000
get_data = function(serial_number, data_points=NULL, start_date=NULL, end_date = NULL){
  # this will save all the data
  main_data = c()
  
  # adding serial number in endpoint
  data_endpoint_with_serial = paste0('/devices','/', serial_number, data_endpoint)
  
  # data endpoint for a specific defined serial number
  url = paste0(base_url, data_endpoint_with_serial)
  
  # adding date filter if its not null in parameters
  date_filter = NULL
  if (all(!is.null(start_date) || !is.null(end_date))) {
    date_filter = paste0("timestamp_local,ge,",start_date,";timestamp_local,le,",end_date)
  }
  # different parameters we are sending with request
  params = list(
    page=1,limit=data_points,sort="timestamp_local,desc",
    per_page=1000, filter=date_filter)
  
  # for multiple page scrape we add a loop
  index = 1
  repeat{
    response = get_request(url = url, api_key = api_key, params = params)
    response_data = content(response, 'parsed', encoding = 'UTF-8')
    main_data = c(main_data, response_data$data)
    url = response_data$meta$next_url
    if(!is.null(url)){
      index = index + 1
      params$page = index
    }else{
      print('break')
      break()
    }
  }
  print('total no of data points')
  print(length(main_data))
  json_data = toJSON(main_data,auto_unbox = TRUE, pretty = TRUE)
  return(json_data)
}

get_raw_data = function(serial_number, data_points=NULL, start_date=NULL, end_date = NULL){
  # this will save all the data
  main_raw_data = c()
  
  # adding serial number in endpoint
  raw_data_endpoint_with_serial = paste0('/devices','/', serial_number, raw_data_endpoint)
  
  # data endpoint for a specific defined serial number
  url = paste0(base_url, raw_data_endpoint_with_serial)
  
  # adding date filter if its not null in parameters
  date_filter = NULL
  if (all(!is.null(start_date) || !is.null(end_date))) {
    date_filter = paste0("timestamp_local,ge,",start_date,";timestamp_local,le,",end_date)
  }
  # different parameters we are sending with request
  params = list(
    page=1,limit=data_points,sort="timestamp_local,desc",
    per_page=1000, filter=date_filter)
  
  # for multiple page scrape we add a loop
  index = 1
  repeat{
    response = get_request(url = url, api_key = api_key, params = params)
    response_data = content(response, 'parsed', encoding = 'UTF-8')
    main_raw_data = c(main_raw_data, response_data$data)
    url = response_data$meta$next_url
    if(!is.null(url)){
      index = index + 1
      params$page = index
    }else{
      print('break')
      break()
    }
  }
  print('total no of data points')
  print(length(main_raw_data))
  json_data = toJSON(main_raw_data, auto_unbox = TRUE, pretty = TRUE)
  return(json_data)
}
data = get_data(serial_number =  serial_number, data_points = 720)
webmasterk <- jsonlite::fromJSON(data) %>%
  select( #selects certain variables from dataset
    timestamp_local, pm25, pm10
  ) %>%
  rename( # Renames them so that they display nicely in plots
    PM2.5 = pm25,
    PM10 = pm10
  ) 


webmasterk$timestamp_local <- ymd_hms(webmasterk$timestamp_local)

webmasterk <- webmasterk %>%
  rename(
    time = timestamp_local
  )

pm2p5avg <- nowcast(webmasterk)  #round(mean(webmasterk$PM2.5, na.rm = TRUE), digits = 2)
pm10avg <- nowcast10(webmasterk) # round(mean(webmasterk$PM10,na.rm = TRUE), digits= 2)   

#### Onset Weather Station API: ----------------------------------------------------------------------------
# authentication details 

get_access_token <- function(client_id, client_secret){
  # headers
  headers <- c(
    `Content-type` = 'application/x-www-form-urlencoded',
    `Accept` = 'application/json'
  )
  auth_url = 'https://webservice.hobolink.com/ws/auth/token'
  #  payload 
  data <- paste0("grant_type=client_credentials&client_id=",client_id,"&client_secret=",client_secret)
  
  auth_response <- httr::POST(
    url = auth_url, 
    add_headers(.headers = headers), 
    body = data)
  
  access_token = content(auth_response, as = 'parsed')$access_token
  return(access_token)
}


generic_data_request <- function(token, user_id, params){
  
  headers = c(
    `authority` = 'webservice.hobolink.com',
    `accept` = 'application/json;charset=utf-8',
    `authorization` = paste('Bearer',token)
  )
  
  response <- httr::GET(
    url = paste0('https://webservice.hobolink.com/ws/data/file/JSON/user/',user_id), 
    httr::add_headers(.headers=headers), 
    query = params)
  
  d = content(response, encoding = 'parsed')
  
  to_json = toJSON(d$observation_list, auto_unbox = TRUE)
  
  dataframe = fromJSON(to_json)
  
  # this column is creating problem and its contains NULL values
  # assigning NULL to this column
  dataframe$scaled_unit = 'NULL'
  return(dataframe)
  
}

get_data_using_start_and_end_date <- function(token, user_id, logger, start_date_time, end_date_time){
  
  #  authentication headers
  params = list(
    `loggers` = logger,
    `start_date_time` = start_date_time,
    `end_date_time` = end_date_time
  )
  
  data = generic_data_request(token, user_id, params)
  return(data)
  
}

get_last_hours_data <- function(token, user_id, logger, hours){
  
  
  # set this time zone of your device location
  time = with_tz(Sys.time(), tzone = "America/New_York")
  current_date_time = format(time)
  time_before_hours <-  format(time - (hours * 3600))
  
  params = list(
    `loggers` = logger,
    `start_date_time` = time_before_hours,
    `end_date_time` = current_date_time
  )
  
  data = generic_data_request(token, user_id, params)
  return(data)
  
}

client_id = "bardcsl_WS"
client_secret = "e82463df4346b6297cab652db53dbe9f5b4de46a"
token = get_access_token(client_id, client_secret)

user_id = '11160'
logger = '20989094'

hours = 1
# get data of last x hours
wdata = get_last_hours_data(token, user_id, logger, hours)


### Weather Data Code ----------------------------------------------------

wdata <- wdata %>%
  mutate(
    timestamp = ymd_hms(timestamp)
  ) 

# Here we are organizing the weather data into smaller dataframes that can be used easily by the plots
wddata <- wdata %>%
  filter(
    sensor_measurement_type == "Wind Direction"
  ) %>%
  rename(
    wd = si_value
  ) %>%
  select(
    timestamp,wd
  )

wsdata <- wdata %>%
  filter(
    sensor_measurement_type == "Wind Speed"
  ) %>%
  rename(
    ws = si_value
  ) %>%
  select(
    timestamp,ws
  )

tempdata <- wdata %>%
  filter(
    sensor_measurement_type == "Temperature"
  ) %>%
  rename(
    temp = si_value
  ) %>%
  select(
    timestamp,temp
  ) %>%
  tail(1)

rhdata <- wdata %>%
  filter(
    sensor_measurement_type == "RH"
  ) %>%
  rename(
    rh = si_value
  ) %>%
  select(
    timestamp,rh
  ) %>%
  tail(1)

rose_df <- inner_join(wddata,wsdata, by = "timestamp") 


### App Code ----------------------------------------------------

# Define UI for application
ui <- fluidPage(
   # Styling
   theme = shinytheme("darkly"),
   tags$head(includeCSS("app.css"),
            HTML(
              # <!-- Global site tag (gtag.js) - Google Analytics -->
                "<script async src='https://www.googletagmanager.com/gtag/js?id=G-E1XW5VZ9Z3'></script>
                <script>
                window.dataLayer = window.dataLayer || [];
              function gtag(){dataLayer.push(arguments);}
              gtag('js', new Date());
              
              gtag('config', 'G-E1XW5VZ9Z3');
              </script>"
            )
            
            ),
  
    # Application title
    # titlePanel("Kingston NY Air Quality"),

    titlePanel( div(column(width = 4, tags$a(href="https://landairwater.bard.edu/projects/kaqi/", tags$img(src = "bcesh_logo.png", height = 50, width = 400))),
                column(width = 8, h2("Kingston NY Air Quality"))
                  ),
              windowTitle="Kingston Particulate Matter"
  ),

  
  
    sidebarLayout(
      sidebarPanel(
        helpText("The Bard College Center for Environmental Sciences and Humanities maintains
               an air quality monitoring station on top of the Andy Murphy Neighborhood Center
               at 467 Broadway in Kingston, NY. Here you can see the most recent Particulate Matter data from this sensor. ",tags$br(),tags$br(),
                 "You can learn more information about the monitoring program ",
                 tags$a(href="https://landairwater.bard.edu/projects/kaqi/", "here."),
        tags$br(),tags$br(),
        "You can explore historic data from Kingston ",
        tags$a(href="https://tributary.shinyapps.io/AMNCexplorer/", "here.")),
        tags$br(),tags$br(),
        HTML("
          <h3>US EPA Air Quality Interpretation</h2>
            
            <table>
            <tr>
            <th>Air Quality</th>
            <th>PM2.5 Concentration</th>
            <th>Caution Notes</th>
            </tr>
            <tr style=color:black;background-color:rgb(0,228,0)>
            <td>Good</td>
            <td>0-12.0</td>
            <td>None</td>
            </tr>
            <tr style=color:black;background-color:rgb(255,255,0)>
            <td>Moderate</td>
            <td>12.1-35.4</td>
            <td>Unusually sensitive people should consider reducing prolonged or heavy exertion.</td>
            </tr>
            <tr style=color:black;background-color:rgb(255,126,0)>
            <td>Unhealthy for Sensitive Groups</td>
            <td>35.5-55.4</td>
            <td>People with heart or lung disease, older adults, children, and people of lower socioeconomic status should reduce prolonged or heavy exertion.</td>
            </tr>
            <tr style=color:black;background-color:rgb(255,0,0)>
            <td>Unhealthy</td>
            <td>55.5-150.4</td>
            <td>People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid prolonged or heavy exertion; everyone else should reduce prolonged or heavy exertion.</td>
            </tr>
            <tr style=background-color:rgb(143,63,151)>
            <td>Very Unhealthy</td>
            <td>150.5-250.4</td>
            <td>People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid all physical activity outdoors. Everyone else should avoid prolonged or heavy exertion.</td>
            </tr>
            <tr style=background-color:rgb(126,0,35)>
            <td>Hazardous</td>
            <td>250.5-500.4</td>
            <td>Everyone should avoid all physical activity outdoors; people with heart or lung disease, older adults, children, and people of lower socioeconomic status should remain indoors and keep activity levels low.</td>
            </tr>
            </table>
            "
        ), # end HTML
        tags$br(),tags$br(),
        tags$img(src = "AMNC_location1.png", width = "100%")
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Live Data",
                      
        fluidRow(
          column(6,
                plotOutput("plot1", height = "275px"),
                htmlOutput("pm2p5avg"),
                textOutput("pm2p5_caution")%>% 
                  tagAppendAttributes(class = 'caution'),
               # plotOutput("plot3") # windrose
          ),
         column(6,
                plotOutput("plot2", height = "275px"),
                htmlOutput("pm10avg"),
                textOutput("pm10_caution")%>% 
                  tagAppendAttributes(class = 'caution'),
               # plotlyOutput("plot4") # windspeed
         )
        ), # End first fluidRow
        br(),
        br(),
        fluidRow(
          column(6,
                 if (max(rose_df$ws) == 0) {
                   textOutput("no_wind")
                 }
                 else {
                 plotOutput("plot3") # windrose
                 }
          ),
          column(6,
                 plotlyOutput("plot4") # windspeed
          )
          ), # End second fluidRow
        
        br(),
        fluidRow(
        column(6,
               plotlyOutput("ftemp_plot") # temperature
        ),
        column(6,
               plotlyOutput("plot6") # RH
        )
        ) # End third fluidRow
        
        ), # End Tabpanel 1
        
        tabPanel("Past 12 Hours",
                 plotOutput("pm2.5Plot"),
                 verbatimTextOutput("summary"),
                 checkboxGroupInput("variable", "Observations to display:",
                                    c("PM2.5" = "PM2.5",
                                      "PM10" = "PM10"
                                    ),
                                    selected = "PM2.5"
                 )
  
                 
        ) # End Tabpanel 2
        
      ) # End Tabsetpanel

        
        ) # End mainPanel
    ) # End sidebarLayout
  ,
  # FOOTER 
  hr(),
  print("References:"), br(),
  print("Particulate Matter monitor:"), tags$a(href= "https://assets.quant-aq.com/downloads/spec-sheets/modulair-pm.latest.pdf", "QuantAQ Modulair"), br(),
  print("Weather Station:"), tags$a(href= "https://www.onsetcomp.com/datasheet/RX2100", "OnsetRX2100"), br(),
  print("This dashboards PM concentrations and warnings are based off of the EPA's Nowcast calculation method."), br(),
  print("More info on NowCast Reporting:"), tags$a(href= "https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf", "Technical Assistance Document for the Reporting of Daily Air Quality ")
  
    ) # End fluidPage


# Define server logic  ---------------------------------------------
server <- function(input, output) {
  thematic::thematic_shiny()
  
  # Subsets dataset based on variable selections for 12-hour plot
  halfday_df <-  reactive({
    webmasterk %>%
      select(
        time,
        c(input$variable)
      ) })
  
  
  # Plots ---------------------------------------------
    output$plot1 <- renderPlot({
      par(mar=c(1,4.1, 4.1, 2.1))
       barplot(
         rep(1,1),
         main = "PM2.5 - Fine Particles", cex.main=1.5,
         col = color1,
         space = 0,
         axes = FALSE
       ) 
      text(.5,.5,pm2p5_category, col = "black", cex = 3)
    })
    
    output$plot2 <- renderPlot({
      par(mar=c(1,4.1, 4.1, 2.1))
      barplot(
        rep(1,1),
        main = "PM10 - Coarse Particles", cex.main=1.5,
        col = color2,
        space = 0,
        axes = FALSE
      )
      text(.5,.5,pm10_category, col = "black", cex = 3)
    })
    
    # windrose
    output$plot3 <- renderPlot({ 
      validate(
        need(FALSE, "No wind currently... Check back later. ")
      )
      
      windRose(rose_df, paddle = FALSE, main = "Wind Direction - past hour", key.footer = expression("(m/s)"),  par.settings = list(axis.text = list(col = "white"), par.main.text = list(col = "white", fontsize=18),add.text = list(col = "white"), par.sub.text = list(col = "white",fontsize=14), fontsize=list(text=20))
      )
    })
    
    # windspeed gauge
    output$plot4 <- renderPlotly({ 
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = mean(wsdata$ws, na.rm = TRUE),
        title = list(text = "Wind Speed, m/s"),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis =list(range = list(NULL, 24))
        ) 
      ) %>%
        layout(margin = list(l=20,r=30),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               font = list(color = '#00FFFF')
               )
    })
    
    # C temperature gauge
    output$plot5 <- renderPlotly({ 
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = tempdata$temp,
        title = list(text = "Temperature, deg. C"),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis =list(range = list(-25, 45))
        ) 
      ) %>%
        layout(margin = list(l=20,r=30),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               font = list(color = '#ff0000')
        )
    })
    
    # F temperature gauge
    output$ftemp_plot <- renderPlotly({ 
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = (tempdata$temp * (9/5)) + 32,
        title = list(text = "Temperature, deg. F"),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis =list(range = list(-15, 110))
        ) 
      ) %>%
        layout(margin = list(l=20,r=30),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               font = list(color = '#ff0000')
        )
    })
    
    # RH gauge
    output$plot6 <- renderPlotly({ 
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = rhdata$rh,
        title = list(text = "Relative Humidity, %"),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis =list(range = list(0, 100))
        ) 
      ) %>%
        layout(margin = list(l=20,r=30),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               font = list(color = '#fcf403')
        )
    })
    

    
    output$pm2.5Plot <- renderPlot({
      # Displays gentle error message if no variables are selected in checkbox
      shiny::validate(
         need(input$variable != "", "Please select at least one variable to display")
        )
      
      # Time series point chart displaying data that user selects
      halfday_df() %>%
        pivot_longer(starts_with("PM"), names_to = "Pollutant Class", values_to = "observation") %>%
        ggplot(aes(x = time, y = observation, color = `Pollutant Class`) ) +
        geom_point() +
        scale_color_manual(values = c("PM2.5" = "darkorange1",
                                      "PM10"="dodger blue"
                                        )) +
        scale_x_datetime(minor_breaks = NULL, date_breaks = "30 min", date_labels = "%b%e %l:%M %p") +
        labs(
          y = expression(Mass - (μg/~m^3)),
          x = NULL,
          title = paste(
            "Most Recent Particulate Matter Readings"
          ) ) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5) ) +
        theme(plot.title = element_text(size = 18)) +
        theme(plot.subtitle = element_text(hjust = 0.5) ) +
        theme(axis.text.x = element_text(angle=60, hjust=1) ) +
        theme(axis.text = element_text(size = 11)) +
        theme(axis.title = element_text(size = 13)) +
        theme(legend.text = element_text(size = 11))     
         
    }) # End of renderPlot
    
    
    # Other outputs ---------------------------------------------
    output$summary <- renderPrint({
      dataset <- webmasterk %>%
        select(
          -time
        )
      summary(dataset )
              })
    
    #mass_express <- "μg / m ^ 3"
    
    output$pm2p5avg <- renderUI({
      HTML(paste("<h3>Current PM2.5:", pm2p5avg, "μg / m <sup>3</sup></h3>"))
    })
    
    output$pm10avg <- renderText({
      HTML(paste("<h3>Current PM2.5:", pm10avg, "μg / m <sup>3</sup></h3>"))
    })

    
# Set Categories and Cationary Statements based on PM2.5 concentrations        
    if (pm2p5avg < 12.1) {
      pm2p5_category = "Good"
      pm2p5_caution = "None"
      color1 = rgb(0, 228, 0, max=255)
    }
    else if (pm2p5avg < 35.5) {
      pm2p5_category = "Moderate"
      pm2p5_caution = "Unusually sensitive people should consider reducing prolonged or heavy exertion."
      color1 = rgb(255, 255, 0, max=255)
    }   
    else if (pm2p5avg < 55.5) {
      pm2p5_category = "Unhealthy for Sensitive Groups"
      pm2p5_caution = "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should reduce prolonged or heavy exertion."
      color1 = rgb(255, 126, 0, max=255)
    }
    else if (pm2p5avg < 150.5) {
      pm2p5_category = "Unhealthy"
      pm2p5_caution = "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid prolonged or heavy exertion; everyone else should reduce prolonged or heavy exertion."
      color1 = rgb(255, 0, 0, max=255)
    }    
    else if (pm2p5avg < 250.5) {
      pm2p5_category = "Very Unhealthy"
      pm2p5_caution = "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid all physical activity outdoors. Everyone else should avoid prolonged or heavy exertion."
      color1 = rgb(143, 63, 151, max=255)
    }    
    else if (pm2p5avg >= 250.5) {
      pm2p5_category = "Hazardous"
      pm2p5_caution = "Everyone should avoid all physical activity outdoors; people with heart or lung disease, older adults, children, and people of lower socioeconomic status should remain indoors and keep activity levels low."
      color1 = rgb(126, 0, 35, max=255)
    }    

# Set Categories and Cautionary Statements based on PM10 concentrations        
    if (pm10avg < 55) {
      pm10_category = "Good"
      pm10_caution = "None"
      color2 = rgb(0, 228, 0, max=255)
    }
    else if (pm10avg < 155) {
      pm10_category = "Moderate"
      pm10_caution = "Unusually sensitive people should consider reducing prolonged or heavy exertion."
      color2 = rgb(255, 255, 0, max=255)
    }   
    else if (pm10avg < 255) {
      pm10_category = "Unhealthy for Sensitive Groups"
      pm10_caution = "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should reduce prolonged or heavy exertion."
      color2 = rgb(255, 126, 0, max=255)
    }
    else if (pm10avg < 355) {
      pm10_category = "Unhealthy"
      pm10_caution = "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid prolonged or heavy exertion; everyone else should reduce prolonged or heavy exertion."
      color2 = rgb(255, 0, 0, max=255)
    }    
    else if (pm10avg < 425) {
      pm10_category = "Very Unhealthy"
      pm2p5_caution = "People with heart or lung disease, older adults, children, and people of lower socioeconomic status should avoid all physical activity outdoors. Everyone else should avoid prolonged or heavy exertion."
      color2 = rgb(143, 63, 151, max=255)
    }    
    else if (pm10avg >= 425) {
      pm10_category = "Hazardous"
      pm10_caution = "Everyone should avoid all physical activity outdoors; people with heart or lung disease, older adults, children, and people of lower socioeconomic status should remain indoors and keep activity levels low."
      color2 = rgb(126, 0, 35, max=255)
    }        
    
        
    output$pm2p5_category <- renderText({
      paste(pm2p5_category)
    })    
    
    output$pm10_category <- renderText({
      paste(pm10_category)
    })   
    
    output$pm2p5_caution <- renderText({
      paste("EPA Caution Notes:", pm2p5_caution)
    })    
    
    output$pm10_caution <- renderText({
      paste("EPA Caution Notes:", pm10_caution)
    })   
    
    output$no_wind <- renderText({
      paste("No wind currently.... Check back later.")
    })   
    
} # End server function

# Run the application 
shinyApp(ui = ui, server = server)
