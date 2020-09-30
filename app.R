library(dplyr)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(geosphere)

#importing data
dataset_ship <- read.csv('https://media.githubusercontent.com/media/AKostiv8/data-ship/master/ships.csv')

ui <- bootstrapPage(
    
    theme = shinythemes::shinytheme('simplex'),
    leaflet::leafletOutput('mymap', width = '100%', height = '100%'),
    absolutePanel(top = 10, right = 10, id = 'controls',
                  selectInput("shiptype", "Ship type", choices = unique(dataset_ship$ship_type)),
                  selectInput("shipname", "Ship name", choices = NULL),
                  textOutput("m_distance")
                  # CODE BELOW: Add an action button named show_about
                  #actionButton('show_about', 'About')
    ),
    tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
    .shiny-output-error { visibility: hidden; }
    .shiny-output-error:before { visibility: hidden; }
  ")
)


# Define server 
server <- function(input, output, session) {
    
    shiptype <- reactive({
        filter(dataset_ship, ship_type == input$shiptype)
    })
    
    observeEvent(shiptype(), {
        choices <- unique(shiptype()$SHIPNAME)
        updateSelectInput(session, "shipname", choices = choices) 
    })
    
    data_sample <- reactive({
        req(input$shiptype)
        req(input$shipname)
        
        dataset_ship %>%
            filter(ship_type == input$shiptype) %>%
            filter(SHIPNAME == input$shipname) %>%
            mutate(Distance = distHaversine(cbind(LON, LAT), cbind(lag(LON), lag(LAT))))
        
    })
    
    max_point <- reactive({
        req(data_sample)
        
        max(data_sample()$Distance, na.rm = TRUE)
        
    })
    
    
    
    
    output$m_distance <- renderText({  
        paste("Distance:", round(max_point(),4), "meters.")
    })
    
    output$mymap <- renderLeaflet({
        
        dist_vector <- c((which(data_sample()$Distance == max_point())), 
                         ((which(data_sample()$Distance == max_point()))-1))
        
        
        
        leaflet() %>%
            setView(lng = mean(data_sample()[dist_vector, ]$LON),
                    lat = mean(data_sample()[dist_vector, ]$LAT), zoom = 7) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircles(lng = data_sample()[dist_vector, ]$LON, 
                       lat = data_sample()[dist_vector, ]$LAT,
                       weight = 1, color = c("navy", "red"), 
                       radius = 3500, 
                       popup = data_sample()[dist_vector, ]$DATETIME) %>%
            addProviderTiles(providers$CartoDB.Positron)
    })
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
