library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leaflet.extras)
library(DT)


setwd("C:/Users/abhis/Documents/R/Case Study")


ds <- read.csv("filtered_data.csv")
data_geo <- read.csv("gemeinden_clean.csv") %>%
  select(-X.1)
names(data_geo)[6] <- "Distanz"
names(data_geo)[7] <- "Anz_Fahrzeuge"
names(data_geo) <- c("No", "PLZ", "Gemeinde", "Laengengrad", "Breitengrad", "Distanz", "Anz_Fahrzeuge")


ui <- fluidPage(
  titlePanel("Map for Rückholaktion"),
  
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("no_circles", "# Kreise", c(1,2,3,4,5)),
      checkboxInput("toggle_marker", "Marker einblenden", 1),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")), # set slider color to identify with circles
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: magenta}")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: cyan}")),
      conditionalPanel(
        condition = "input.no_circles >= 1",
        sliderInput("radius_1", "Radius 1 [km]", 0, 200, 0)
      ),
      conditionalPanel(
        condition = "input.no_circles >= 2",
        sliderInput("radius_2", "Radius 2 [km]", 0, 200, 0)
      ),
      conditionalPanel(
        condition = "input.no_circles >= 3",
        sliderInput("radius_3", "Radius 3 [km]", 0, 200, 0)
      ),
      conditionalPanel(
        condition = "input.no_circles >= 4",
        sliderInput("radius_4", "Radius 4 [km]", 0, 200, 0)
      ),
      conditionalPanel(
        condition = "input.no_circles >= 5",
        sliderInput("radius_5", "Radius 5 [km]", 0, 200, 0)
      )
    ),
    
    mainPanel(
      #this will create a space for us to display our map
      tabsetPanel(type = "tabs",
                  tabPanel("Karte", leafletOutput(outputId = "mymap", height = "625px")),
                  tabPanel("Tabelle", DT::dataTableOutput("table"))
      ),
      textOutput("text1")
    )
  ),
  
  fluidRow(
    sidebarPanel(height = "300px", width = "100px",
                 conditionalPanel(
                   condition = "input.no_circles >= 1",
                   plotOutput(outputId = "hist1")
                 )
             )
  )
)


server <- function(input, output, session) {
  df <- reactive({data.frame(Kreis = 1:5,
                             Radius = c(input$radius_1, input$radius_2, input$radius_3, input$radius_4, input$radius_5),
                             Anzahl = c(nrow(filter(ds, as.numeric(distance) < as.numeric(input$radius_1))),
                                        nrow(filter(ds, as.numeric(distance) < as.numeric(input$radius_2))),
                                        nrow(filter(ds, as.numeric(distance) < as.numeric(input$radius_3))),
                                        nrow(filter(ds, as.numeric(distance) < as.numeric(input$radius_4))),
                                        nrow(filter(ds, as.numeric(distance) < as.numeric(input$radius_5)))
                             ))
  })
  # find max input radius to only plot gemeinde once
  max_radius <- reactive({max(input$radius_1, input$radius_2, input$radius_3, input$radius_4, input$radius_5)})
  # find gemeinde within range input radius
  gemeinde <- reactive({data.frame(filter(data_geo, Distanz < max_radius())
  )})
  table_data <- reactive({
    filter(data_geo, Distanz < max_radius()) %>%
      select(Postleitzahl, Gemeinde, Distanz, Anz_Fahrzeuge)
  })
  # enables toggle marker
  observe({
    proxy <- leafletProxy("mymap", data = ds)
    proxy %>% clearMarkers()
    if (input$toggle_marker) {
      proxy %>% addMarkers(data = gemeinde(), lat = ~ gemeinde()$Breitengrad, lng = ~ gemeinde()$Laengengrad,
                           popup = paste("<div>",
                                         "<h5>",
                                         gemeinde()$Gemeinde,
                                         "</h5>",
                                         "</div><br>",
                                         "Anz. Fzg: ",
                                         gemeinde()$Anz_Fahrzeuge))
    }
  })
  # create the map
  output$mymap <- renderLeaflet({
    leaflet(ds) %>%
      setView(lat = 48.7706, lng = 9.18457, zoom = 8)  %>% #setting the view over stuttgart
      addTiles() %>%
      setMaxBounds(lng1 = 6, # setting max. boundaries ~ border of Germany
                   lat1 = 47.1,
                   lng2 = 14.7,
                   lat2 = 54.6) %>%
      addCircles(lat = 48.7706, lng = 9.18457, radius = (input$radius_1*1000), color = "Green") %>% # add circle at lat/lng of stuttgart, radius*1000 = km
      addCircles(lat = 48.7706, lng = 9.18457, radius = (input$radius_2*1000), color = "Orange") %>%
      addCircles(lat = 48.7706, lng = 9.18457, radius = (input$radius_3*1000), color = "Purple") %>%
      addCircles(lat = 48.7706, lng = 9.18457, radius = (input$radius_4*1000), color = "Magenta") %>%
      addCircles(lat = 48.7706, lng = 9.18457, radius = (input$radius_5*1000), color = "Cyan")
  })
  # basic barplot
  output$hist1 <- renderPlot({
    ggplot(data = df(), aes(Kreis, y = Anzahl, fill = c("1","2","3","4","5"))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Anzahl), vjust = 1.5, size = 5) +
      ggtitle("Anzahl Fahrzeuge in Umkreisen") +
      theme_minimal() +
      scale_fill_manual("legend", values = c("1" = "Darkgreen","2" = "Orange","3" = "Purple","4" = "Magenta","5" = "Cyan"))
  })
  output$table <- DT::renderDataTable({
    table_data()
  })
}





shinyApp(ui = ui, server = server)