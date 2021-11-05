library(shiny)
library(shiny.semantic)
library(dplyr)
library(leaflet)

#ui <- fluidPage(title = "Marinero",
#                textOutput(outputId = "hello"))

just_types <- readRDS("data/just_types.rds")
ships_moves_max_dist <- readRDS("data/ships_max_dist.rds")

ui <- semanticPage(title = "Marinero",
        header("Select ship type:", description = ""),
        dropdown_input("ship_types_input",
                       choices = just_types$SHIP_TYPE_NAME,
                       choices_value = just_types$SHIPTYPE_ID,
                       type = "selection",
                       default_text = "Select ship type:"
                      ),
        header("Select ship name:", description = ""),
        dropdown_input("ship_names_input",
                        choices = NULL,
                        choices_value = NULL,
                        type = "selection",
                        default_text = "Select ship name:"
                      ),
        semantic_DTOutput("ship_table"),
        leafletOutput("map")
        )



server <- function(input, output, session){
  
  ship_type <- reactive({
    filter(ships_moves_max_dist, SHIPTYPE_ID == input$ship_types_input)
  })
  
  ship_name <- reactive({
    filter(ships_moves_max_dist, SHIP_ID == input$ship_names_input)
  })
  
  observeEvent(ship_type(), {
    choices <- ship_type()$SHIPNAME
    choices_values <- ship_type()$SHIP_ID
    update_dropdown_input(session,
                          input_id = "ship_names_input",
                          choices = choices,#ship_names,
                          choices_value = choices_values#ship_ids
    )
  })
  
  observeEvent(ship_name(),{
    ship_data_row <- ship_name()
    output$ship_table <- DT::renderDataTable(
      semantic_DT(ship_data_row)
    )
    output$map <- renderLeaflet(
      {df <- tibble(.rows = 2)
      
      df$lat <- c(ship_data_row$LAT.prev, ship_data_row$LAT)
      df$lng <- c(ship_data_row$LON, ship_data_row$LON.prev)
      
      
      leaflet(data = df) %>%
        #addProviderTiles("OpenTopoMap") %>%  
        addTiles() %>%
        addCircleMarkers(radius = 5, color = "red", popup = ship_data_row$SHIPNAME) %>%
        addPolylines(lng = ~lng, lat = ~lat,color = "red", dashArray = "4 8",
                     weight = 3, popup = paste("Distance: ",ship_data_row$dist, sep = ""),
                     popupOptions = popupOptions(closeButton = FALSE)) %>%
        clearBounds()}
    )
  })
  
  
  
}

shinyApp(ui, server)