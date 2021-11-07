library(shiny)
library(shiny.semantic)
library(dplyr)
library(leaflet)
source("map_helpers.R")

just_types <- readRDS("data/just_types.rds")
ships_moves_max_dist <- readRDS("data/ships_max_dist.rds")


marineroGridTempl <- grid_template(
  default = list(
    areas = cbind(
      c("header", "drop_downs", "map")
    ),
    rows_height = c("65px", "120px", "400px"),
    cols_width = c("570px")
  ))

dropdownsGrid <- grid_template(
  default = list(
    areas = rbind(c("left", "right")),
    rows_height = c("100%"),
    cols_width = c("50%", "50%")
  )
)


ui <- semanticPage(title = "Marinero",
         tags$head(
           tags$link(rel = "stylesheet", type = "text/css", href = "marinero.css")
         ),
         grid(marineroGridTempl,
          header = segment(class = "title", header("Ships' biggest moves.", description = "",icon = "ship")),
           drop_downs = segment(grid(dropdownsGrid,
             left = div(class = "myDropdown",
                      h4("Select type:"),
                      dropdown_input("ship_types_input",
                        choices = NULL,
                        choices_value = NULL,
                        default_text = "Select ship type:"
                      )
                    ),
             right = div(class = "myDropdown",
               h4("Select ship name:"),
             dropdown_input("ship_names_input",
                            choices = NULL,
                            choices_value = NULL,
                              default_text = "Select ship name:"
             ))
           )),
          map = segment(leafletOutput("map"))
         )
    )



server <- function(input, output, session){
  
  ship_type <- reactive({
    filter(ships_moves_max_dist, SHIPTYPE_ID == input$ship_types_input)
  })
  
  ship_name <- reactive({
    filter(ships_moves_max_dist, SHIP_ID == input$ship_names_input)  
  })
  observeEvent(input$ship_types_input, {
    update_dropdown_input(session,
                          input_id = "ship_types_input",
                          choices = c("-",just_types$SHIP_TYPE_NAME),
                          choices_value = c(NA,just_types$SHIPTYPE_ID)
                          )
  }, once = TRUE)
  observeEvent(ship_type(), {
    if (nrow(ship_type()) > 0) {
      choices <- ship_type()$SHIPNAME
      choices_values <- ship_type()$SHIP_ID
      update_dropdown_input(session,
                            input_id = "ship_names_input",
                            choices = choices,#ship_names,
                            choices_value = choices_values#ship_ids
      )
    }
    else {
      update_dropdown_input(session,
                            input_id = "ship_names_input",
                            choices = c("-"),
                            choices_value = c(-1),
      )
    }
  })
  
    output$map <- renderLeaflet({
      ship_data_row <- ship_name()
        
        if (nrow(ship_data_row) == 1){
          df <- tibble(.rows = 2)
          df$lat <- c(ship_data_row$LAT.prev, ship_data_row$LAT)
          df$lng <- c(ship_data_row$LON.prev, ship_data_row$LON)  

          leaflet(data = df) %>%
          addProviderTiles(map_cfg("map_provider")) %>%
          clearBounds() %>%
          addCircleMarkers(lng = df$lng[1], lat = df$lat[1], radius = map_cfg("marker_radius"), 
                           color = map_cfg("start_color"), popup = pos_popup(ship_data_row)) %>%
          addCircleMarkers(lng = df$lng[2], lat = df$lat[2], radius = map_cfg("marker_radius"), 
                           color = map_cfg("end_color"), popup = pos_popup(ship_data_row, FALSE)) %>%
          addPolylines(lng = ~lng, lat = ~lat,color = map_cfg("dash_color"), dashArray = map_cfg("dash_shape"),
                       weight = map_cfg("dash_weight"), popup = move_popup(ship_data_row),
                       popupOptions = popupOptions(closeButton = FALSE)) %>%
            zoom_ctrl_pos("bottomright")
          }
        else {
          leaflet() %>%
          addProviderTiles(map_cfg("map_provider")) %>%
          setView(lat = 58.5, lng = 19, zoom = 5) %>%
            zoom_ctrl_pos("bottomright")
        }
      
  })
}

shinyApp(ui, server)