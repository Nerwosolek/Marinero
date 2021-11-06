library(shiny)
library(shiny.semantic)
library(dplyr)
library(leaflet)

just_types <- readRDS("data/just_types.rds")
ships_moves_max_dist <- readRDS("data/ships_max_dist.rds")


marineroGridTempl <- grid_template(
  default = list(
    areas = cbind(
      c("header", "drop_downs", "map")
    ),
    rows_height = c("50px", "120px", "400px"),
    cols_width = c("500px")
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
              container_style = "border: 1px solid; width: 520px;  height: 600px; text-align: center;",
              area_styles = list(header = "border: 3px solid #5c969e",
                                 drop_downs = "position: static;"),
          header = div(class = "title", header("Ships' biggest moves.", description = "",icon = "ship")),
           drop_downs = grid(dropdownsGrid,
               area_styles = list(left = "border: 1px solid #5c969e; position:static;",
                                  right = "border: 1px solid #5c969e; position:static;"),                                        
             
             left = div(class = "myDropdown",
                               h4("Select type:"),
                dropdown_input("ship_types_input",
                              choices = NULL,
                              choices_value = NULL,
                              default_text = "Select ship type:"
                              )),
             right = div(class = "myDropdown",
               h4("Select ship name:", description = ""),
             dropdown_input("ship_names_input",
                            choices = NULL,
                            choices_value = NULL,
                            type = "selection",
                            default_text = "Select ship name:"
             ))
           ),
          map = segment(leafletOutput("map"), class = "raised")
         )
    )



server <- function(input, output, session){
  
  ship_type <- reactive({
    filter(ships_moves_max_dist, SHIPTYPE_ID == input$ship_types_input)
  })
  
  ship_name <- reactive({
    out <- filter(ships_moves_max_dist, SHIP_ID == input$ship_names_input)  
    print(out)
    out
  })
  observeEvent(input$ship_types_input, {
    update_dropdown_input(session,
                          input_id = "ship_types_input",
                          choices = c("-",just_types$SHIP_TYPE_NAME),
                          choices_value = c(NA,just_types$SHIPTYPE_ID)
                          )
  }, once = TRUE)
  observeEvent(ship_type(), {
    print(paste("ship_type# ",nrow(ship_type())))
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
      print("In else.")
      update_dropdown_input(session,
                            input_id = "ship_names_input",
                            choices = c("-"),
                            choices_value = c(-1),
      )
    }
  })
  
  observeEvent(ship_name(),{
    ship_data_row <- ship_name()
    
    output$map <- renderLeaflet({
        df <- tibble(.rows = 2)
        if (nrow(ship_data_row) == 1){
          df$lat <- c(ship_data_row$LAT.prev, ship_data_row$LAT)
          df$lng <- c(ship_data_row$LON.prev, ship_data_row$LON)  
          print(df)
          leaflet(data = df) %>%
          addProviderTiles("CartoDB.Positron") %>%
          clearBounds() %>%
          addCircleMarkers(lng = df$lng[1], lat = df$lat[1], radius = 5, color = "green", popup = ship_data_row$SHIPNAME) %>%
          addCircleMarkers(lng = df$lng[2], lat = df$lat[2], radius = 5, color = "red", popup = ship_data_row$SHIPNAME) %>%
          addPolylines(lng = ~lng, lat = ~lat,color = "red", dashArray = "4 8",
                       weight = 3, popup = paste("Distance: ",ship_data_row$dist, sep = ""),
                       popupOptions = popupOptions(closeButton = FALSE)) %>%
            htmlwidgets::onRender("
              function(el, x) {
                this.zoomControl.setPosition('bottomright')
              }")
          }
        else {
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lat = 58.5, lng = 19, zoom = 5) %>%
            htmlwidgets::onRender("
              function(el, x) {
                this.zoomControl.setPosition('bottomright')
              }")
        }
      })
  })
}

shinyApp(ui, server)