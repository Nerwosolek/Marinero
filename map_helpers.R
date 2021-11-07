
# Map config --------------------------------------------------------------
map_cfg <- function(par){
  cfg <- c(
    "map_provider" = "CartoDB.Positron",
    "marker_radius" = "5",
    "start_color" = "green",
    "end_color" = "red",
    "dash_color" = "red",
    "dash_shape" = "4 8",
    "dash_weight" = "3")
  unname(cfg[par])
} 


# Helper functions for map ------------------------------------------------
pos_popup <- function(data, start = TRUE) {
  paste(paste0("<U>", ifelse(start, "Start position", "End position"),"</U>"),
        paste("Lon:",
              ifelse(start, round(data$LON.prev, 3),round(data$LON, 3)),
              "Lat:",ifelse(start, round(data$LAT.prev, 3), round(data$LAT, 3))
              ),
        paste("Ship name:",data$SHIPNAME), 
        paste("Speed:",data$SPEED),
        paste("Course:",data$COURSE),
        sep = "<BR>")
}

move_popup <- function(data) {
  paste("Distance:",round(data$dist, 3))
}

zoom_ctrl_pos <- function(x, pos) {
  htmlwidgets::onRender(x, paste0("
              function(el, x) {
                this.zoomControl.setPosition('",pos,"')
              }"))
}