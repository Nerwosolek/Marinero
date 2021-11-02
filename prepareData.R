library(readr)
library(dplyr)
library(Imap)
library(crayon)
start_time = Sys.time()
ships_raw <- 
  read_csv("data/ships_04112020.zip", 
                  col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
                                   date = col_date(format = "%Y-%m-%d"), 
                                   is_parked = col_logical()))
cat(green("Zipped csv read."))
prev_time = Sys.time()
prev_time - start_time

ships <- ships_raw %>% dplyr::arrange(SHIP_ID, desc(DATETIME))
rm(ships_raw)
cat(green("Ships sorted."))
cat(green(Sys.time() - prev_time))
prev_time = Sys.time()
ships <- ships %>% group_by(SHIP_ID) %>% mutate(pos_nbr = seq(n(),1))
ships <- ships %>% mutate(prev_pos_nbr = pos_nbr - 1)
cat(green("Ships positions calculated."))
cat(green(Sys.time() - prev_time))
prev_time = Sys.time()
ships <- ungroup(ships)
ships_LJ <- 
  ships %>% 
  left_join(ships, suffix = c("",".prev"), by = c("SHIP_ID" = "SHIP_ID","prev_pos_nbr" = "pos_nbr")) %>% 
  select(SHIP_ID, SHIPNAME, LAT, LON, LAT.prev, LON.prev, DATETIME, SHIPTYPE, ship_type, 
         SPEED, COURSE, HEADING, DESTINATION, PORT, port, FLAG, LENGTH, WIDTH, DWT, 
         date, week_nb, is_parked, pos_nbr, prev_pos_nbr)
rm(ships)
gc()
cat(green("Ships prev positions joined."))
cat(green(Sys.time() - prev_time))
prev_time = Sys.time()
dist_imap <- 
  function(x.lon,x.lat,y.lon, y.lat, units = "m")
  {
    #res <- 1:length(x.lon)
    mapply(gdist,x.lon,x.lat,y.lon, y.lat, units = units)
    #for (i in seq_along(x.lon)) {
    #  res[i] <- gdist(x.lon,x.lat,y.lon, y.lat, units = units)
    #}
    #res
  }

distances <- dist_imap(ships_LJ$LON, ships_LJ$LAT, ships_LJ$LON.prev, ships_LJ$LAT.prev)
ships_LJ$dist <- distances
rm(distances)
cat(green("Ships moves distances calculated."))
cat(green(Sys.time() - prev_time))
prev_time = Sys.time()
ships_moves_max_dist <- 
  ships_LJ %>% 
  group_by(SHIP_ID) %>% 
  slice_max(dist, with_ties = FALSE, n = 1)
cat(green("Ships longest movement selected."))
cat(green(Sys.time() - prev_time))
end_time = Sys.time()
end_time - start_time
just_types <- ungroup(ships_moves_max_dist) %>% select(SHIPTYPE, ship_type) %>% unique()

