library(readr)
library(dplyr)
library(Imap)
library(crayon)

rep_time <- function(text, time_difference) {
  cat(green(text), "\n")
  cat("Took",time_difference,units(time_difference),"\n", sep = " ")  
}

dist_imap <- 
  function(ships_moves, units = "m")
  {
    mapply(gdist, ships_moves$LON.prev, ships_moves$LAT.prev,
           ships_moves$LON, ships_moves$LAT, units = units)
  }

prepareData <- function(input_zip = "data/ships_04112020.zip", 
                        output_types = "data/just_types.rds",
                        output_ships = "data/ships_max_dist.rds"
                        )
{
  start_time = Sys.time()

# read ZIP file with ships data -------------------------------------------

  ships_raw <- 
    read_csv(input_zip, 
                    col_types = cols(DATETIME = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
                                     date = col_date(format = "%Y-%m-%d"), 
                                     is_parked = col_logical()))

# Tiding column names -----------------------------------------------------

    ships_raw <- rename(ships_raw, SHIPTYPE_ID = SHIPTYPE, 
                PORT_REPORTED = PORT,
                PORT_LOCATED = port, 
                DATE = date,
                WEEK_NB = week_nb, 
                SHIP_TYPE_NAME = ship_type, 
                IS_PARKED = is_parked)

  prev_time = Sys.time()
  rep_time("Zipped csv read.",prev_time - start_time)

# Sorting every ship by date & time ---------------------------------------

  ships <- ships_raw %>% dplyr::arrange(SHIP_ID, desc(DATETIME))
  
  rm(ships_raw)
  rep_time("Ships sorted.", Sys.time() - prev_time)
  prev_time = Sys.time()

# Calculating ships positions number and previous positions number --------

  ships <- ships %>% group_by(SHIP_ID) %>% mutate(pos_nbr = seq(n(),1))
  ships <- ships %>% mutate(prev_pos_nbr = pos_nbr - 1)
  rep_time("Ships positions calculated.", Sys.time() - prev_time)
  prev_time = Sys.time()
  ships <- ungroup(ships)

# Getting current and previous geolocation in 1 row -----------------------
  
  ships_LJ <- 
    ships %>% 
    left_join(ships, suffix = c("",".prev"), by = c("SHIP_ID" = "SHIP_ID","prev_pos_nbr" = "pos_nbr")) %>% 
    select(SHIP_ID, SHIPNAME, LAT, LON, LAT.prev, LON.prev, DATETIME, SHIPTYPE_ID, SHIP_TYPE_NAME, 
           SPEED, COURSE, HEADING, DESTINATION, PORT_REPORTED, PORT_LOCATED, FLAG, LENGTH, WIDTH, DWT, 
           DATE, WEEK_NB, IS_PARKED, pos_nbr, prev_pos_nbr)
  rm(ships)
  gc()
  rep_time("Ships prev positions joined.", Sys.time() - prev_time)
  prev_time = Sys.time()

# Calculating distances for every move in meters. -------------------------

  distances <- dist_imap(ships_LJ)
  ships_LJ$dist <- distances
  rm(distances)
  rep_time("Ships moves distances calculated.", Sys.time() - prev_time)
  prev_time = Sys.time()

# Choosing max distance for every ship ------------------------------------

  ships_moves_max_dist <- 
    ships_LJ %>% 
    group_by(SHIP_ID) %>% 
    slice_max(dist, with_ties = FALSE, n = 1) %>%
    ungroup() %>%
    arrange(SHIPTYPE_ID, SHIPNAME)
  rep_time("Ships longest movement selected.", Sys.time() - prev_time)
  prev_time = Sys.time()

# Ship types for dropdown -------------------------------------------------

  just_types <- ungroup(ships_moves_max_dist) %>% 
    select(SHIPTYPE_ID, SHIP_TYPE_NAME) %>% 
    unique() %>%
    arrange(SHIP_TYPE_NAME)
  saveRDS(just_types, file = output_types)
  saveRDS(ships_moves_max_dist, file = output_ships)
  rm(ships_LJ)
  rep_time("Files saved.", Sys.time() - prev_time)
  rep_time("Whole process ended.", Sys.time() - start_time)
}

prepareData()