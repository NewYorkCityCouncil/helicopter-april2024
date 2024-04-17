# Wrangle flights for analysis

## Load Libraries -----------------------------------------------
source("code/00_load_dependencies.R")

# Read and clean data --------------------------------------------------
# flight points

flight_points_df <- read_parquet("data/output/flight_points/2023-05-15.parquet") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# heliport zones
heliports_path <- "data/input/heliports.csv"
heliport_df <- 
  read_csv(heliports_path) %>%
  janitor::clean_names() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS) %>%
  mutate(
    geometry = st_buffer(geometry, dist = radius) 
  )

# convert points to lines
flight_lines <- flight_points_df %>% 
  group_by(flight_id) %>% 
  # keep the timestamp order
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>%
  # smooth the edges of the lines
  smoothr::smooth(method = "spline")

# merge flight summary with flight lines to add geometry data
flight_summary <- read_csv("data/output/flight_summary.csv") %>% 
  merge(flight_lines, by = "flight_id") %>%
  st_as_sf()

# manually edit flights that end right before/after heliport radius
# start
manual_kearny_start <- c("31", "40", "41", "42", "44", "144", "145", "147", "162", 
                         "166", "181", "182", "183", "184", "185", "188", "263", 
                         "264", "265", "266", "279", "282", "312")
manual_teterboro_start <- c("220")
manual_nypd_start <- c("321", "322")
# end
manual_kearny_end <- c("147", "181", "185", "188")
manual_midtownwest_end <- c("51", "220", "312")
manual_dmh_end <- c("173", "174")

# create df of all flights used for mapping
flights <- flight_summary %>%
  mutate(
    flight_start_heliport_name = case_when(
      flight_id %in% manual_kearny_start ~ "Kearny, NJ", 
      flight_id %in% manual_teterboro_start ~ "Teterboro Airport", 
      flight_id %in% manual_nypd_start ~ "NYPD Floyd Bennett Field", 
      TRUE ~ flight_start_heliport_name
    ), 
    flight_end_heliport_name = case_when(
      flight_id %in% manual_kearny_end ~ "Kearny, NJ", 
      flight_id %in% manual_midtownwest_end ~ "West 30th Street Heliport", 
      flight_id %in% manual_dmh_end ~ "Downtown Manhattan Heliport", 
      TRUE ~ flight_end_heliport_name
      )
  ) %>%
  filter(starts_at != "unknown" | !is.na(flight_start_heliport_name), 
         ends_at != "unknown" | !is.na(flight_end_heliport_name)) %>%
  mutate(
    flight_start = case_when(
      !is.na(flight_start_heliport_name) ~ flight_start_heliport_name, 
      flight_start_bbox_edge == "S" ~ "South of NYC (Unknown Location)",
      flight_start_bbox_edge == "N" ~ "North of NYC (Unknown Location)",
      flight_start_bbox_edge == "W" ~ "West of NYC (Unknown Location)",
      flight_start_bbox_edge == "E" ~ "East of NYC (Unknown Location)",
      TRUE ~ NA
      ), 
    flight_end = case_when(
      !is.na(flight_end_heliport_name) ~ flight_end_heliport_name, 
      flight_end_bbox_edge == "S" ~ "South of NYC (Unknown Location)",
      flight_end_bbox_edge == "N" ~ "North of NYC (Unknown Location)",
      flight_end_bbox_edge == "W" ~ "West of NYC (Unknown Location)",
      flight_end_bbox_edge == "E" ~ "East of NYC (Unknown Location)",
      TRUE ~ NA
      )
    ) 

# classify flights as tours, commuter/other, or nypd
flights <- flights %>%
  mutate(
    flight_type = case_when(
      (flight_start == flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "tour",
      (flight_start != flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "non-tour",
      str_detect(hex_id, "PD") ~ "NYPD",
      TRUE ~ "NA"
    ), 
    flight_path = map2_chr(flight_start, flight_end, function(x,y) paste(sort(c(x, y)), collapse = " - "))
    )

# create df of tour flights used for mapping
tour_flights <- flights %>%
  filter(flight_type == "tour")

# create df of commuter/other flights, i.e. non-tour, non-military/non-NYPD flights used for mapping
other_flights <- flights %>%
  filter(flight_type == "non-tour")

# create df of NYPD/military flights used for mapping
nypd_flights <- flights %>%
  filter(flight_type == "NYPD")

NA_fights <- flights %>%
  filter(is.na(flight_type))



