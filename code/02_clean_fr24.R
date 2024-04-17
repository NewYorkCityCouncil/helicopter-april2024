
source("code/00_load_dependencies.R")
source("code/01_clean_helicopters.R")

output_path <- glue("data/output")

# Monday, May 15, 2023
# csvs are not available for public use. Contact FlightRadar24 to purchase historical flight data. 
flights_df <- read_csv("data/input/flightradar24/20230515_flights.csv", col_types = cols(.default = "c"))

positions_df <- list.files(path = "data/input/flightradar24/20230515_positions",
                        pattern = "\\.csv$",
                        full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(~read_csv(., col_types = cols(.default = "c")), .id = "file_name") %>%
  mutate(
    flight_id = str_replace(str_extract(string = file_name, pattern = "[^_]+$"), ".csv", ""), 
    date = str_replace(str_extract(string = file_name, pattern = "[^/]+$"), "_[0-9]+.csv", "")
  )

merged_df <- positions_df %>%
  left_join(flights_df, by = "flight_id") %>%
  mutate(datetime = as.POSIXct(as.numeric(snapshot_id), tz = "UTC",
                               origin = "1970-01-01"))


# scraped positions
heli_position_df <- 
  merged_df %>%
  janitor::clean_names() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS) %>%
  mutate(obs_id = row_number()) %>%
  select(
    callsign, #callsign
    reg, #starts_with("aircraft")
    speed, #ground_speed
    heading, #heading
    altitude, #altitude
    #vertical_rate, 
    squawk, #squawk_code
    flight_id, #unique_id
    datetime, # snapshot_id -> datetime
    obs_id # obs_id (from mutate above)
  ) %>%
  rename(
    hex_id = reg, # aircraft_registration
    alt_ft = altitude,
    timestamp = datetime, 
    unique_id = flight_id # doesn't exist in previous
  )

clean_helis(
  heli_position_df = heli_position_df,
  heliports_path = "data/input/heliports.csv",
  output_path = "data/output",
  WRITE_TO_RULE_TESTING_DIR = F
)


