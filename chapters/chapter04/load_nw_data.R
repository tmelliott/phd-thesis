suppressPackageStartupMessages({
    library(magrittr)
    library(RSQLite)
    library(dplyr)
    library(dbplyr)
    library(tidyr)
    library(RProtoBuf)
})
readProtoFiles("gtfs-realtime.proto")


if (!file.exists("archive.zip")) {
    # download archive from PI
    source("../../data/fetch_data.R")
}

# stop info
db <- file.path("..", "chapter02", "at_gtfs.sqlite")
if (!file.exists(db)) {
    curd <- setwd("../chapter02")
    source("load_data.R")
    setwd(curd)
}

con <- dbConnect(SQLite(), db)
on.exit(dbDisconnect(con))

# r <- con %>% 
#     tbl("routes") %>%
#     filter(route_short_name == "NX1") %>%
#     filter(route_long_name %like% "Britomart%") %>%
#     head(1) %>% collect() %>% pull("route_id")
# t <- con %>% tbl("trips") %>%
#     filter(route_id == r) %>%
#     head(1) %>% collect() %>% pull("trip_id")
#     # times over the bridge
# stop_ids <- con %>% tbl("stops") %>%
#     filter(stop_id %like% paste(stop_nums[1]))
# %>% tbl("stop_times") %>%
#     filter(trip_id == t) %>%
#     arrange(desc(stop_sequence)) %>%
#     head(2) %>% collect() %>% pull("stop_id")
# stop_ids <- gsub("-.+", "", stop_ids)
stop_ids <- c("7151", "7149")

if (!file.exists("symonds_tt.rda")) {
    files <- unzip("archive.zip", list = TRUE)
    tufiles <- files %>% 
        filter(stringr::str_detect(Name, "^trip")) %>%
        pull(Name)
    # get DEPARTURES from stop_ids[1] and ARRIVALS at stop_ids[2]
    tus <- lapply(tufiles, function(f) {
        file <- unzip("archive.zip", files = f)
        feed <- read(transit_realtime.FeedMessage, f)
        unlink(file)
        # need vehicle, trip, arr/dep time
        lapply(feed$entity, function(e) {
            stu <- e$trip_update$stop_time_update[[1]]
            if (!grepl(paste(stop_ids, collapse = "|"), stu$stop_id))
                return(NULL)
            tibble(
                vehicle_id = e$trip_update$vehicle$id,
                trip_id = e$trip_update$trip$trip_id,
                stop_id = stu$stop_id,
                arrival_time = 
                    ifelse(stu$has("arrival"), 
                        as.integer(stu$arrival$time), NA_integer_),
                departure_time = 
                    ifelse(stu$has("departure"), 
                        as.integer(stu$departure$time), NA_integer_)
            )
        }) %>% bind_rows
    }) %>% bind_rows %>% distinct
    
    tts <- tus %>% 
        gather(key = "type", value = "time", 
            arrival_time, departure_time
        ) %>%
        filter(
            (type == "departure_time" & grepl(stop_ids[1], stop_id)) |
            (type == "arrival_time" & grepl(stop_ids[2], stop_id))
        ) %>%
        select(vehicle_id, trip_id, type, time) %>%
        filter(!is.na(time)) %>% 
        spread(key = "type", value = "time") %>% 
        mutate(travel_time = arrival_time - departure_time) %>%
        filter(!is.na(travel_time)) %>%
        mutate(arrival_time = 
            as.POSIXct(arrival_time, origin = "1970-01-01")
        )

    save(tts, file = "symonds_tt.rda")
} else {
    load("symonds_tt.rda")
}


# library(ggplot2)
# ggplot(tts, aes(arrival_time, travel_time/60)) + geom_point() +
#     scale_x_datetime()
