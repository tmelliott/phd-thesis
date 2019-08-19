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

seg_table <-
    rbind(
        c(7151, 7149),
        c(8413, 8411),
        c(7036, 4063),
        c(7324, 7326),
        c(7147, 7145),
        c(3184, 3186),
        c(4674, 4672),
        c(5865, 5867),
        c(8118, 8120),
        c(6220, 6779),
        c(7189, 7187)
    )
mode(seg_table) <- "character"

unlink("symonds_tt.rda")
file <- "all_tt.rda"
if (!file.exists(file)) {
    files <- unzip("archive.zip", list = TRUE)
    tufiles <- files %>%
        filter(stringr::str_detect(Name, "^trip")) %>%
        pull(Name)
    match_str <- paste("^", seg_table, "-", sep = "", collapse = "|")
    # get DEPARTURES from stop_ids[1] and ARRIVALS at stop_ids[2]
    tus <- lapply(tufiles, function(f) {
        file <- unzip("archive.zip", files = f)
        feed <- read(transit_realtime.FeedMessage, f)
        unlink(file)
        # need vehicle, trip, arr/dep time
        lapply(feed$entity, function(e) {
            stu <- e$trip_update$stop_time_update[[1]]
            if (!grepl(match_str, stu$stop_id)) return(NULL)
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

    tt_all <- tus %>%
        gather(key = "type", value = "time",
            arrival_time, departure_time
        ) %>%
        filter(
            (type == "departure_time" &
                sapply(stop_id, function(x) any(stringr::str_detect(x, seg_table[,1])))) |
            (type == "arrival_time" &
                sapply(stop_id, function(x) any(stringr::str_detect(x, seg_table[,2]))))
        ) %>%
        select(vehicle_id, trip_id, stop_id, type, time) %>%
        mutate(stop_id = gsub("-.+", "", stop_id)) %>%
        filter(!is.na(time)) %>%
        group_by(vehicle_id, trip_id, stop_id, type) %>%
        summarize(time = first(time)) %>%
        ungroup() %>%
        mutate(stop_id =
            ifelse(type == "arrival_time",
                sapply(stop_id, function(x) seg_table[seg_table[, 2] == x, 1]) %>%
                    as.character(),
                stop_id
            )
        ) %>%
        spread(key = "type", value = "time") %>%
        mutate(travel_time = arrival_time - departure_time) %>%
        filter(!is.na(travel_time)) %>%
        mutate(
            arrival_time = as.POSIXct(arrival_time, origin = "1970-01-01"),
            departure_time = as.POSIXct(departure_time, origin = "1970-01-01")
        )

    save(tt_all, file = file)
} else {
    load(file)
}


# library(ggplot2)
# ggplot(tts, aes(arrival_time, travel_time/60)) + geom_point() +
#     scale_x_datetime()

tts <- tt_all %>% filter(stop_id == seg_table[1, 1])
t30 <- paste(sep = ":",
    format(tts$arrival_time,
        "%Y-%m-%d %H"),
    format(tts$arrival_time, "%M") %>%
        as.integer %>% `%/%`(5) %>% `*`(5) %>%
        stringr::str_pad(2, pad = "0")
) %>% as.POSIXct
