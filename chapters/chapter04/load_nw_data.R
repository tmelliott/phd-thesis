suppressPackageStartupMessages({
    library(magrittr)
    library(RSQLite)
    library(dplyr)
    library(dbplyr)
    library(tidyr)
    library(RProtoBuf)
})
readProtoFiles("gtfs-realtime.proto")

curd <- setwd("../../data")
source("load_data.R")
setwd(curd)

library(dbplyr)
library(magrittr)

data_from_file <- function(file, con, archive_dir = "../../data/archive") {
    stop_nodes <- con %>% tbl("stops") %>%
        select(stop_code, node_id)
    seg_table <- con %>% tbl("road_segments") %>%
        mutate(min_travel_time = length / 30) %>%
        left_join(stop_nodes,
            by = c("node_from" = "node_id"),
            suffix = c("", "_from")
        ) %>%
        left_join(stop_nodes,
            by = c("node_to" = "node_id"),
            suffix = c("", "_to")
        ) %>%
        rename(
            stop_from = "stop_code",
            stop_to = "stop_code_to"
        ) %>%
        select(-node_from, -node_to) %>%
        collect()

    if (!file.exists(file)) {
        files <- list.files(archive_dir, full.names = TRUE)
        tufiles <- files[grepl("trip_updates", files)]
        # get DEPARTURES from stop_ids[1] and ARRIVALS at stop_ids[2]
        tus <- lapply(tufiles, function(f) {
            feed <- read(transit_realtime.FeedMessage, f)
            # need vehicle, trip, arr/dep time
            lapply(feed$entity, function(e) {
                stu <- e$trip_update$stop_time_update[[1]]
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

        tripnodes <-
            con %>% tbl("trips") %>% filter(trip_id %in% !!unique(tus$trip_id)) %>%
                left_join(con %>% tbl("shape_nodes")) %>%
                select(trip_id, node_id, node_sequence) %>%
                collect()
        tripsegs <- tripnodes %>% group_by(trip_id) %>%
            do((.) %>% (function(x) {
                x <- x %>% arrange(node_sequence)
                tibble(
                    trip_id = x$trip_id[-1],
                    node_from = x$node_id[-nrow(x)],
                    node_to = x$node_id[-1],
                    node_sequence = x$node_sequence[-nrow(x)]
                )
            })) %>%
            left_join(con %>% tbl("road_segments") %>%
                left_join(con %>% tbl("stops") %>% select(stop_id, node_id) %>%
                    rename(stop_from = stop_id),
                    by = c("node_from" = "node_id")
                ) %>%
                left_join(con %>% tbl("stops") %>% select(stop_id, node_id) %>%
                    rename(stop_to = stop_id),
                    by = c("node_to" = "node_id")
                ) %>% collect() %>%
                mutate(
                    stop_from = gsub("-.+", "", stop_from),
                    stop_to = gsub("-.+", "", stop_to)
                )
            )
        tt_all <- tus %>%
            mutate(stop_id = gsub("-.+", "", stop_id)) %>%
            inner_join(tripsegs %>% select(trip_id, stop_from, road_segment_id),
                by = c("trip_id", "stop_id" = "stop_from")) %>%
            rename(segment_from = road_segment_id) %>%
            inner_join(tripsegs %>% select(trip_id, stop_to, road_segment_id),
                by = c("trip_id", "stop_id" = "stop_to")) %>%
            rename(segment_to = road_segment_id) %>%
            mutate(segment_id = ifelse(is.na(arrival_time), segment_from, segment_to)) %>%
            select(vehicle_id, trip_id, stop_id, arrival_time, departure_time, segment_id) %>%
            gather(key = "type", value = "time",
                arrival_time, departure_time
            ) %>%
            mutate(
                keep_from = stop_id %in% seg_table$stop_from & type == "departure_time",
                keep_to = stop_id %in% seg_table$stop_to & type == "arrival_time"
            ) %>%
            filter(keep_from | keep_to) %>%
            select(vehicle_id, trip_id, stop_id, segment_id, type, time) %>%
            filter(!is.na(time)) %>%
            group_by(vehicle_id, trip_id, segment_id, type) %>%
            summarize(time = first(time)) %>%
            ungroup() %>%
            spread(key = "type", value = "time") %>%
            mutate(travel_time = arrival_time - departure_time) %>%
            filter(!is.na(travel_time) & travel_time > 0) %>%
            mutate(
                arrival_time = as.POSIXct(arrival_time, origin = "1970-01-01"),
                departure_time = as.POSIXct(departure_time, origin = "1970-01-01")
            ) %>%
            left_join(con %>% tbl("road_segments") %>%
                    select("road_segment_id", "length") %>% collect(),
                by = c("segment_id" = "road_segment_id")
            )

        save(tt_all, file = file)
    } else {
        load(file)
    }

    tt_all
}

con <- dbConnect(SQLite(), db)
tt_all <- data_from_file("all_tt.rda", con)

if (!dir.exists("future_archive"))
    unzip("../../data/archive_2019-08-20.zip", exdir = "future_archive")
tt_future <- data_from_file("future_tt.rda", con, "future_archive")

# sids <- table(tt_all$segment_id) %>% sort() %>% tail(20) %>% names()
sids <- c(44, 211, 245, 49, 212, 47)

tts <- tt_all %>% filter(segment_id == sids[1])
t30 <- paste(sep = ":",
    format(tts$arrival_time,
        "%Y-%m-%d %H"),
    format(tts$arrival_time, "%M") %>%
        as.integer %>% `%/%`(5) %>% `*`(5) %>%
        stringr::str_pad(2, pad = "0")
) %>% as.POSIXct

mu <- (con %>% tbl("road_segments") %>%
    filter(road_segment_id == !!sids[1]) %>%
    collect() %>% pull("length")) / 30


data.list <- lapply(sids[-1], function(id) {
    tts <- tt_all %>% filter(segment_id == id)
    t30 <- paste(sep = ":",
        format(tts$arrival_time,
            "%Y-%m-%d %H"),
        format(tts$arrival_time, "%M") %>%
            as.integer %>% `%/%`(5) %>% `*`(5) %>%
            stringr::str_pad(2, pad = "0")
    ) %>% as.POSIXct
    attr(tts, "t30") <- t30
    attr(tts, "mu") <- (con %>% tbl("road_segments") %>%
        filter(road_segment_id == !!id) %>%
        collect() %>% pull("length")) / 30
    tts
})


segdat <- tt_all %>%
    filter(segment_id %in% sids) %>%
    mutate(timestamp = departure_time, error = 3) %>%
    arrange(segment_id, timestamp) %>%
    select(segment_id, timestamp, travel_time, error, length) %>%
    mutate(
        timestamp = as.POSIXct(
            paste0(
                format(timestamp, "%Y-%m-%d %H:%M:"),
                30 * as.integer(format(timestamp, "%S")) %/% 30
            )
        ),
        l = as.integer(as.factor(segment_id)),
        t = as.integer(timestamp)
    ) %>%
    group_by(l) %>%
    do(
        (.) %>% mutate(
            # t0 = as.integer(t == min(t)),
            c = c(1, diff(t) > 0)
        )
    ) %>% ungroup() %>%
    mutate(
        c = cumsum(c)
    )


jdata <-
    list(
        b = segdat$travel_time,
        e = segdat$error,
        # identify index of the BETAs
        ell = segdat$l,
        c = segdat$c,
        c0 = tapply(segdat$c, segdat$l, min) %>% as.integer,
        c1 = tapply(segdat$c, segdat$l, min) %>% as.integer + 1,
        cJ = tapply(segdat$c, segdat$l, max) %>% as.integer,
        delta = do.call(c, tapply(segdat$t, segdat$l, function(tt) {
                c(0, diff(unique(tt)))
            })) %>% as.integer,
        L = length(unique(segdat$segment_id)),
        N = nrow(segdat),
        mu = (tapply(segdat$length, segdat$l, min) / 30) %>% as.numeric
    )
jdata_t <- do.call(c, tapply(segdat$timestamp, segdat$l, unique))
names(jdata_t) <- NULL



dbDisconnect(con)
