## load trip info
suppressPackageStartupMessages({
    library(magrittr)
    library(RSQLite)
    library(dplyr)
    library(dbplyr)
    library(tidyr)
    library(RProtoBuf)
})
readProtoFiles("../chapter04/gtfs-realtime.proto")

stop_delays_file <- "stop_delays.rda"
if (!file.exists(stop_delays_file)) {
    tmp <- tempfile(fileext = ".csv")
    files <- list.files("../../data", "archive_.+.zip", full.names = TRUE)

    cat("trip_id,vehicle_id,stop_id,stop_sequence,type,time,delay\n", file = tmp)
    d <- tempdir()
    lapply(files[-1], function(zf) {
        cat("\n", zf, "\n")
        allfiles <- unzip(zf, list = TRUE)$Name
        tufiles <- allfiles[grepl("^trip_updates", allfiles)]
        pbapply::pblapply(tufiles, function(f) {
            file <- unzip(zf, files = f, exdir = d)
            feed <- read(transit_realtime.FeedMessage, file)
            lapply(feed$entity, function(e) {
                stu <- e$trip_update$stop_time_update[[1]]
                cat(
                    e$trip_update$trip$trip_id,
                    e$trip_update$vehicle$id,
                    stu$stop_id,
                    stu$stop_sequence,
                    ifelse(stu$has("arrival"), "arrival", "departure"),
                    ifelse(stu$has("arrival"),
                        stu$arrival$time, stu$departure$time),
                    ifelse(stu$has("arrival"),
                        stu$arrival$delay, stu$departure$delay),
                    file = tmp, sep = ",", append = TRUE
                )
                cat("\n", file = tmp, append = TRUE)
                invisible(NULL)
            })
            unlink(file)
            invisible(NULL)
        })
        invisible(NULL)
    })

    stop_delays <- readr::read_csv(tmp, col_types = "")
    unlink(tmp)
    save(stop_delays, file = stop_delays_file)
} else {
    load(stop_delays_file)
}

# dwell times
if (file.exists("dwell_times.rda")) {
    load("dwell_times.rda")
} else {
    dwell_times <- stop_delays %>%
        group_by(trip_id, vehicle_id, stop_id, type) %>%
        summarize(
            delay = min(delay)
        ) %>%
        ungroup() %>%
        spread(
            key = type, value = delay
        ) %>%
        mutate(dwell = departure - arrival) %>%
        # between 0 and 5 min
        filter(between(dwell, 3, 5*60)) %>%
        mutate(stop_id = stringr::str_replace(stop_id, "-.+", "")) %>%
        group_by(stop_id) %>%
        summarize(
            avg = mean(dwell),
            sd = sd(dwell),
            n = n(),
            se = sd / sqrt(n),
            min = min(dwell),
            max = max(dwell),
            q5 = quantile(dwell, 0.05),
            q25 = quantile(dwell, 0.25),
            q50 = quantile(dwell, 0.5),
            q75 = quantile(dwell, 0.75),
            q95 = quantile(dwell, 0.95)
        )

    save(dwell_times, file = "dwell_times.rda")
}

library(RSQLite)
con <- dbConnect(SQLite(), "../../../transitr/at_gtfs.db")
dbWriteTable(con, "dwell_times", dwell_times, overwrite = TRUE)
dbDisconnect(con)


## clean stop delays (by day/trip)
if (file.exists("delays.rda")) {
    load("delays.rda")
} else {
    delays <- stop_delays %>%
        mutate(
            time = as.POSIXct(time, origin = "1970-01-01"),
            date = format(time, "%Y-%m-%d")
        ) %>%
        unique() %>%
        group_by(date, trip_id) %>%
        group_modify(~{
            # .y <- tibble(date = "2019-08-12", trip_id = delays$trip_id[15])
            # .x <- delays %>% filter(date == .y$date & trip_id == .y$trip_id)
            vs <- table(.x$vehicle_id)
            x <- .x %>% filter(vehicle_id == names(vs)[which.max(vs)]) %>%
                arrange(time, type) %>% unique() %>%
                mutate(
                    z = cumsum(c(1, diff(stop_sequence) < 0))
                )
            if (all(table(x$z) == 1)) return(x %>% filter(FALSE))
            x %>%
                filter(z == which.max(table(z))) %>%
                select(-z) %>%
                group_by(stop_sequence) %>%
                summarize(
                    stop_id = first(stop_id),
                    time = first(time),
                    delay = first(delay)
                )
        })
    save(delays, file = "delays.rda")
}

# library(ggplot2)
# ggplot(delays, aes(delay/60, stop_sequence)) +
#     geom_hex()

## explorateur
#
# merge route ID
library(RSQLite)
con <- dbConnect(SQLite(), "../../../transitr/at_gtfs.db")
trip <- dbGetQuery(con, "SELECT route_id, trip_id FROM trips")
dbDisconnect(con)


delays2 <- delays %>%
    left_join(trip, by = "trip_id") %>%
    ungroup() %>%
    mutate(
        stop_id = stringr::str_replace(stop_id, "-.+", ""),
        trip_id = stringr::str_replace(trip_id, "-.+", ""),
        route_id = stringr::str_replace(route_id, "-.+", "")
    ) %>%
    filter(between(delay, -30*60, 2*60*60))

smry <- delays2 %>%
    group_by(trip_id, stop_id) %>%
    summarize(
        stop_sequence = first(stop_sequence),
        route_id = first(route_id),
        avg = mean(delay),
        sd = sd(delay),
        n = n(),
        se = sd / sqrt(n),
        min = min(delay),
        max = max(delay),
        q5 = quantile(delay, 0.05),
        q25 = quantile(delay, 0.25),
        q50 = quantile(delay, 0.5),
        q75 = quantile(delay, 0.75),
        q95 = quantile(delay, 0.95)
    )

smry2 <- delays2 %>%
    filter(between(delay, -30*60, 2*60*60)) %>%
    group_by(route_id, stop_id) %>%
    summarize(
        stop_sequence = first(stop_sequence),
        avg = mean(delay),
        sd = sd(delay),
        n = n(),
        se = sd / sqrt(n),
        min = min(delay),
        max = max(delay),
        q5 = quantile(delay, 0.05),
        q25 = quantile(delay, 0.25),
        q50 = quantile(delay, 0.5),
        q75 = quantile(delay, 0.75),
        q95 = quantile(delay, 0.95)
    )

smry3 <- delays2 %>%
    filter(between(delay, -30*60, 2*60*60)) %>%
    group_by(stop_sequence) %>%
    summarize(
        avg = mean(delay),
        sd = sd(delay),
        n = n(),
        se = sd / sqrt(n),
        q50 = quantile(delay, 0.5)
    )

smry2 <- smry2 %>% 
    left_join(
        smry3 %>% select(stop_sequence, avg, sd, n, se, q50) %>% 
            rename(avg2 = avg, sd2 = sd, n2 = n, se2 = se, q502 = q50),
        by = "stop_sequence"
    ) %>%
    mutate(
        avg = ifelse(n < 10, avg2, avg),
        sd = ifelse(n < 10, sd2, sd),
        se = ifelse(n < 10, se2, se),
        q50 = ifelse(n < 10, q502, q50),
        n = ifelse(n < 10, n2, n)
    ) %>%
    select(-avg2, -sd2, -n2, -se2, -q502)

final_smry <- smry %>% ungroup() %>%
    left_join(
        smry2 %>% ungroup() %>% select(route_id, stop_sequence, avg, sd, n, se, q50) %>% 
            rename(avg2 = avg, sd2 = sd, n2 = n, se2 = se, q502 = q50),
        by = c("route_id", "stop_sequence")
    ) %>%
    mutate(
        avg = ifelse(n < 10, avg2, avg),
        sd = ifelse(n < 10, sd2, sd),
        se = ifelse(n < 10, se2, se),
        q50 = ifelse(n < 10, q502, q50),
        n = ifelse(n < 10, n2, n)
    ) %>%
    select(-avg2, -sd2, -n2, -se2, -q502)


library(RSQLite)
con <- dbConnect(SQLite(), "../../../transitr/at_gtfs.db")
dbWriteTable(con, "stop_delays", final_smry, overwrite = TRUE)
dbDisconnect(con)
