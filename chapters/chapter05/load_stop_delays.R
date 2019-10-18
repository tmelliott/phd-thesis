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
smry <- delays %>%
    filter(between(delay, -30*60, 2*60*60)) %>%
    group_by(trip_id, stop_id) %>%
    summarize(
        stop_sequence = first(stop_sequence),
        avg_delay = mean(delay),
        sd_delay = sqrt(var(delay)),
        n = n()
    )

smry2 <- delays %>%
    filter(between(delay, -30*60, 2*60*60)) %>%
    group_by(stop_sequence) %>%
    summarize(
        avg_delay = mean(delay),
        sd_delay = sqrt(var(delay)),
        n = n()
    )

smry <- smry %>%
    left_join(
        smry2 %>% select(stop_sequence, sd_delay) %>% rename(err = sd_delay),
        by = "stop_sequence"
    ) %>%
    mutate(
        sd_delay = ifelse(is.na(sd_delay), err, sd_delay)
    ) %>%
    select(-err)

library(RSQLite)
con <- dbConnect(SQLite(), "../../../transitr/at_gtfs.db")
dbWriteTable(con, "stop_delays", smry, overwrite = TRUE)
dbDisconnect(con)
