source("scripts/load_nw_data.R")

suppressPackageStartupMessages({
    library(tidybayes)
    library(RSQLite)
    library(chron)
})

if (!file.exists("data/data_week0.rda")) {
    dates_week0 <- list.files("../../data", "archive_.+.zip", full.names = TRUE)[1:7]
    con <- dbConnect(SQLite(), db)
    data_week0 <- lapply(dates_week0, function(zip_archive) {
        if (grepl("2019-08-13", zip_archive))
            return(tt_all)
        d <- tools::file_path_sans_ext(basename(zip_archive))
        unzip(zip_archive, exdir = d)
        on.exit(unlink(d, recursive = TRUE, force = TRUE))
        return(data_from_file(paste0(d, ".rda"), con, d))
    }) %>% bind_rows %>%
        mutate(
            timestamp = arrival_time,
            date = format(timestamp, "%Y-%m-%d"),
            dow = format(timestamp, "%A") %>%
                factor(levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                  "Friday", "Saturday", "Sunday")),
            time = as.POSIXct(paste("2019-08-12", format(timestamp, "%H:%M:%S")))
        )
    dbDisconnect(con)

    save("data_week0", file = "data/data_week0.rda")
} else {
    load("data/data_week0.rda")
}


if (!file.exists("data/data_week1.rda")) {
    dates_week1 <- list.files("../../data", "archive_.+.zip", full.names = TRUE)[8:14]
    con <- dbConnect(SQLite(), db)
    data_week1 <- lapply(dates_week1, function(zip_archive) {
        if (grepl("2019-08-20", zip_archive))
            return(tt_future)
        d <- tools::file_path_sans_ext(basename(zip_archive))
        unzip(zip_archive, exdir = d)
        on.exit(unlink(d, recursive = TRUE, force = TRUE))
        return(data_from_file(paste0(d, ".rda"), con, d))
    }) %>% bind_rows %>%
        mutate(
            timestamp = arrival_time,
            date = format(timestamp, "%Y-%m-%d"),
            dow = format(timestamp, "%A") %>%
                factor(levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                  "Friday", "Saturday", "Sunday")),
            time = as.POSIXct(paste("2019-08-12", format(timestamp, "%H:%M:%S")))
        )
    dbDisconnect(con)

    save("data_week1", file = "data/data_week1.rda")
} else {
    load("data/data_week1.rda")
}
