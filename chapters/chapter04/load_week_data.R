source("load_nw_data.R")

suppressPackageStartupMessages({
    library(tidybayes)
    library(RSQLite)
    library(chron)
})

if (!file.exists("data_week0.rda")) {
    dates_week0 <- list.files("../../data", "archive_.+.zip", full.names = TRUE)[1:7]
    con <- dbConnect(SQLite(), db)
    data_week0 <- lapply(dates_week0, function(zip_archive) {
        if (grepl("2019-08-13", zip_archive))
            return(tt_all)
        d <- tools::file_path_sans_ext(basename(zip_archive))
        unzip(zip_archive, exdir = d)
        on.exit(unlink(d))
        return(data_from_file(paste0(d, ".rda"), con, d))
    }) %>% bind_rows %>%
        mutate(
            timestamp = arrival_time,
            date = format(timestamp, "%Y-%m-%d"),
            dow = format(timestamp, "%A") %>%
                factor(levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                  "Friday", "Saturday", "Sunday")),
            time = as.POSIXct(format(timestamp, "%H:%M:%S"), format = "%H:%M:%S")
        )
    dbDisconnect(con)

    save("data_week0", file = "data_week0.rda")
} else {
    load("data_week0.rda")
}

