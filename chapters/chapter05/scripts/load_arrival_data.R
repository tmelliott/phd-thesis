curd <- setwd("../../data")
source("load_data.R")
setwd(curd)


datfile <- "data/arrival_data.rda"
if (!file.exists(datfile)) {
    td <- tempdir()
    unzip("../chapter04/data/archive.zip", exdir = td)
    tufiles <- list.files(td, patter = "trip_updates", full.names = TRUE)
    transitr:::processEtas(tufiles, "data/arrivaldata.csv", db)

    library(magrittr)
    dat <-
        readr::read_csv(
            "data/arrivaldata.csv",
            col_names = c(
                "trip_id", "route_id", "vehicle_id", "timestamp",
                "stop_sequence", "current_delay",
                "arrival_time", "scheduled_arrival",
                "lower", "upper", "type"
            ),
            col_types = "ccciiiiiiif"
        ) %>%
        dplyr::select(
            trip_id, route_id, timestamp, stop_sequence,
            scheduled_arrival, current_delay, type
        )

    save(dat, file = datfile)
    unlink(td, TRUE, TRUE)
    unlink("data/arrivaldata.csv")
} else {
    load(datfile)
}
