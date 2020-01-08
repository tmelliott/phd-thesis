load_vps <- function() {
    vpfile <- file.path("data", "vps.rda")
    if (file.exists(vpfile)) {
        load("data/vps.rda")
        return(vps)
    }

    # load protobuf:
    suppressPackageStartupMessages(library(RProtoBuf))
    readProtoFiles("../../data/gtfs-realtime.proto")

    # use files in ../../data/archive/vehicle_locations_*.pb
    vfiles <- list.files(
        file.path("..", "..", "data", "archive"),
        pattern = "vehicle_locations_.+\\.pb",
        full.names = TRUE
    )
    vps <- do.call(bind_rows,
        pbapply::pblapply(vfiles,
            function(f) {
                feed <- read(transit_realtime.FeedMessage, f)
                do.call(bind_rows,
                    lapply(feed$entity,
                        function(x) {
                            if (!x$has('vehicle') ||
                                !x$vehicle$has('vehicle') ||
                                !x$vehicle$has('trip') ||
                                !x$vehicle$has('position'))
                                return(NULL)
                            tibble(
                                vehicle_id = x$vehicle$vehicle$id,
                                timestamp = x$vehicle$timestamp,
                                trip_id = x$vehicle$trip$trip_id,
                                route_id = x$vehicle$trip$route_id,
                                lon = x$vehicle$position$longitude,
                                lat = x$vehicle$position$latitude
                            )
                        }
                    )
                )
            }
        )
    )

    vps <- vps %>% distinct()
    save(vps, file = vpfile)

    vps
}

vps <- load_vps()
