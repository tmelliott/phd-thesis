if (!file.exists("archive.zip")) {
    source("fetch_data.R")
}

db <- "at_gtfs.sqlite"
atzip <- "at_gtfs.zip"
if (!file.exists(db)) {
    nw <- transitr::create_gtfs(atzip, db = db)
    transitr::construct(nw)
}

nw <- transitr::load_gtfs(db)
