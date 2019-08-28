wd <- getwd()
library(RSQLite)

if (!dir.exists("archive")) {
    source("fetch_data.R")
}
archive_dir <- file.path(wd, "archive")

db <- file.path(wd, "at_gtfs.sqlite")
atzip <- file.path(wd, "at_gtfs.zip")
if (!file.exists(db)) {
    nw <- transitr::create_gtfs(atzip, db = db)
    transitr::construct(nw)
}
nw <- transitr::load_gtfs(db)

