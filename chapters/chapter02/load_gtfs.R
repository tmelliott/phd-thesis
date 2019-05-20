library(RSQLite)
url <- "https://cdn01.at.govt.nz/data/gtfs.zip"
db <- "at_gtfs.sqlite"
outdated <- function(url) {
    date <- gsub("Last-Modified: |\r", "", system(sprintf("curl -sI %s | grep -i Last-Modified", url), intern = TRUE), ignore.case = TRUE)
    last.mod <- as.POSIXct(date, tz = "GMT", format = "%a, %d %b %Y %H:%M:%S GMT")

    cur.mod <- ".gtfs_date"
    if (!file.exists(cur.mod)) {
        writeLines(as.character(last.mod), cur.mod)
        return(TRUE)
    }
    prev.mod <- as.POSIXct(readLines(cur.mod), tz = "GMT")
    if (prev.mod < last.mod) {
        writeLines(as.character(last.mod), cur.mod)
        return(TRUE)
    }
    FALSE
}
atzip <- "at_gtfs.zip"
if (!file.exists(atzip))
    download.file(url, atzip)
if (!file.exists(db) || outdated(url)) {
    if (file.exists(db)) unlink(db)
    nw <- transitr::create_gtfs(atzip, db = db)
}
