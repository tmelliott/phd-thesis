# exploring layovers

suppressPackageStartupMessages({
    library(tidyverse)
    library(RSQLite)
    library(dbplyr)
})

source("scripts/load_arrival_data.R")
tids <- unique(dat$trip_id)

con <- dbConnect(SQLite(), db)
layovers <- con %>% tbl("stop_times") %>%
    filter(
        trip_id %in% tids &
        departure_time != arrival_time
    ) %>%
    select(trip_id, stop_sequence, departure_time) %>%
    collect()
dbDisconnect(con)

layover <- dat %>%
    inner_join(layovers,
        by = c("trip_id", "stop_sequence")
    ) %>%
    filter(between(current_delay, -60*60, 60*60)) %>%
    mutate(
        driver_waits = ifelse(current_delay > -60, "yes", "no")
    )

# ggplot(layover, aes(current_delay / 60)) +
#     geom_histogram()

layover_pr <- layover %>%
    count(driver_waits) %>%
    mutate(prob = n / sum(n))
# about 70% wait

# layover %>%
#     group_by(route_id) %>%
#     summarize(pr = mean(driver_waits == "yes")) %>%
#     ggplot(aes(pr, route_id)) +
#         geom_point()
