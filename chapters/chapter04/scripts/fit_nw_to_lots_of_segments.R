source("scripts/load_nw_data.R")

tids <- tt_all %>%
    filter(segment_id %in% sids) %>%
    pull(trip_id) %>% unique
segdat_all <- tt_all %>%
    filter(trip_id %in% tids) %>%
    mutate(timestamp = departure_time, error = 3) %>%
    arrange(segment_id, timestamp) %>%
    select(segment_id, timestamp, travel_time, error, length) %>%
    mutate(
        timestamp = as.POSIXct(
            paste0(
                format(timestamp, "%Y-%m-%d %H:%M:"),
                30 * as.integer(format(timestamp, "%S")) %/% 30
            )
        ),
        l = as.integer(as.factor(segment_id)),
        t = as.integer(timestamp)
    ) %>%
    group_by(l) %>%
    do(
        (.) %>% mutate(
            # t0 = as.integer(t == min(t)),
            c = c(1, diff(t) > 0)
        )
    ) %>% ungroup() %>%
    mutate(
        c = cumsum(c)
    ) %>%
    group_by(l) %>%
    do(
        (.) %>% mutate(
            travel_time_centered = travel_time - mean(travel_time),
            n_in_seg = length(travel_time)
        )
    ) %>% ungroup()

## only segments with enough data
# library(ggplot2)
# ggplot(segdat_all, aes(timestamp, travel_time_centered)) +
#     geom_point(aes(colour = n_in_seg > 30)) +
#     facet_wrap(~l, scales = "free_y")

jdata_all <-
    list(
        b = segdat_all$travel_time_centered,
        e = segdat_all$error,
        # identify index of the BETAs
        ell = segdat_all$l,
        c = segdat_all$c,
        c0 = tapply(segdat_all$c, segdat_all$l, min) %>% as.integer,
        c1 = tapply(segdat_all$c, segdat_all$l, min) %>% as.integer + 1,
        cJ = tapply(segdat_all$c, segdat_all$l, max) %>% as.integer,
        delta = do.call(c, tapply(segdat_all$t, segdat_all$l, function(tt) {
                c(0, diff(unique(tt)))
            })) %>% as.integer,
        L = length(unique(segdat_all$segment_id)),
        N = nrow(segdat_all),
        mu = tapply(segdat_all$travel_time, segdat_all$l, median) %>% as.numeric,
        long_segs = segdat_all %>% filter(n_in_seg > 30) %>% pull(l) %>% unique,
        short_segs = segdat_all %>% filter(n_in_seg <= 30) %>% pull(l) %>% unique
    )
jdata_all_t <- do.call(c, tapply(segdat_all$timestamp, segdat_all$l, unique))
names(jdata_all_t) <- NULL


##################### Fit the model
library(rjags)
library(tidybayes)

# source("load_week_data.R")
# data_week0 %>% group_by(segment_id) %>%
#     summarize(tt_mean = mean(travel_time), tt_var = var(travel_time))

jm_all_file <- "data/jm_all_samples.rda"
if (!file.exists(jm_all_file)) {
    jm_all <-
        jags.model(
            "models/nw_hier_model.jags",
            # quiet = !interactive(),
            data = jdata_all,
            n.chains = 4,
            n.adapt = 100000
        )

    jm_all_samples <-
        coda.samples(jm_all,
            variable.names = c("phi", "q", "theta", "sig_phi", "mu_q", "sig_q"),
            n.iter = 10000,
            thin = 10
        )
    save(jm_all_samples, file = jm_all_file)
} else {
    load(jm_all_file)
}

segtt <- segdat_all %>%
    group_by(segment_id) %>%
    summarize(
        tt = mean(travel_time),
        tt_var = var(travel_time),
        n = n()
    )

var_lm <- lm(sqrt(tt_var) ~ tt, data = segtt)
# with(segtt, plot(tt, sqrt(tt_var)))

segtt <- segtt %>%
    mutate(
        tt_var = ifelse(
            is.na(.data$tt_var),
            predict(var_lm, newdata = segtt),
            .data$tt_var
        ),
        tt_se = sqrt(tt_var / n)
    )
# with(segtt, plot(tt, tt_se))

## Write results to database
con <- dbConnect(SQLite(), "~/Documents/uni/transitr/at_gtfs.db")
# dbReadTable(con, "segment_parameters")
qphi <- jm_all_samples %>%
    spread_draws(phi[l], q[l]) %>%
    group_by(l) %>%
    summarize(phi = mean(phi), q = mean(q)) %>%
    left_join(
        segdat_all %>% group_by(segment_id) %>% summarize(l = min(l)),
        by = "l"
    ) %>%
    select(segment_id, q, phi)
segpars <- left_join(qphi, segtt, by = "segment_id")
dbWriteTable(con, "segment_parameters", segpars, overwrite = TRUE)
dbDisconnect(con)

jm_all_samples %>% spread_draws(theta[i]) %>% mean_qi

# library(coda)
# gelman.diag(jm_all_samples)

# library(ggplot2)
# ggplot(segdat_all, aes(timestamp, travel_time_centered)) +
#     geom_point(aes(colour = n_in_seg > 30)) +
#     facet_wrap(~l, scales = "free_y")
