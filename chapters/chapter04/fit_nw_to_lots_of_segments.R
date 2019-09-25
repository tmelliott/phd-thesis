source("load_nw_data.R")

tids <- tt_all %>%
    filter(segment_id %in% sids[1]) %>%
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
    )


jdata_all <-
    list(
        b = segdat_all$travel_time,
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
        # mu = (tapply(segdat$length, segdat$l, min) / 30) %>% as.numeric
        mu = tapply(segdat_all$travel_time, segdat_all$l, median) %>% as.numeric
    )
jdata_all_t <- do.call(c, tapply(segdat_all$timestamp, segdat_all$l, unique))
names(jdata_all_t) <- NULL


##################### Fit the model
library(rjags)
library(tidybayes)

jm_all <-
    jags.model(
        "nw_hier_model.jags",
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

