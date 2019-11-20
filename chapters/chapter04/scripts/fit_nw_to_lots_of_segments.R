source("scripts/load_nw_data.R")
load("data/data_week0.rda")




# First KF to each day for sampled values of beta0, q, and phi.
# Returns the likelihood p(b, B, beta | beta0, P0, q, phi, [e])
KF <- function(t, b, e, beta0, P0, q, phi, value.only = TRUE, draw = FALSE) {
    ts <- sort(unique(t))
    t <- as.factor(t)
    ti <- as.integer(as.factor(t))
    t <- as.integer(levels(t))

    I <- 1.0 / (e^2 + phi)
    i <- b / (e^2 + phi)
    Z <- as.numeric(tapply(I, ti, sum))
    z <- as.numeric(tapply(i, ti, sum))

    delta <- c(0, diff(t))
    beta <- numeric(length(t))
    P <- numeric(length(t))
    beta[1] <- beta0
    P[1] <- P0

    for (k in seq_along(t)[-1]) {
        # predict:
        beta[k] <- beta[k - 1]
        P[k] <- P[k - 1] + (delta[k] * q)^2.0
        # update:
        U <- 1.0 / P[k]
        u <- beta[k] / P[k]
        U <- U + Z[k]
        u <- u + z[k]
        beta[k] <- u / U
        P[k] <- 1.0 / U
    }

    ret <- list(
        fit = tibble(
            t = ts,
            beta = beta,
            P = P
        ),
        nllh =
            - sum(dnorm(b, beta[ti], sqrt(P[ti]) + phi, log = TRUE)) -
            sum(dnorm(beta[-1], beta[-length(beta)], delta[-1] * q, log = TRUE))
    )

    if (draw) {
        dx <- tibble(x = ts[ti], y = b)
        p <- ggplot() +
            geom_point(aes(x, y), data = dx) +
            geom_path(aes(t, beta), data = ret$fit) +
            geom_path(aes(t, qnorm(0.025, beta, sqrt(P))),
                data = ret$fit, lty = 2) +
            geom_path(aes(t, qnorm(0.975, beta, sqrt(P))),
                data = ret$fit, lty = 2)

        dev.hold()
        print(p)
        dev.flush(dev.flush())
    }

    if (value.only) return(ret$nllh)
    ret
}


tday1 <- tts %>% filter(date == "2019-08-13") %>% arrange(time)
k1 <- KF(tday1$time, tday1$speed, tday1$error,
    beta0 = 8.0, P0 = 3.0, q = 0.001, phi = 1.5,
    value.only = FALSE
)

ggplot(tday1, aes(time, speed)) +
    geom_point() +
    geom_path(aes(t, beta), data = k1$fit) +
    geom_path(aes(t, qnorm(0.025, beta, sqrt(P))), data = k1$fit, lty = 2) +
    geom_path(aes(t, qnorm(0.975, beta, sqrt(P))), data = k1$fit, lty = 2)



# q <- seq(0.005, 0.05, length = 1000L)
# kk <- numeric(length(q))
# for (i in seq_along(q)) {
#     kk[i] <- KF(tday1$time, tday1$speed, tday1$error,
#         beta0 = 8.0, P0 = 3.0, q = q[i], phi = 1.5)$sum_log_likelihood
# }
# plot(q, kk, type = "l")

# phi <- seq(0.01, 0.3, length = 1000L)
# kk <- numeric(length(phi))
# for (i in seq_along(phi)) {
#     kk[i] <- KF(tday1$time, tday1$speed, tday1$error,
#         beta0 = 8.0, P0 = 3.0, q = 0.02, phi = phi[i])$sum_log_likelihood
# }
# plot(phi, kk, type = "l")

# beta0 <- seq(10, 15, length = 1000L)
# kk <- numeric(length(beta0))
# for (i in seq_along(beta0)) {
#     kk[i] <- KF(tday1$time, tday1$speed, tday1$error,
#         beta0 = beta0[i], P0 = 3.0, q = 0.02, phi = 0.1)$sum_log_likelihood
# }
# plot(beta0, kk, type = "l")

# P0 <- seq(1, 100, length = 1000L)
# kk <- numeric(length(P0))
# for (i in seq_along(P0)) {
#     kk[i] <- KF(tday1$time, tday1$speed, tday1$error,
#         beta0 = 12, P0 = P0[i], q = 0.02, phi = 0.1)$sum_log_likelihood
# }
# plot(P0, kk, type = "l")

.KF <- function(theta, dat, ...) {
    q <- theta[1]
    phi <- theta[2]
    beta0 <- theta[3]
    P0 <- theta[4]
    KF(dat$time, dat$speed, dat$error, beta0, P0, q, phi, ...)
}

fit1 <- optim(c(0.01, 0.1, 10, 10),
    .KF,
    dat = tday1,
    method = "L-BFGS-B",
    lower = c(0, 0, 0, 0)
)
fit1.out <- .KF(fit1$par, tday1, value.only = FALSE)
ggplot(tday1, aes(time, speed)) +
    geom_point() +
    geom_path(aes(t, beta), data = k1$fit) +
    geom_path(aes(t, qnorm(0.025, beta, sqrt(P))), data = k1$fit, lty = 2) +
    geom_path(aes(t, qnorm(0.975, beta, sqrt(P))), data = k1$fit, lty = 2)


## Fit model to each segment independently ...
all_fits <- sapply(unique(data_week0$segment_id), function(sid) {
    # sid <- unique(data_week0$segment_id)[1]
    tts <- data_week0 %>% filter(segment_id == sid) %>%
        mutate(timestamp = departure_time, error = 0.8) %>%
        arrange(timestamp) %>%
        select(segment_id, timestamp, travel_time, error, length) %>%
        mutate(
            time = as.integer(format(timestamp, "%H")) * 60 * 60 +
                as.integer(format(timestamp, "%M")) * 60 +
                30 * (as.integer(format(timestamp, "%S")) %/% 30),
            speed = length / travel_time,
            date = as.factor(format(timestamp, "%Y-%m-%d"))
        )

    ggplot(tts, aes(time, speed)) + geom_point(aes(colour=date))

    Vmax <- (round(
        quantile(tts$speed, ifelse(nrow(tts) > 30, 0.95, 0.99)) * 3.6 / 10
    ) * 10 + 10) / 3.6

    jdatai <- list(
        b = tts$speed,
        e = tts$error,
        # identify index of the BETAs
        c0 = tapply(tts$t, tts$date, min) %>% as.integer,
        c1 = tapply(tts$t, tts$date, min) %>% as.integer + 1,
        cJ = tapply(tts$t, tts$date, max) %>% as.integer,
        t = tts$t,
        delta = do.call(c,
            tapply(tts$time, tts$date,
                function(tt) {
                    c(0, diff(unique(tt)))
                }
            )) %>% as.integer,
        D = length(levels(tts$date)),
        N = nrow(tts),
        mu = Vmax[[1]]
    )


    library(rjags)
    library(tidybayes)

    jm_all_file <- glue::glue("data/jm_all_samples{sid}.rda")
    if (!file.exists(jm_all_file)) {
        jm_all <-
            jags.model(
                "models/nw_model2.jags",
                # quiet = !interactive(),
                data = jdatai,
                n.chains = 4,
                n.adapt = 10000
            )

        jm_all_samples <-
            coda.samples(jm_all,
                variable.names = c("phi", "q"),
                n.iter = 10000,
                thin = 10
            )
        jm_betas <- coda.samples(jm_all,
            variable.names = c("beta"),
            n.iter = 10000,
            thin = 10
        )
        save(jm_all_samples, file = jm_all_file)
    } else {
        load(jm_all_file)
    }

    jmss <- jm_all_samples %>% spread_draws(phi, q)
    egg::ggarrange(
        ggplot(jmss, aes(.iteration, phi, group = .chain)) + geom_path(),
        ggplot(jmss, aes(.iteration, q, group = .chain)) + geom_path(),
        ncol = 1

    gelman.diag(jm_betas)
})


# 1: 50 random segments
# 2: 500 random segments, but using posterior as prior - does it change?
for (i in 1:2) {

    tids <- tt_all %>%
        filter(segment_id %in% sids) %>%
        pull(trip_id) %>% unique()

    if (i == 1) {
        segii <- tt_all %>% filter(trip_id %in% tids) %>%
            group_by(segment_id) %>%
            summarize(n = n()) %>%
            filter(n > 50) %>%
            pull(segment_id) %>% sample(20)
    } else {
        segii <- tt_all %>% filter(trip_id %in% tids) %>%
            group_by(segment_id) %>%
            summarize(n = n()) %>%
            filter(n > 50) %>%
            pull(segment_id) %>% sample(100)
    }

    segdat_all <- tt_all %>%
        filter(trip_id %in% tids) %>%
        filter(segment_id %in% segii) %>%
        mutate(timestamp = departure_time, error = 0.8) %>%
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
            t = as.integer(timestamp),
            speed = length / travel_time
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
                n_in_seg = length(travel_time)
            )
        ) %>% ungroup()

## only segments with enough data
# library(ggplot2)
# ggplot(segdat_all, aes(timestamp, travel_time_centered)) +
#     geom_point(aes(colour = n_in_seg > 30)) +
#     facet_wrap(~l, scales = "free_y")

    Vmax <- segdat_all %>% group_by(segment_id) %>%
        summarize(
            n = n(),
            Vmax = quantile(speed, ifelse(n < 50, 1, 0.95)) * 3.6,
            Vmax = round(pmax(50, pmin(100, Vmax)) / 10) * 10 + 10
        )

    jdata_all <-
        list(
            b = segdat_all$speed,
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
            mu = Vmax$Vmax
            # long_segs = segdat_all %>% filter(n_in_seg > 30) %>% pull(l) %>% unique,
            # short_segs = segdat_all %>% filter(n_in_seg <= 30) %>% pull(l) %>% unique
        )
    jdata_all_t <-
        do.call(c, tapply(segdat_all$timestamp, segdat_all$l, unique))
    names(jdata_all_t) <- NULL


    ##################### Fit the model
    library(rjags)
    library(tidybayes)

    # source("load_week_data.R")
    # data_week0 %>% group_by(segment_id) %>%
    #     summarize(tt_mean = mean(travel_time), tt_var = var(travel_time))

    jm_all_file <- glue::glue("data/jm_all_samples{i}.rda")
    if (!file.exists(jm_all_file)) {
        jm_all <-
            jags.model(
                "models/nw_hier_model.jags",
                # quiet = !interactive(),
                data = jdata_all,
                n.chains = 4,
                n.adapt = 10000
            )

        jm_all_samples <-
            coda.samples(jm_all,
                variable.names = c("phi", "mu_phi", "sig_phi", "q"),
                n.iter = 10000,
                thin = 10
            )
        save(jm_all_samples, file = jm_all_file)
    } else {
        load(jm_all_file)
    }

    if (FALSE) {
        library(ggplot2)
        jmss <- jm_all_samples %>% spread_draws(mu_phi, sig_phi, q)
        egg::ggarrange(
            ggplot(jmss, aes(.iteration, mu_phi, group = .chain)) + geom_path(),
            ggplot(jmss, aes(.iteration, sig_phi, group = .chain)) + geom_path(),
            ggplot(jmss, aes(.iteration, q, group = .chain)) + geom_path(),
            ncol = 1
        )
        ggplot(jmss, aes(log(q), group = l, colour = l)) +
            geom_density()
    }
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
