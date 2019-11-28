source("scripts/load_nw_data.R")

library(tidyverse)
library(rjags)
library(tidybayes)
load("data/data_week0.rda")

## fit some model to the data
stab <- table(data_week0$segment_id)
sids <- names(stab[stab > 500])

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
            - sum(dnorm(b, beta[ti], sqrt(P[ti] + phi + e^2), log = TRUE)) #-
            #sum(dnorm(beta[-1], beta[-length(beta)], delta[-1] * q, log = TRUE))
    )

    if (draw) {
        dx <- tibble(x = ts[ti], y = b)
        p <- ggplot() +
            geom_point(aes(x, y), data = dx) +
            geom_path(aes(t, beta), data = ret$fit) +
            geom_path(aes(t, qnorm(0.025, beta, sqrt(P))),
                data = ret$fit, lty = 2) +
            geom_path(aes(t, qnorm(0.975, beta, sqrt(P))),
                data = ret$fit, lty = 2) +
            geom_path(aes(t, qnorm(0.025, beta, sqrt(P + phi))),
                data = ret$fit, lty = 3) +
            geom_path(aes(t, qnorm(0.975, beta, sqrt(P + phi))),
                data = ret$fit, lty = 3)

        return(p)
        dev.hold()
        print(p)
        dev.flush(dev.flush())
    }

    if (value.only) return(ret$nllh)
    ret
}

# big_results_list <- vector("list", length(sids))
# names(big_results_list) <- paste0("seg", sids)

all_ids <- unique(data_week0$segment_id)
sids <- data_week0 %>%
    group_by(segment_id) %>%
    summarize(n = n()) %>%
    ungroup () %>%
    mutate(
        phi = NA_real_,
        q = NA_real_,
        nllh = NA_real_,
        np = NA_integer_,
        phi_null = NA_real_,
        nllh_null = NA_real_,
        np_null = NA_integer_,
        speed = NA_real_,
        speed_var = NA_real_,
        max_speed = NA_real_
    )

pb <- txtProgressBar(0, length(all_ids), style = 3)
for (sid in all_ids) {
    setTxtProgressBar(pb, pb$getVal() + 1)
    # cat("\n working on segment", sid, "\n")
    # fid <- glue::glue("data/res_list_{sid}.rda")
    # if (!is.null(big_results_list[[paste0("seg", sid)]])) {
    #     seg_list <- big_results_list[[paste0("seg", sid)]]
    #     save(seg_list$estimate[1,], file = fid)
    # }
    # if (file.exists(fid)) next

    # sid <- names(sort(table(data_week0$segment_id), TRUE))[2]
    ds <- data_week0 %>% filter(segment_id == sid) %>%
        mutate(
            speed = length / travel_time,
            weekday =
                ifelse(dow %in% c("Saturday", "Sunday"),
                    "weekend", "weekday"),
            ts = timestamp - as.integer(format(timestamp, "%S")) %% 30,
            t = as.integer(format(ts, "%H")) * 3600 +
                as.integer(format(ts, "%M")) * 60 +
                as.integer(format(ts, "%S"))
        )

    vmax <- quantile(ds$speed, ifelse(nrow(ds) < 50, 0.99, 0.95)) %>% as.numeric
    vmax <- min(110, ceiling(vmax * 3.6 / 10) * 10 + 10)

    # ggplot(ds, aes(time, speed * 3.6, colour = weekday, group = date)) +
    #     geom_point() +
    #     geom_smooth(span = 0.4, se = FALSE) +
    #     geom_hline(yintercept = 50 + c(0, 10), lty = c(1, 2)) +
    #     scale_y_continuous(limits = c(0, 100))

    try({
        fits <- lapply(unique(ds$date),
            function(d) {
                dd <- ds %>% filter(date == d) %>% arrange(t)
                sm <- loess(dd$speed ~ dd$t)
                r <- resid(sm)
                ti <- as.integer(tapply(seq_len(nrow(dd)), dd$t, min))
                tt <- dd$t[ti]
                yhat <- sm$fitted[ti]
                v <- abs(diff(yhat)) / diff(tt)
                list(r = r, v = v)
            }
        )
        phi <- sqrt(var(do.call(c, map(fits, pluck, "r"))))# - 0.8^2)
        #q <- quantile(do.call(c, map(fits, pluck, "v")), 0.9)
        q <- max(do.call(c, map(fits, pluck, "v")))
        if (is.nan(q)) q <- NA

        # library(patchwork)
        pps <- lapply(unique(ds$date),
            function(d) {
                dd <- ds %>% filter(date == d) %>% arrange(t)
                # KF(dd$t, dd$speed, rep(0.8, nrow(dd)), 11.0, 5.0, q, phi, draw = TRUE)
                KF(dd$t, dd$speed, rep(0.8, nrow(dd)), 11.0, 5.0, q, phi)
            }
        )
        nllh <- sum(unlist(pps))
        sids$phi[sids$segment_id == sid] <- phi
        sids$q[sids$segment_id == sid] <- q
        sids$nllh[sids$segment_id == sid] <- nllh
        sids$np[sids$segment_id == sid] <-
            length(do.call(c, map(fits, pluck, "v"))) + 2L
    }, silent = TRUE)
    # versus
    phi_null <- sqrt(var(ds$speed))
    nllh_null <- -sum(dnorm(ds$speed, median(ds$speed), sqrt(phi_null + 0.8^2), log = TRUE))
    sids$nllh_null[sids$segment_id == sid] <- nllh_null
    sids$phi_null[sids$segment_id == sid] <- phi_null
    sids$np_null[sids$segment_id == sid] <- 2L

    sids$speed[sids$segment_id == sid] <- median(ds$speed)
    sids$speed_var[sids$segment_id == sid] <- var(ds$speed)
    sids$max_speed[sids$segment_id == sid] <- vmax / 3.6

    # print(
    #     Reduce("+",
    #         c(
    #             pps,
    #             list(
    #                 plot_layout(ncol = 1),
    #                 plot_annotation(title = glue::glue("phi={signif(phi, 3)}, q={signif(q, 2)}"))
    #             )
    #         )
    #     )
    # )

    # grid::grid.locator()
    # next
}; close(pb)

sids <- sids %>%
    mutate(
        aic = nllh + 2 * 3L,
        aic_null = nllh_null + 2 * np_null,
        model = ifelse(aic < aic_null, "KF", "linear")
    )

# sids %>%
#     ggplot(aes(aic, aic_null, colour = model, size = n)) +
#     geom_point() +
#     geom_abline() +
#     scale_x_continuous(trans = "log") +
#     scale_y_continuous(trans = "log")

sid_tbl <- sids %>%
    mutate(
        q = ifelse(model == "KF", q, 0.0),
        q = ifelse(is.na(q), 0.0, q),
        phi = ifelse(model == "KF", phi, phi_null),
        phi = ifelse(is.na(phi), phi_null, phi)
    ) %>%
    select(segment_id, q, phi, speed, speed_var, max_speed)

con <- dbConnect(SQLite(), "~/Documents/uni/transitr/at_gtfs.db")
dbWriteTable(con, "segment_parameters", sid_tbl, overwrite = TRUE)
dbDisconnect(con)




    # single day:
    {
        # ds1 <- ds %>% filter(date == "2019-08-12")
        # ggplot(ds1, aes(t, travel_time)) + geom_point()
        # ggplot(ds1, aes(t, speed * 3.6)) +
        #     geom_point() +
        #     geom_smooth(span = 0.4, se = FALSE) +
        #     geom_hline(yintercept = c(50, 60), lty = c(1, 2)) +
        #     scale_y_continuous(limits = c(0, 70))

        # segdata <-
        #     list(
        #         b = ds1$speed,
        #         t = as.integer(as.factor(ds1$t)),
        #         delta = diff(sort(unique(as.integer(ds1$t)))),
        #         N = nrow(ds1),
        #         M = length(unique(ds1$t)),
        #         Vmax = 60 / 3.6,
        #         E = 0.8
        #     )
        # dtimes <- sort(unique(ds1$t))

        # n1_fit <-
        #     jags.model(
        #         "models/quickfit.jags",
        #         data = segdata,
        #         n.chains = 4,
        #         n.adapt = 10000
        #         # quiet = TRUE
        #     )

        # ## For checking, also take some samples of
        # ## the vehicles' fitted Bs:
        # n1_B_samples.raw <-
        #     coda.samples(n1_fit,
        #         variable.names = "B",
        #         n.iter = 5000,
        #         thin = 5
        #     )
        # n1_B_samples <- n1_B_samples.raw %>%
        #     spread_draws(B[i]) %>%
        #     mutate(time = ds1$t[i])

        # ## Graph sample of Bs with observed data:
        # n1_B_samples %>%
        #     sample_draws(50) %>%
        #     ggplot(aes(time, B)) +
        #     geom_point() +
        #     geom_point(aes(t, speed),
        #         data = ds1,
        #         colour = "red"
        #     )

        # ## Sample the main model parameters
        # n1_samples.raw <-
        #     coda.samples(n1_fit,
        #         variable.names = c("beta", "phi", "q"),
        #         n.iter = 5000,
        #         thin = 5
        #     )
        # n1_samples <- n1_samples.raw %>%
        #     spread_draws(beta[t], phi, q) %>%
        #     mutate(time = dtimes[t])

        # n1_samples %>%
        #     filter(t == 1) %>%
        #     ggplot(aes(.iteration, group = .chain)) +
        #         geom_path(aes(y = phi))

        # n1_samples %>%
        #     filter(t == 1) %>%
        #     ggplot(aes(.iteration, group = .chain)) +
        #         geom_path(aes(y = q))

        # n1_samples %>%
        #     sample_draws(n = 50) %>%
        #     arrange(time) %>%
        #     ggplot(aes(time, beta, group = .draw)) +
        #         geom_path()

        # n1_estimate <- n1_samples %>%
        #     median_qi()

        # # transform from beta to travel time
        # # tt = L / V; V = Vmax / (1 + e^-eta)
        # # tt = L * (1 + e^-eta) / Vmax
        # ilogit <- function(eta) eta
        #     # segdata$L * (1 + exp(-eta)) / segdata$Vmax
        # n1_res <- n1_estimate %>%
        #     mutate(
        #         eta_lower = qnorm(0.1, beta.lower, phi),
        #         eta_upper = qnorm(0.975, beta.upper, phi),
        #         mean = ilogit(beta),
        #         mean_lower = ilogit(beta.upper),
        #         mean_upper = ilogit(beta.lower),
        #         B_lower = ilogit(eta_upper),
        #         B_upper = ilogit(eta_lower)
        #     ) %>%
        #     arrange(time)

        # n1_res %>%
        #     ggplot(aes(time, beta)) +
        #         geom_ribbon(
        #             aes(ymin = eta_lower, ymax = eta_upper),
        #             fill = "orangered",
        #             alpha = 0.5
        #         ) +
        #         geom_ribbon(
        #             aes(ymin = beta.lower, ymax = beta.upper)
        #         ) +
        #         geom_path()

        # n1_res %>%
        #     ggplot(aes(time)) +
        #     geom_ribbon(
        #         aes(ymin = B_lower, ymax = B_upper),
        #         colour = "gray", alpha = 0.25
        #     ) +
        #     geom_ribbon(aes(ymin = mean_lower, ymax = mean_upper),
        #         fill = "orangered", alpha = 0.5
        #     ) +
        #     geom_path(aes(y = mean), colour = "orangered") +
        #     geom_point(
        #         aes(t, speed),
        #         data = ds1, size = 1
        #     )


        # n2_fit <-
        #     jags.model(
        #         "models/quickfit_linear.jags",
        #         data = segdata[c("b", "N", "Vmax", "E")],
        #         n.chains = 4,
        #         n.adapt = 10000
        #         # quiet = TRUE
        #     )

        # n2_B_samples.raw <-
        #     coda.samples(n2_fit,
        #         variable.names = "B",
        #         n.iter = 5000,
        #         thin = 5
        #     )
        # n2_B_samples <- n2_B_samples.raw %>%
        #     spread_draws(B[i]) %>%
        #     mutate(time = ds1$t[i])

        # n2_B_samples %>%
        #     sample_draws(50) %>%
        #     ggplot(aes(time, B)) +
        #     geom_point() +
        #     geom_point(aes(t, speed),
        #         data = ds1,
        #         colour = "red"
        #     )

        # n2_samples.raw <-
        #     coda.samples(n2_fit,
        #         variable.names = c("beta", "phi"),
        #         n.iter = 5000,
        #         thin = 5
        #     )
        # n2_samples <- n2_samples.raw %>%
        #     spread_draws(beta, phi)

        # n2_samples %>%
        #     ggplot(aes(.iteration, group = .chain)) +
        #     geom_path(aes(y = phi))

        # n2_samples %>%
        #     ggplot(aes(beta)) +
        #     geom_histogram()

        # n2_estimate <- n2_samples %>%
        #     median_qi()

        # # transform from beta to travel time
        # # tt = L / V; V = Vmax / (1 + e^-eta)
        # # tt = L * (1 + e^-eta) / Vmax
        # ilogit <- function(eta) eta
        #     # segdata$L * (1 + exp(-eta)) / segdata$Vmax
        # n2_res <- n2_estimate %>%
        #     mutate(
        #         eta_lower = qnorm(0.1, beta.lower, phi),
        #         eta_upper = qnorm(0.975, beta.upper, phi),
        #         mean = ilogit(beta),
        #         mean_lower = ilogit(beta.upper),
        #         mean_upper = ilogit(beta.lower),
        #         B_lower = ilogit(eta_upper),
        #         B_upper = ilogit(eta_lower),
        #         tmin = min(dtimes),
        #         tmax = max(dtimes)
        #     )

        # # n1_res %>%
        # #     ggplot() +
        # #     geom_ribbon(
        # #         aes(ymin = eta_lower, ymax = eta_upper),
        # #         fill = "orangered",
        # #         alpha = 0.5
        # #     ) +
        # #     geom_ribbon(
        # #         aes(ymin = beta.lower, ymax = beta.upper)
        # #     ) +
        # #     geom_path()

        # n2_res %>%
        #     ggplot() +
        #     geom_rect(
        #         aes(
        #             xmin = tmin, xmax = tmax,
        #             ymin = B_lower, ymax = B_upper
        #         ),
        #         colour = "gray", alpha = 0.25
        #     ) +
        #     geom_rect(
        #         aes(
        #             xmin = tmin, xmax = tmax,
        #             ymin = mean_lower, ymax = mean_upper
        #         ),
        #         fill = "orangered", alpha = 0.5
        #     ) +
        #     geom_hline(aes(yintercept = mean), colour = "orangered") +
        #     geom_point(
        #         aes(t, speed),
        #         data = ds1, size = 1
        #     )


        # ## Compare!
        # n1_dic <- dic.samples(n1_fit, n.iter = 5000, thin = 5)
        # n2_dic <- dic.samples(n2_fit, n.iter = 5000, thin = 5)

        # n1_dic
        # n2_dic
        # diffdic(n1_dic, n2_dic)
    }

    ########### And repeat for the whole week
    d_all <- ds %>%
        mutate(
            timestamp = as.integer(as.POSIXct(date)) + t
        ) %>%
        arrange(timestamp) %>%
        mutate(
            t_index = cumsum(c(1, diff(timestamp) > 0))
        ) %>%
        select(trip_id, timestamp, date, t, t_index, speed)

    segdata <-
        list(
            b = d_all$speed,
            E = 0.8,
            t = d_all$t_index,
            delta = do.call(c, tapply(d_all$t, d_all$date,
                function(ti) c(0, diff(sort(unique(ti))))
            )) %>% as.integer,
            N = nrow(d_all),
            M = max(d_all$t_index),
            D = length(unique(d_all$date)),
            c0 = as.integer(tapply(d_all$t_index, d_all$date, min)),
            c1 = as.integer(tapply(d_all$t_index, d_all$date, min)) + 1L,
            cJ = as.integer(tapply(d_all$t_index, d_all$date, max)),
            Vmax = 60 / 3.6
        )
    dtimes <- tapply(d_all$t, d_all$t_index, min)
    ddates <- tapply(d_all$date, d_all$t_index, min)

    n12_fit <-
        jags.model(
            # "models/quickfit2.jags",
            "models/nw_model_kf.jags",
            data = segdata,
            n.chains = 4,
            n.adapt = 10000
            # quiet = TRUE
        )

    ## For checking, also take some samples of
    ## the vehicles' fitted Bs:
    n12_B_samples.raw <-
        coda.samples(n12_fit,
            variable.names = "B",
            n.iter = 5000,
            thin = 5
        )
    n12_B_samples <- n12_B_samples.raw %>%
        spread_draws(B[i]) %>%
        mutate(
            date = d_all$date[i],
            time = d_all$t[i]
        )

    ## Graph sample of Bs with observed data:
    n12_B_samples %>%
        sample_draws(50) %>%
        ggplot(aes(time, B)) +
        geom_point() +
        geom_point(aes(t, speed),
            data = d_all,
            colour = "red"
        ) +
        facet_grid(date~.)

    ## Sample the main model parameters
    n12_samples.raw <-
        coda.samples(n12_fit,
            variable.names = c("beta", "phi", "q"),
            n.iter = 5000,
            thin = 5
        )
    n12_samples <- n12_samples.raw %>%
        spread_draws(beta[t], phi, q) %>%
        mutate(
            date = ddates[t],
            time = dtimes[t]
        )

    n12_samples %>%
        filter(t == 1) %>%
        ggplot(aes(.iteration, group = .chain)) +
            geom_path(aes(y = phi))

    n12_samples %>%
        filter(t == 1) %>%
        ggplot(aes(.iteration, group = .chain)) +
            geom_path(aes(y = q))

    n12_samples %>%
        sample_draws(n = 50) %>%
        arrange(time) %>%
        ggplot(aes(time, beta, group = .draw)) +
            geom_path() +
            facet_grid(date~.)

    n12_estimate <- n12_samples %>%
        select(.chain, .iteration, .draw, t, beta, phi, q) %>%
        median_qi()

    n12_res <- n12_estimate %>%
        mutate(
            eta_lower = qnorm(0.1, beta.lower, phi),
            eta_upper = qnorm(0.975, beta.upper, phi),
            mean = beta,
            mean_lower = beta.upper,
            mean_upper = beta.lower,
            B_lower = eta_upper,
            B_upper = eta_lower,
            date = ddates[t],
            time = dtimes[t]
        ) %>%
        arrange(time)

    n12_res %>%
        ggplot(aes(time, beta)) +
            geom_ribbon(
                aes(ymin = eta_lower, ymax = eta_upper),
                fill = "orangered",
                alpha = 0.5
            ) +
            geom_ribbon(
                aes(ymin = beta.lower, ymax = beta.upper)
            ) +
            geom_path() +
            facet_grid(date~.)

    n12_res %>%
        ggplot(aes(time)) +
        geom_ribbon(
            aes(ymin = B_lower, ymax = B_upper),
            colour = "gray", alpha = 0.25
        ) +
        geom_ribbon(aes(ymin = mean_lower, ymax = mean_upper),
            fill = "orangered", alpha = 0.5
        ) +
        geom_path(aes(y = mean), colour = "orangered") +
        geom_point(
            aes(t, speed),
            data = d_all, size = 1
        ) +
        facet_grid(date~.)

    res_list <-
        list(
            # data = segdata,
            # Bs = n12_B_samples,
            # pars = n12_samples,
            estimate = n12_samples.raw %>% spread_draws(phi, q) %>% median_qi
            # res = n12_res
        )
    save(res_list$estimate, file = fid)
}
