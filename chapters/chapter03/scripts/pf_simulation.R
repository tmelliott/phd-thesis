## Perform a robust particle filter simulation for the purposes
## of evaluating the performance of various models
## in estimating average road speed.

suppressPackageStartupMessages(library(tidyverse))
source("scripts/vehicle_simulation.R")

run_simulation <- function(seed = 1, include = "none",
                           pr_stop = 0.0, pr_int = 0.0,
                           noise = c(0.5, 2.0, 0.2),
                           accel_prop = 0.5,
                           n_particle = 2000,
                           prefix = "simA_results",
                           draw = FALSE) {
    rda_file <- sprintf("sims/%s_%03d.rda", prefix, seed)
    if (file.exists(rda_file)) {
        load(rda_file)
        return(results)
    }

    # Step 1: simulate data
    sim <- simulate_vehicle(
        include = include,
        seed = seed,
        accel_prop = accel_prop,
        pi = as.integer(include %in% c("stops", "both")),
        rho = as.integer(include %in% c("segments", "both")),
    )

    if (interactive()) {
        ggplot(sim$path, aes(time, distance)) +
            geom_path()
    }

    # Step 3: run particle filter to estimate average segment speeds
    #         - transition function A1 (constant speed)
    #         - transition function A2 (random speed)
    #         - transition function A3 (random acceleration)
    fits <- lapply(1:3,
        function(model) {
            lapply(1:3,
                function(obs) {
                    dat <- sim$observations[[obs]]
                    isBAD <- TRUE
                    while (isBAD) {
                        pf <- try({
                            particle_filter(dat$t, dat$x, n_particle, model,
                                sim$stops, sim$segments,
                                noise[model], 3.0, pr_stop, pr_int,
                                draw = draw
                            )
                        })
                        isBAD <- inherits(pf, "try-error")
                    }
                    tibble(
                        speed_estimate = pf$mean,
                        speed_sd = pf$sd,
                        segment = paste("Segment", seq_len(nrow(sim$segments)))
                    ) %>%
                        mutate(model = model, obs = obs)
                }
            ) %>% bind_rows()
        }
    ) %>% bind_rows() %>%
        left_join(
            sim$segments %>% select(segment, length, avg_speed),
            by = "segment"
        ) %>%
        select(model, obs, segment, length, avg_speed, speed_estimate, speed_sd)

    results <- list(fits = fits, sim = sim)
    save(results, file = rda_file)

    results
}

particle_filter <- function(t, x, n = 5000, model = 1,
                            stops, segments,
                            noise = 2.0, gps = 3.0,
                            pr_stop = 0.0, pr_int = 0.0,
                            gamma = 10.0, tau = c(18, 10),
                            draw = FALSE) {

    # storage
    Tstart <- matrix(0L, nrow = n, ncol = nrow(segments))
    Tend <- matrix(0L, nrow = n, ncol = nrow(segments))
    dwell <- matrix(0L, nrow = n, ncol = nrow(stops))
    # map stops to segments
    H <- matrix(0L, nrow = nrow(segments), ncol = nrow(stops))
    for (i in 1:nrow(stops)) {
        H[max(which(stops$distance[i] >= segments$distance)), i] <- 1L
    }

    # inital state
    state <- tibble(
        distance = numeric(n),
        speed = runif(n, 0, 30),
        acceleration = numeric(n),
        likelihood = numeric(n),
        weight = rep(1 / n, n),
        segment_index = 1L,
        stop_index = 1L,
        dwell = integer(n),
        dwell_time = replicate(n, numeric(nrow(stops)), simplify = FALSE),
        segment_start = replicate(n, numeric(nrow(segments)), simplify = FALSE),
        segment_end = replicate(n, numeric(nrow(segments)), simplify = FALSE)
    )

    current_segment <- 1L
    current_stop <- 1L
    time <- 0
    for (k in 2:length(t)) {
        # k=2
        delta <- t[k] - t[k-1]
        # add noise
        if (model == 1) {
            state <- state %>%
                mutate(
                    speed = truncnorm::rtruncnorm(n, 0, 30, speed,
                        delta * noise)
                )
        }

        state <- state %>%
            mutate(
                dwell = ifelse(dwell == 0,
                    0,
                    truncnorm::rtruncnorm(n, 0, Inf, tau[1], tau[2])
                )
            )

        # mutate
        while (time < t[k]) {
            if (model == 2) {
                state <- state %>%
                    mutate(
                        speed = ifelse(dwell == 0,
                            truncnorm::rtruncnorm(n, 0, 30, speed, noise),
                            speed
                        )
                    )
            }
            if (model == 3) {
                state <- state %>%
                    mutate(
                        acceleration = ifelse(dwell == 0,
                            truncnorm::rtruncnorm(
                                n,
                                -speed,
                                30 - speed,
                                acceleration,
                                noise
                            ),
                            acceleration
                        ),
                        speed = ifelse(dwell == 0, speed + acceleration, speed)
                    )
            }

            state <- state %>%
                mutate(
                    distance = ifelse(dwell == 0, distance + speed, distance),
                    dwell = pmax(0L, dwell - 1L)
                )

            for (i in 1:n) {
                if (state$distance[i] >= max(stops$distance)) {
                    state$distance[i] <- max(stops$distance)
                    if (state$segment_end[[i]][nrow(segments)] == 0) {
                        state$segment_end[[i]][nrow(segments)] <- time
                    }
                    next
                }
                # pass stop?
                if (state$distance[i] >=
                    stops$distance[state$stop_index[i] + 1L]) {
                    state$stop_index[i] <- state$stop_index[i] + 1L
                    if (runif(1) < pr_stop) {
                        state$dwell[i] <- round(
                            gamma + truncnorm::rtruncnorm(1, 0, Inf,
                                mean = tau[1], sd = tau[2]
                            )
                        )
                        state$distance[i] <- stops$distance[state$stop_index[i]]
                        state$dwell_time[[i]][state$stop_index[i]] <-
                            state$dwell[i]
                    }
                }
                # pass intersection?
                if (state$segment_index[i] == nrow(segments)) {
                    next
                }
                if (state$distance[i] >=
                    segments$distance[state$segment_index[i] + 1L]) {
                    state$segment_end[[i]][state$segment_index[i]] <- time
                    state$segment_index[i] <- state$segment_index[i] + 1
                    state$segment_start[[i]][state$segment_index[i]] <- time
                }
            }

            time <- time + 1L
        }

        if (draw) {
            p0 <- ggplot(sim$path, aes(time, distance)) +
                geom_path() +
                geom_hline(aes(yintercept = distance),
                    data = stops, lty = 2, colour = "red") +
                geom_hline(aes(yintercept = distance),
                    data = segments, lty = 3, colour = "blue")
        }

        # reweight
        state <- state %>%
            mutate(
                likelihood = dnorm(x[k], distance, gps),
                weight = weight * likelihood / sum(weight * likelihood)
            )

        Neff <- 1 / sum(state$weight^2)
        if (draw) {
            print(
                p0 + geom_point(
                    aes(time, distance, alpha = weight, colour = weight),
                    data = state
                ) +
                ggtitle(sprintf("Neff = %s", Neff))
            )
            # grid::grid.locator()
        }


        # resample
        #if (Neff < n / 2) {
            state <- state %>%
                sample_n(n, replace = TRUE, weight = weight) %>%
                mutate(weight = 1 / n)
        #}

        # compute travel times
        if (min(state$segment_index) > current_segment) {

            Tend[, current_segment] <-
                sapply(state$segment_end, function(z) z[current_segment])
            current_segment <- current_segment + 1

            Tstart[, current_segment] <-
                sapply(state$segment_start, function(z) z[current_segment])

            # dwell times for intermediary stops
            int_stops <-
                stops$distance > segments$distance[current_segment - 1] &
                stops$distance < segments$distance[current_segment - 1] +
                    segments$length[current_segment - 1]

            if (any(int_stops)) {
                for (j in which(int_stops)) {
                    dwell[, j] <- sapply(state$dwell_time, function(z) z[j])
                }
            }
        #} else if (all(state$segment_index == nrow(segments))) {
        } else if (all(state$distance == max(stops$distance))) {
            # Tstart[, nrow(segments)] <-
            #     sapply(state$segment_start, function(z) z[nrow(segments)])
            Tend[, nrow(segments)] <-
                sapply(state$segment_end, function(z) z[nrow(segments)])

            # dwell times for intermediary stops
            int_stops <-
                stops$distance > segments$distance[current_segment] &
                stops$distance < segments$distance[current_segment] +
                    segments$length[current_segment]

            if (any(int_stops)) {
                for (j in which(int_stops)) {
                    dwell[, j] <- sapply(state$dwell_time, function(z) z[j])
                }
            }
        }
    }

    TT <- Tend - Tstart - dwell %*% t(H)
    speed <- sweep(TT, 2, segments$length,
        function(a, b) b / a)

    list(
        mean = colMeans(speed),
        sd = apply(speed, 2, sd)
    )
}
