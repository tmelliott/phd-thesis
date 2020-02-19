
simulate_vehicle <- function(length = 2000,
                             stops = seq(0, length, length.out = 5),
                             segments = c(200, 400, 800, 1400),
                             include = c("none", "stops", "segments", "both"),
                             segment.speeds = runif(length(segments), 5, 15),
                             seed = 1,
                             noise = 0.5,
                             accel_prop = 0.5,
                             pi = 0,
                             gamma = 10,
                             tau = c(18, 10),
                             rho = 0,
                             omega = c(30, 10)
                            ) {
    set.seed(seed)
    include <- match.arg(include)

    distance <- numeric()
    speed <- numeric()
    acceleration <- numeric()

    stops <- tibble(type = "stop", distance = stops)
    segments <- tibble(type = "segment", distance = unique(c(0, segments)))
    z <- bind_rows(stops, segments) %>% arrange(distance) %>%
        distinct() %>%
        mutate(t = 0, d = 0)
    znext <- which(z$distance > 0)[1]

    x <- numeric(3)
    x[2] <- runif(1, 5, 15)
    distance <- c(distance, x[1])
    speed <- c(speed, x[2])
    acceleration <- c(acceleration, x[3])
    while (x[1] < length) {
        # propose new acceleration (speed in [5, 20])
        accel <- truncnorm::rtruncnorm(1, 5 - x[2], 20 - x[2], x[3], noise)
        alpha <- min(1, dnorm(accel, 0, accel_prop) / dnorm(x[3], 0, accel_prop))
        if (runif(1) < alpha) x[3] <- accel
        else x[3] <- max(min(x[3], 20 - x[2]), 5 - x[2])
        x[2] <- x[2] + x[3]
        x[1] <- min(length, x[1] + x[2])
        if (znext <= nrow(z) && x[1] >= z$distance[znext]) {
            # print(z[znext, "type"])
            switch(z$type[znext],
                "stop" = {
                    z$t[znext] <- length(distance)
                    if (runif(1) < pi) {
                        dwell <- round(gamma +
                            truncnorm::rtruncnorm(1, 0, Inf, tau[1], tau[2])
                        )
                        z$d[znext] <- dwell
                        x[1] <- z$distance[znext]
                        x[2] <- runif(1, 5, 10)
                        x[3] <- 0
                        distance <- c(distance, rep(x[1], dwell))
                        speed <- c(speed, rep(0, dwell))
                        acceleration <- c(acceleration, rep(x[3], dwell))
                    }
                },
                "segment" = {
                    z$t[znext] <- length(distance)
                    if (runif(1) < rho) {
                        dwell <- round(
                            truncnorm::rtruncnorm(1, 0, Inf, omega[1], omega[2])
                        )
                        z$d[znext] <- dwell
                        x[1] <- z$distance[znext]
                        x[2] <- runif(1, 5, 10)
                        x[3] <- 0
                        distance <- c(distance, rep(x[1], dwell))
                        speed <- c(speed, rep(0, dwell))
                        acceleration <- c(acceleration, rep(x[3], dwell))
                    }
                }
            )
            znext <- znext + 1
        }

        distance <- c(distance, x[1])
        speed <- c(speed, x[2])
        acceleration <- c(acceleration, x[3])
    }

    Tmax <- length(distance) - 1
    Dmax <- max(distance)
    list(
        path = tibble(
            time = 0:Tmax,
            distance = distance,
            speed = speed,
            acceleration = acceleration
        ),
        stops = z %>% filter(type == "stop"),
        segments = z %>% filter(type == "segment") %>%
            mutate(
                id = as.character(1:n()),
                segment = paste("Segment", id),
                length = diff(c(distance, Dmax)),
                tt = diff(c(t, Tmax)) - d, # minus dwell times at any stops in this segment
                avg_speed = length / tt
            ),
        observations = structure(
            lapply(c("high", "low", "waypoints"),
                function(obs) {
                    tibble(
                        t = switch(obs,
                            "high" = c(seq(0, Tmax, by = 10), Tmax) %>% unique,
                            "low" = c(seq(0, Tmax, by = 30), Tmax) %>% unique,
                            "waypoints" = c(z %>% pluck("t") %>% unique, Tmax)
                        )

                    ) %>% mutate(
                        x = distance[t+1] # 1-index, but time starts at 0
                    ) %>%
                    bind_rows(tibble(t = Tmax + 30, x = Dmax))
                }
            ),
            .Names = c("high", "low", "waypoints")
        )
    )
}

s1 <- simulate_vehicle(noise = 0.5, accel_prop = 0.5, seed = 3)

# p <- ggplot(s1$path, aes(time)) +
#     xlab("Time (s)")
# pd <- p +
#     geom_hline(yintercept = s1$stops$distance, col = "red", lty = 3) +
#     geom_hline(yintercept = s1$segments$distance, col = "blue", lty = 3)
# egg::ggarrange(
#     pd + geom_path(aes(y = distance)) + ylab("Distance (m)"),
#     pd + geom_point(aes(t, x), data = s1$observations$high),
#     p + geom_path(aes(y = speed)) + ylab("Speed (m/s)"),
#     pd + geom_point(aes(t, x), data = s1$observations$low),
#     p + geom_path(aes(y = acceleration)) + ylab("Acceleration (m/s/s)"),
#     pd + geom_point(aes(t, x), data = s1$observations$waypoints),
#     # ggplot(s1$segments) +
#     #     geom_segment(aes(x = 0, xend = tt, y = id, yend = id)) +
#     #     geom_point(aes(tt, id)) +
#     #     xlab("Travel time (s)") + ylab("Segment"),
#     ncol = 2
# )
