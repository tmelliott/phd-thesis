suppressPackageStartupMessages(library(tidyverse))

source("vehicle_simulation.R")
sim1 <- function(n = 120, noise = 0.09, seed = 1) {
    set.seed(seed)
    x <- numeric(n+1)
    x[1] <- 0
    v <- 10 # assume in motion already
    a <- 0
    # generate trajectory
    for (i in 1:n + 1) {
        an <- NA
        j <- 1
        while (is.na(an) || v + an < 0 || v + an > 15) {
            an <- rnorm(1, a, noise * j)
            j <- j + 0.01
        }
        a <- an
        v <- v + a
        x[i] <- x[i-1] + v
    }
    # sample

    # return
    tibble(
        t = 0:n,
        x = x,
        v = c(diff(x), v)
    )
}

doSim <- function(Nparticle, simnames, seed, fn) {
    file <- sprintf('sims/sim1_%s.rda', fn)
    if (file.exists(file)) {
        load(file)
        if (!is.null(z)) return(z)
    }
    s1 <- simulate_vehicle(seed = seed)
    segs <- lapply(1:nrow(s1$segments),
        function(i) s1$segments$distance[i] + c(0, s1$segments$length[i])
    )
    tt <- s1$segments$tt

    S1 <- lapply(seq_along(s1$observations), function(i) {
        sr <- NULL
        badcount <- 0
        while (is.null(sr) || inherits(sr, "try-error")) try({
            badcount <- badcount + 1
            if (badcount > 10) return(NULL)
            sr <- pf1(s1$observations[[i]],
                n = Nparticle,
                noise = switch(i,
                    0.2 + badcount / 100,
                    0.1 + badcount / 100,
                    1.0 + badcount / 20
                ),
                seg = segs
            ) %>%
                mutate(
                    s1 = ifelse(s1 > 0, s1, NA),
                    s2 = ifelse(s2 > 0, s2, NA),
                    s3 = ifelse(s3 > 0, s3, NA),
                    s4 = ifelse(s4 > 0, s4, NA),
                    s5 = ifelse(s5 > 0, s5, NA),
                ) %>%
                select(s1, s2, s3, s4, s5) %>%
                gather(key = "segment", value = "travel_time") %>%
                filter(!is.na(travel_time)) %>%
                mutate(sim = simnames[i])
        }, silent = TRUE)
        sr
    }) %>%
        bind_rows() %>%
        mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A1"
        )

    S2 <- lapply(seq_along(s1$observations), function(i) {
        sr <- NULL
        badcount <- 0
        while (is.null(sr) || inherits(sr, "try-error")) try({
            badcount <- badcount + 1
            if (badcount > 10) return(NULL)
            sr <- pf2(s1$observations[[i]],
                n = Nparticle,
                noise = switch(i,
                    0.5 + badcount / 100,
                    0.5 + badcount / 100,
                    1.0 + badcount / 20
                ),
                seg = segs
            ) %>%
                mutate(
                    s1 = ifelse(s1 > 0, s1, NA),
                    s2 = ifelse(s2 > 0, s2, NA),
                    s3 = ifelse(s3 > 0, s3, NA),
                    s4 = ifelse(s4 > 0, s4, NA),
                    s5 = ifelse(s5 > 0, s5, NA),
                ) %>%
                select(s1, s2, s3, s4, s5) %>%
                gather(key = "segment", value = "travel_time") %>%
                filter(!is.na(travel_time)) %>%
                mutate(sim = simnames[i])
        }, silent = TRUE)
        sr
    }) %>%
        bind_rows() %>%
        mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A2"
        )

    S3 <- lapply(seq_along(s1$observations), function(i) {
        sr <- NULL
        badcount <- 0
        while (is.null(sr) || inherits(sr, "try-error")) try({
            badcount <- badcount + 1
            if (badcount > 10) return(NULL)
            sr <- pf3(s1$observations[[i]],
                n = Nparticle,
                noise = switch(i,
                    0.1 + badcount / 100,
                    0.5 + badcount / 50,
                    0.5 + badcount / 50
                ),
                seg = segs
            ) %>%
                mutate(
                    s1 = ifelse(s1 > 0, s1, NA),
                    s2 = ifelse(s2 > 0, s2, NA),
                    s3 = ifelse(s3 > 0, s3, NA),
                    s4 = ifelse(s4 > 0, s4, NA),
                    s5 = ifelse(s5 > 0, s5, NA),
                ) %>%
                select(s1, s2, s3, s4, s5) %>%
                gather(key = "segment", value = "travel_time") %>%
                filter(!is.na(travel_time)) %>%
                mutate(sim = simnames[i])
        }, silent = TRUE)
        sr
    }) %>%
        bind_rows() %>%
        mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A3"
        )

    z <- bind_rows(S1, S2, S3) %>%
        mutate(
            truth = tt[as.numeric(gsub("Segment ", "", segment,))]
        ) %>%
        group_by(sim, model, segment) %>%
        summarize(
            estimate = mean(travel_time, na.rm = TRUE),
            variance = var(travel_time, na.rm = TRUE),
            rmse = mean((travel_time - truth)^2, na.rm = TRUE),
            median = median(travel_time, na.rm = TRUE),
            truth = first(truth)
        )
    save(z, file = file)
    z
}


pf1 <- function(d, n = 5000, seg, noise = 1, gps = 3, stops = range(start, end),
                pi = 0.5, gamma = 6, tau = c(15, 5)) {
    nseg <- 0
    start <- min(d$x)
    end <- max(d$x)
    if (!missing(seg)) {
        nseg <- length(seg)
        start <- sapply(seg, function(x) x[1])
        end <- sapply(seg, function(x) x[2])
    }
    X <- array(NA, dim = c(n, 2 + nseg, nrow(d)))
    t <- d$t
    y <- d$x
    delta <- c(0, diff(t))
    X[,1,1] <- 0
    X[,2,1] <- runif(n, 0, 30)
    for (i in 2:nrow(d)) {
        curstop <- sapply(X[,1,i-1], function(x) tail(which(stops <= x), 1))
        # initial wait time
        wait <- pmin(round(ifelse(curstop > 1 & X[,1,i-1] == stops[curstop],
            truncnorm::rtruncnorm(n, 0, Inf, tau[1], tau[2]),
            0
        )), delta[i])

        # distance based on prev speed (for plotting)
        X[,1,i] <- X[,1,i-1] + (delta[i] - wait) * X[,2,i-1]

        # wait time if it passes a stop
        wait <- pmin(wait + round(ifelse(curstop > 1 &
                                    curstop + 1 < length(stops) &
                                    X[,1,i] >= stops[curstop+1] &
                                    runif(n) < pi,
            gamma + truncnorm::rtruncnorm(n, 0, Inf, tau[1], tau[2]),
            0
        )), delta[i])
        X[,1,i] <- X[,1,i-1] + (delta[i] - wait) * X[,2,i-1]

        if (nseg > 0) {
            X[,2 + 1:nseg,i] <- t(sapply(1:n, function(ii) {
                est_tt(X[ii,,i-1],
                    start = start, end = end, dt = delta[i] - wait[ii]
                )
            }))
        }

        # lhood
        wt <- dexp(((y[i] - X[,1,i]) / gps)^2, 0.5)
        if (sum(wt) > 0) {
            wt <- wt / sum(wt)
            wi <- sample(n, replace = TRUE, prob = wt)
            X[,,i] <- X[wi,,i]
        } else {
            wt <- rep(1/n, n)
        }

        # speed noise after resampling
        X[,2,i] <- truncnorm::rtruncnorm(n, 0, 30, X[wi,2,i-1], noise * delta[i])
    }
    # reshape it ...
    segn <- NULL
    if (nseg > 0) segn <- paste0("s", 1:nseg)
    dimnames(X) <- list(NULL, c("x", "v", segn), c(paste0("p", t)))
    Z <- apply(X, 3, as_tibble)
    for (i in names(Z))
        Z[[i]] <- Z[[i]] %>%
            mutate(
                t = as.integer(gsub("p", "", i)),
                delta = delta[which(d$t == t[1]) + 1],
            )
    bind_rows(Z) %>%
        mutate(xhat = x + delta * v)
}


pf2 <- function(d, n = 5000, seg, noise = 0.01, gps = 3, stops = range(start, end),
                pi = 0.5, gamma = 6, tau = c(15, 5)) {
    nseg <- 0
    start <- min(d$x)
    end <- max(d$x)
    if (!missing(seg)) {
        nseg <- length(seg)
        start <- sapply(seg, function(x) x[1])
        end <- sapply(seg, function(x) x[2])
    }
    X <- array(NA, dim = c(n, 2 + nseg, nrow(d)))
    t <- d$t
    y <- d$x
    delta <- c(0, diff(t))
    X[,1,1] <- 0
    X[,2,1] <- runif(n, 5, 25)
    for (i in 2:nrow(d)) {
        curstop <- sapply(X[,1,i-1], function(x) tail(which(stops <= x), 1))
        wait <- pmin(round(ifelse(curstop > 1 & X[,1,i-1] == stops[curstop],
            truncnorm::rtruncnorm(n, 0, Inf, tau[1], tau[2]),
            0
        )), delta[i])

        # distance based on prev speed (for plotting)
        X[,,i] <- X[,,i-1]
        if (nseg > 0)
            for (j in 1:nseg)
                X[, 2 + j, i] <- ifelse(X[, 2+j, i] > 0, NA, X[, 2+j, i])

        for (k in 1:n) {
            j = 1
            while (j <= delta[i] - wait[k]) {
                j <- j + 1
                X[k,2,i] <- truncnorm::rtruncnorm(1, 0, 30, X[k,2,i], noise)
                if (nseg > 0) {
                    X[k,2 + 1:nseg,i] <- est_tt(X[k,,i],
                        start = start, end = end, dt = 1, keep = TRUE
                    )
                }
                X[k,1,i] <- min(max(end), X[k,1,i] + X[k,2,i])

                # potentially wait at stop
                if (curstop[k] + 1 < length(stops) && X[k,1,i] >= stops[curstop[k]+1]) {
                    curstop[k] <- curstop[k] + 1
                    if (runif(1) < pi) {
                        X[k,1,i] <- stops[curstop[k]]
                        X[k,2,i] <- runif(1, 5, 25)
                        j <- j + gamma + truncnorm::rtruncnorm(1, 0, Inf, tau[1], tau[2])
                    }
                }
            }
        }

        # lhood
        wt <- dexp(((y[i] - X[,1,i]) / gps)^2, 0.5)
        if (sum(wt) > 0) {
            wt <- wt / sum(wt)
            wi <- sample(n, replace = TRUE, prob = wt)
            X[,,i] <- X[wi,,i]
        } else {
            wt <- rep(1/n, n)
        }
    }
    # reshape it ...
    segn <- NULL
    if (nseg > 0) segn <- paste0("s", 1:nseg)

    dimnames(X) <- list(NULL, c("x", "v", segn), c(paste0("p", t)))
    Z <- apply(X, 3, as_tibble)
    for (i in names(Z))
        Z[[i]] <- Z[[i]] %>%
            mutate(
                t = as.integer(gsub("p", "", i)),
                delta = delta[which(d$t == t[1]) + 1],
            )

    bind_rows(Z)
}


pf3 <- function(d, n = 5000, seg, noise = 0.5, gps = 3, stops = range(start, end),
                pi = 0.5, gamma = 6, tau = c(15, 5)) {
    nseg <- 0
    start <- min(d$x)
    end <- max(d$x)
    if (!missing(seg)) {
        nseg <- length(seg)
        start <- sapply(seg, function(x) x[1])
        end <- sapply(seg, function(x) x[2])
    }
    X <- array(NA, dim = c(n, 3 + nseg, nrow(d)))
    t <- d$t
    y <- d$x
    delta <- c(0, diff(t))
    X[,1,1] <- 0
    X[,2,1] <- runif(n, 0, 30)
    X[,3,1] <- 0
    for (i in 2:nrow(d)) {
        curstop <- sapply(X[,1,i-1], function(x) tail(which(stops <= x), 1))
        wait <- pmin(round(ifelse(curstop > 1 & X[,1,i-1] == stops[curstop],
            truncnorm::rtruncnorm(n, 0, Inf, tau[1], tau[2]),
            0
        )), delta[i])

        X[,,i] <- X[,,i-1]
        if (nseg > 0)
            for (j in 1:nseg)
                X[, 3 + j, i] <-
                    ifelse(X[, 3 + j, i] > 0, NA, X[, 3 + j, i])


        for (k in 1:n) {
            j = 1
            while (j <= delta[i] - wait[k]) {
                j <- j + 1
                # v + a < 30 & v + a > 0
                # a < 30 - v & a > -v
                v <- X[k,2,i]
                X[k,3,i] <- truncnorm::rtruncnorm(1, -v, 30 - v, X[k,3,i], noise)
                X[k,2,i] <- X[k,2,i] + X[k,3,i]
                if (nseg > 0) {
                    X[k, 3 + 1:nseg,i] <- est_tt(X[k,,i],
                        start = start, end = end, dt = 1, keep = TRUE, xdim = 3
                    )
                }
                X[k,1,i] <- X[k,1,i] + X[k,2,i]

                # potentially wait at stop
                if (curstop[k] + 1 < length(stops) && X[k,1,i] >= stops[curstop[k]+1]) {
                    curstop[k] <- curstop[k] + 1
                    if (runif(1) < pi) {
                        X[k,1,i] <- stops[curstop[k]]
                        X[k,2,i] <- runif(1, 5, 25)
                        j <- j + gamma + truncnorm::rtruncnorm(1, 0, Inf, tau[1], tau[2])
                    }
                }
            }
        }

        # lhood
        wt <- dexp(((y[i] - X[,1,i]) / gps)^2, 0.5)
        if (sum(wt) > 0) {
            wt <- wt / sum(wt)
            wi <- sample(n, replace = TRUE, prob = wt)
            X[,,i] <- X[wi,,i]
        } else {
            wt <- rep(1/n, n)
        }
    }
    # reshape it ...
    segn <- NULL
    if (nseg > 0) segn <- paste0("s", 1:nseg)

    dimnames(X) <- list(NULL, c("x", "v", "a", segn), c(paste0("p", t)))
    Z <- apply(X, 3, as_tibble)
    for (i in names(Z))
        Z[[i]] <- Z[[i]] %>%
            mutate(
                t = as.integer(gsub("p", "", i)),
                delta = delta[which(d$t == t[1]) + 1],
            )

    bind_rows(Z)
}



est_tt <- function(x, start, end, dt, keep = FALSE, xdim = 2) {
    if (dt == 0) return(x[-(1:xdim)])
    for (i in seq_along(start)) {
        # starts after this segment, next segment
        if (x[1] >= end[i]) {
            if (!keep) x[xdim + i] <- NA
            next
        }
        xend <- x[1] + x[2] * dt
        # ends before this segment starts, exit
        if (xend < start[i]) break
        if (xend >= end[i]) {
            # ends past end of segment
            if (x[1] <= start[i]) {
                # starts before start of segment
                # proportion of delta: pr.delta = pr.dist
                # pr.dist = len(seg) / len(travelled)
                x[xdim + i] <- dt * (end[i] - start[i]) / (xend - x[1])
            } else {
                # starts after start of segment
                # pr.delta = pr.dist = len(travelled in seg) / len(travelled)
                x[xdim + i] <- -x[xdim + i] + dt * (end[i] - x[1]) / (xend - x[1])
            }
        } else {
            # ends before end of segment (so partial)
            if (x[1] <= start[i]) {
                # starts before start of segment
                # pr.dist = len(tr in seg) / len(tr)
                x[xdim + i] <- - dt * (xend - start[i]) / (xend - x[1])
            } else {
                # starts after start of segment so all of delta
                x[xdim + i] <- x[xdim + i] - dt
            }
        }
    }
    x[-(1:xdim)]
}



get_sim_1 <- function() {
    f <- "sims/sim1.rda"
    if (file.exists(f)) {
        load(f)
        return(S0)
    }

    set.seed(200)
    Nparticle = 2000
    sr1 <- pf1(s1$observations$high, n = Nparticle, noise = 0.2, seg = segs)

    # ggplot(sr1, aes(t, x)) +
    #     geom_point(aes(x = t + delta, y = xhat), col = "gray") +
    #     geom_point() +
    #     geom_point(data = s1$observations$high, col = "red", shape = 4)

    sr1 <- sr1 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr2 <- pf1(s1$observations$low, n = Nparticle, noise = 0.1, seg = segs)

    # ggplot(sr2, aes(t, x)) +
    #     geom_point(aes(x = t + delta, y = xhat), col = "gray") +
    #     geom_point() +
    #     geom_point(data = s1$observations$low, col = "red", shape = 4)

    sr2 <- sr2 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr3 <- pf1(s1$observations$waypoints, n = Nparticle, noise = 1, seg = segs)

    # ggplot(sr3, aes(t, x)) +
    #     geom_point(aes(x = t + delta, y = xhat), col = "gray") +
    #     geom_point() +
    #     geom_point(data = s1$observations$waypoints, col = "red", shape = 4)

    sr3 <- sr3 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    S1 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A1"
        )

    # ggplot(S1, aes(travel_time))  +
    #     geom_histogram(bins = 30) +
    #     facet_grid(sim~segment, scales = "free_x")

    set.seed(200)
    sr1 <- pf2(s1$observations$high, n = Nparticle, noise = 0.5, seg = segs) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr2 <- pf2(s1$observations$low, n = Nparticle, noise = 1, seg = segs) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr3 <- pf2(s1$observations$waypoints, n = Nparticle, noise = 1, seg = segs) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    S2 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A2"
        )
    # ggplot(S2, aes(travel_time))  +
    #     geom_histogram(bins = 30) +
    #     facet_grid(sim~segment, scales = "free_x")

    set.seed(200)
    sr1 <- pf3(s1$observations$high, n = Nparticle, noise = 0.1, seg = segs) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr2 <- pf3(s1$observations$low, n = Nparticle, noise = 0.5, seg = segs) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr3 <- pf3(s1$observations$waypoints, n = Nparticle, noise = 0.5, seg = segs) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    S3 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A3"
        )

    S0 <- bind_rows(S1, S2, S3)

    save(S0, file = f)
    S0
}


get_sim_2 <- function() {
    f <- "sims/sim2.rda"
    if (file.exists(f)) {
        load(f)
        return(S0)
    }

    set.seed(2000)
    Nparticle = 4000
    sr1 <- pf1(s2$observations$high, n = Nparticle, noise = 0.5, seg = segs,
        stops = s2$stops$distance)

    # ggplot(sr1, aes(t, x)) +
    #     geom_point() +
    #     geom_point(data = s2$observations$high, col = "red", shape = 4)

    sr1 <- sr1 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr2 <- pf1(s2$observations$low, n = Nparticle, noise = 0.5, seg = segs,
        stops = s2$stops$distance)

    # ggplot(sr2, aes(t, x)) +
    #     geom_point() +
    #     geom_point(data = s2$observations$low, col = "red", shape = 4)

    sr2 <- sr2 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr3 <- pf1(s2$observations$waypoints, n = Nparticle, noise = 1, seg = segs,
        stops = s2$stops$distance)

    # ggplot(sr3, aes(t, x)) +
    #     geom_point() +
    #     geom_point(data = s2$observations$waypoints, col = "red", shape = 4)

    sr3 <- sr3 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    S1 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A1"
        )

    # ggplot(S1, aes(travel_time))  +
    #     geom_histogram(bins = 30) +
    #     facet_grid(sim~segment, scales = "free_x")

    set.seed(2000)
    sr1 <- pf2(s2$observations$high, n = Nparticle, noise = 1, seg = segs,
        stops = s2$stops$distance)

    # ggplot(sr1, aes(t, x)) +
    #     geom_point() +
    #     geom_point(data = s2$observations$high, col = "red", shape = 4)

    sr1 <- sr1 %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))


    sr2 <- pf2(s2$observations$low, n = Nparticle, noise = 1, seg = segs,
        stops = s2$stops$distance) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr3 <- pf2(s2$observations$waypoints, n = Nparticle, noise = 1, seg = segs,
        stops = s2$stops$distance) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    S2 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A2"
        )
    ggplot(S2, aes(travel_time))  +
        geom_histogram(bins = 30) +
        facet_grid(sim~segment, scales = "free_x")

    set.seed(2000)
    sr1 <- pf3(s2$observations$high, n = Nparticle, noise = 0.1, seg = segs,
        stops = s2$stops$distance) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr2 <- pf3(s2$observations$low, n = Nparticle, noise = 0.5, seg = segs,
        stops = s2$stops$distance) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    sr3 <- pf3(s2$observations$waypoints, n = Nparticle, noise = 0.5, seg = segs,
        stops = s2$stops$distance) %>%
        mutate(
            s1 = ifelse(s1 > 0, s1, NA),
            s2 = ifelse(s2 > 0, s2, NA),
            s3 = ifelse(s3 > 0, s3, NA),
            s4 = ifelse(s4 > 0, s4, NA),
            s5 = ifelse(s5 > 0, s5, NA),
        ) %>%
        select(s1, s2, s3, s4, s5) %>%
        gather(key = "segment", value = "travel_time") %>%
        filter(!is.na(travel_time))

    S3 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment,
                "Segment 1" = "s1",
                "Segment 2" = "s2",
                "Segment 3" = "s3",
                "Segment 4" = "s4",
                "Segment 5" = "s5"
            ),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A3"
        )

    S0 <- bind_rows(S1, S2, S3)
    save(S0, file = f)
    S0
}
