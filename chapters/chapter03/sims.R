suppressPackageStartupMessages(library(tidyverse))
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

doSim <- function(n, seg1, seg2, Nparticle, simnames, seed, fn) {
    file <- sprintf('sims/sim1_%s.rda', fn)
    if (file.exists(file)) {
        load(file)
        if (!is.null(z)) return(z)
    }
    s1 <- sim1(n = n, noise = 0.08, seed = seed)
    if (max(s1$x) < max(seg2)) return(NULL)
    tt1 <- s1 %>% filter(between(x, seg1[1], seg1[2])) %>% pluck('t') %>% range %>% diff
    tt2 <- s1 %>% filter(between(x, seg2[1], seg2[2])) %>% pluck('t') %>% range %>% diff
    d1 <- s1 %>% filter(t %% 10 == 0)
    d2 <- s1 %>% filter(t %% 30 == 0)
    delta <- cumsum(round(runif(10, 10, 40)))
    d3 <- s1 %>% filter(t %in% delta)

    badcount <- 0

    # gp <- ggplot(s1, aes(t)) +
    #     geom_rect(aes(xmin = 0, xmax = n, ymin = seg1[1], ymax = seg1[2]),
    #         data = NULL, fill = 'lightgray') +
    #     geom_rect(aes(xmin = 0, xmax = n, ymin = seg2[1], ymax = seg2[2]),
    #         data = NULL, fill = 'lightgray') +
    #     theme_minimal() +
    #     theme(panel.grid = element_blank()) +
    #     xlab('Time (s)') + ylab('Distance (m)') +
    #     ylim(0, max(s1$x)) + xlim(0, n) +
    #     geom_path(aes(y = x))
    # print(gp)

    sr1 <- sr2 <- sr3 <- NULL
    while(is.null(sr1) || inherits(sr1, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr1 <- pf1(d1, n = Nparticle, noise = 1, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    while(is.null(sr2) || inherits(sr2, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr2 <- pf1(d2, n = Nparticle, noise = 1, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    while(is.null(sr3) || inherits(sr3, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr3 <- pf1(d3, n = Nparticle, noise = 1, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    S1 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment, "Segment 1" = "s1", "Segment 2" = "s2"),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A1"
        )

    sr1 <- sr2 <- sr3 <- NULL
    while(is.null(sr1) || inherits(sr1, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr1 <- pf2(d1, n = Nparticle, noise = 0.5, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    while(is.null(sr2) || inherits(sr2, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr2 <- pf2(d2, n = Nparticle, noise = 0.5, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    while(is.null(sr3) || inherits(sr3, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr3 <- pf2(d3, n = Nparticle, noise = 0.5, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    S2 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment, "Segment 1" = "s1", "Segment 2" = "s2"),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A2"
        )

    sr1 <- sr2 <- sr3 <- NULL
    while(is.null(sr1) || inherits(sr1, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr1 <- pf3(d1, n = Nparticle, noise = 0.1, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    while(is.null(sr2) || inherits(sr2, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr2 <- pf3(d2, n = Nparticle, noise = 0.1, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    while(is.null(sr3) || inherits(sr3, "try-error")) try({
        badcount <- badcount + 1
        if (badcount > 100) return(NULL)
        sr3 <- pf3(d3, n = Nparticle, noise = 0.1, seg = list(seg1, seg2)) %>%
            mutate(
                s1 = ifelse(s1 > 0, s1, NA),
                s2 = ifelse(s2 > 0, s2, NA)
            ) %>%
            select(s1, s2) %>%
            gather(key = "segment", value = "travel_time") %>%
            filter(!is.na(travel_time))
    }, silent = TRUE)
    S3 <- bind_rows(
        sr1 %>% mutate(sim = simnames[1]),
        sr2 %>% mutate(sim = simnames[2]),
        sr3 %>% mutate(sim = simnames[3])
    ) %>% mutate(
            segment = fct_recode(segment, "Segment 1" = "s1", "Segment 2" = "s2"),
            sim = fct_relevel(sim, simnames[1], simnames[2], simnames[3]),
            model = "A3"
        )
    z <- bind_rows(S1, S2, S3) %>%
        mutate(
            truth = ifelse(segment == "Segment 1", tt1, tt2),
            err = (travel_time - truth)^2
        ) %>%
        group_by(sim, model, segment) %>%
        summarize(
            rmse = mean(err, na.rm = TRUE),
        )
    save(z, file = file)
    z
}


pf1 <- function(d, n = 5000, seg, noise = 1, gps = 3) {
    nseg <- 0
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
        # distance based on prev speed (for plotting)
        X[,1,i] <- X[,1,i-1] + delta[i] * X[,2,i-1]

        if (nseg > 0) {
            X[,2 + 1:nseg,i] <- t(apply(X[,,i-1], 1, est_tt,
                start = start, end = end, dt = delta[i]))
        }

        # lhood
        wt <- dexp(((y[i] - X[,1,i]) / gps)^2, 0.5)
        wt <- wt / sum(wt)
        wi <- sample(n, replace = TRUE, prob = wt)
        X[,,i] <- X[wi,,i]

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


pf2 <- function(d, n = 5000, seg, noise = 0.01, gps = 3) {
    nseg <- 0
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
        # distance based on prev speed (for plotting)
        X[,,i] <- X[,,i-1]
        if (nseg > 0)
            for (j in 1:nseg)
                X[, 2 + j, i] <- ifelse(X[, 2+j, i] > 0, NA, X[, 2+j, i])

        for (j in 1:delta[i]) {
            X[,2,i] <- truncnorm::rtruncnorm(1, 0, 30, X[,2,i], noise)
            if (nseg > 0) {
                X[,2 + 1:nseg,i] <- t(apply(X[,,i], 1, est_tt,
                    start = start, end = end, dt = 1, keep = TRUE))
            }
            X[,1,i] <- X[,1,i] + X[,2,i]
        }
        # lhood
        wt <- dexp(((y[i] - X[,1,i]) / gps)^2, 0.5)
        wt <- wt / sum(wt)
        wi <- sample(n, replace = TRUE, prob = wt)
        X[,,i] <- X[wi,,i]
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


pf3 <- function(d, n = 5000, seg, noise = 0.5, gps = 3) {
    nseg <- 0
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
        X[,,i] <- X[,,i-1]
        if (nseg > 0)
            for (j in 1:nseg)
                X[, 3 + j, i] <-
                    ifelse(X[, 3 + j, i] > 0, NA, X[, 3 + j, i])

        for (j in 1:delta[i]) {
            # v + a < 30 & v + a > 0
            # a < 30 - v & a > -v
            v <- X[,2,i]
            X[,3,i] <- truncnorm::rtruncnorm(1, -v, 30 - v, X[,3,i], noise)
            X[,2,i] <- X[,2,i] + X[,3,i]
            if (nseg > 0) {
                X[, 3 + 1:nseg,i] <- t(apply(X[,,i], 1, est_tt,
                    start = start, end = end, dt = 1, keep = TRUE, xdim = 3))
            }
            X[,1,i] <- X[,1,i] + X[,2,i]
        }
        # lhood
        wt <- dexp(((y[i] - X[,1,i]) / gps)^2, 0.5)
        wt <- wt / sum(wt)
        wi <- sample(n, replace = TRUE, prob = wt)
        X[,,i] <- X[wi,,i]
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
            if (x[1] < start[i]) {
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
            if (x[1] < start[i]) {
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
