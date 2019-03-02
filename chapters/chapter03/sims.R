library(tidyverse)
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
