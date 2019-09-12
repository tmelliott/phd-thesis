get_simulation <- function() {

    set.seed(107)

    transition <- function(x, t, q, delta, mu, peak = c(17.2, 18, 18.8)) {
        q <- delta * q
        if (t < peak[1])
            return(truncnorm::rtruncnorm(1, mu, Inf, x, q))
        if (t < peak[2])
            return(truncnorm::rtruncnorm(1, mu, Inf, x + q/2, q))
        if (t < peak[3])
            return(truncnorm::rtruncnorm(1, mu, Inf, x - q/2, q))
        truncnorm::rtruncnorm(1, mu, Inf, x, q)
    }

    t <- seq(6, 22, by = 5/60) # every 5 minutes
    mu <- 40
    q <- 0.15
    phi <- 50
    e <- 3
    beta <- numeric(length(t))
    beta[1] <- 150
    delta <- 5*60
    for (i in 2:length(beta))
        beta[i] <- transition(beta[i-1], t[i], q, delta, 50)


    tobs <- sample(seq(6, 22, by = 1/60), 500, replace = TRUE)
    ti <- sapply(tobs, function(tt) max(which(t <= tt)))
    zobs <- truncnorm::rtruncnorm(length(tobs), 45, Inf, beta[ti], phi)
    yobs <- rnorm(length(tobs), zobs, e)

    th <- tobs %/% 1
    tm <- (((tobs %% 1) * 60) %/% 5) * 5
    t30 <- th + tm / 60

    list(
        pars = list(mu = mu, phi = phi, q = q, e = e),
        truth = list(
            t = t,
            beta = beta
        ),
        t = tobs,
        t30 = t30,
        B = zobs,
        b = yobs
    )
}

# data <- get_simulation()
# par(mfrow=c(3,1))
# plot(data$truth$t, data$truth$beta, type = "l")
# plot(data$t, data$B)
# plot(data$t, data$b)

