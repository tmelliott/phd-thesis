get_simulation <- function() {

    set.seed(107)

    transition <- function(x, t, q, peak = c(8, 1)) {
        #z <- pmin(0.5, dnorm(t, peak[1], peak[2]) / dnorm(0, 0, peak[2]))
        #if (t > peak[1]) z <- -z
        #in_peak <- dplyr::between(t, peak[1] - peak[2], peak[1] + peak[2])
        # 1: off-peak, 2: prepeak, 3: postpeak
        #rnorm(1, if (in_peak) x + z*q else log(20), q)
        rnorm(1, x, q)
    }

    t <- seq(6, 22, by = 5/60)
    mu <- 40
    q <- 0.2
    phi <- 20
    e <- 5
    beta <- numeric(length(t))
    beta[1] <- log(20)
    for (i in 2:length(beta))
        beta[i] <- transition(beta[i-1], t[i], q)


    tobs <- sample(seq(6, 22, by = 1/60), 500, replace = TRUE)
    ti <- sapply(tobs, function(tt) max(which(t <= tt)))
    zobs <- truncnorm::rtruncnorm(length(tobs), 20, Inf,
        mu + exp(beta[ti]), phi)
    yobs <- rnorm(length(tobs), zobs, e)

    list(
        pars = list(mu = mu, phi = phi, q = q, e = e),
        truth = list(
            t = t,
            beta = beta
        ),
        t = tobs,
        B = zobs,
        b = yobs
    )
}

