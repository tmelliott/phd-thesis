library(rjags)
library(tidybayes)

source("scripts/load_nw_data.R")

segdata <-
    list(
        b = tts$travel_time,
        t = as.integer(as.factor(t30)),
        delta = diff(sort(unique(as.integer(t30)))),
        N = nrow(tts),
        M = length(unique(t30)),
        mu = mu[1]
    )


nw_fit_rda <- "data/nw_results.rda"
if (!file.exists(nw_fit_rda)) {
    t0 <- proc.time()
    n1_fit <-
        jags.model(
            "models/nw_model.jags",
            data = segdata,
            n.chains = 4,
            n.adapt = 10000,
            quiet = TRUE
        )

    n1_samples <-
        coda.samples(n1_fit,
            variable.names = c("beta", "phi", "q"),
            n.iter = 5000,
            thin = 5
        )
    time_mcmc <- proc.time() - t0

    attr(n1_samples, "time") <- time_mcmc
    save(n1_samples, file = nw_fit_rda)
} else {
    load(nw_fit_rda)
    time_mcmc <- attr(n1_samples, "time")
}
