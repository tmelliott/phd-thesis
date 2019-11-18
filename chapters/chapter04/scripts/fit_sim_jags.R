library(rjags)
library(tidybayes)
library(tidyverse)

source("scripts/sim_data.R")
simdata <- get_simulation(mu = 50, q = 0.002, phi = 0.8 * 3.6^2, e = 10, beta0 = 30)

simdata_jags <-
    list(
        b = simdata$b,
        t = as.integer(as.factor(simdata$t30)),
        delta = diff(sort(unique(simdata$t30))) * 60 * 60,
        N = length(simdata$b),
        M = length(unique(simdata$t30)),
        mu = 60,
        E = simdata$pars$e
    )

sim_rda <- "data/sim_results.rda"
if (!file.exists(sim_rda)) {
    sim_fit <-
        jags.model(
            "models/nw_model.jags",
            data = simdata_jags,
            n.chains = 4,
            n.adapt = 10000,
            quiet = TRUE
        )

    sim_samples <-
        coda.samples(sim_fit,
            variable.names = c("beta", "phi", "q"),
            n.iter = 5000,
            thin = 5
        )

    save(sim_samples, file = sim_rda)
} else {
    load(sim_rda)
}
