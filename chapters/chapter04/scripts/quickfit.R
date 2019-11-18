library(tidyverse)
library(rjags)
library(tidybayes)
load("data/data_week0.rda")

## fit some model to the data
sid <- names(sort(table(data_week0$segment_id), TRUE))[2]
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

ggplot(ds, aes(time, speed * 3.6, colour = weekday, group = date)) +
    geom_point() +
    geom_smooth(span = 0.4, se = FALSE) +
    geom_hline(yintercept = 50 + c(0, 10), lty = c(1, 2)) +
    scale_y_continuous(limits = c(0, 100))

ds1 <- ds %>% filter(date == "2019-08-12")
ggplot(ds1, aes(t, travel_time)) + geom_point()
ggplot(ds1, aes(t, speed * 3.6)) +
    geom_point() +
    geom_smooth(span = 0.4, se = FALSE) +
    geom_hline(yintercept = c(50, 60), lty = c(1, 2)) +
    scale_y_continuous(limits = c(0, 70))

segdata <-
    list(
        b = ds1$travel_time,
        t = as.integer(as.factor(ds1$t)),
        delta = diff(sort(unique(as.integer(ds1$t)))),
        N = nrow(ds1),
        M = length(unique(ds1$t)),
        Vmax = 60/3.6,
        L = ds1$length[1]
    )
dtimes <- sort(unique(ds1$t))

n1_fit <-
    jags.model(
        "models/quickfit.jags",
        data = segdata,
        n.chains = 4,
        n.adapt = 10000
        # quiet = TRUE
    )

## For checking, also take some samples of
## the vehicles' fitted Bs:
n1_B_samples.raw <-
    coda.samples(n1_fit,
        variable.names = "B",
        n.iter = 5000,
        thin = 5
    )
n1_B_samples <- n1_B_samples.raw %>%
    spread_draws(B[i]) %>%
    mutate(time = ds1$t[i])

## Graph sample of Bs with observed data:
n1_B_samples %>%
    sample_draws(50) %>%
    ggplot(aes(time, B)) +
    geom_point() +
    geom_point(aes(t, travel_time),
        data = ds1,
        colour = "red"
    )

## Sample the main model parameters
n1_samples.raw <-
    coda.samples(n1_fit,
        variable.names = c("beta", "phi", "q"),
        n.iter = 5000,
        thin = 5
    )
n1_samples <- n1_samples.raw %>%
    spread_draws(beta[t], phi, q) %>%
    mutate(time = dtimes[t])

n1_samples %>%
    filter(t == 1) %>%
    ggplot(aes(.iteration, group = .chain)) +
        geom_path(aes(y = phi))

n1_samples %>%
    filter(t == 1) %>%
    ggplot(aes(.iteration, group = .chain)) +
        geom_path(aes(y = q))

n1_samples %>%
    sample_draws(n = 50) %>%
    arrange(time) %>%
    ggplot(aes(time, beta, group = .draw)) +
        geom_path()

n1_estimate <- n1_samples %>%
    median_qi()

# transform from beta to travel time
# tt = L / V; V = Vmax / (1 + e^-eta)
# tt = L * (1 + e^-eta) / Vmax
ilogit <- function(eta)
    segdata$L * (1 + exp(-eta)) / segdata$Vmax
n1_res <- n1_estimate %>%
    mutate(
        eta_lower = qnorm(0.1, beta.lower, phi),
        eta_upper = qnorm(0.975, beta.upper, phi),
        mean = ilogit(beta),
        mean_lower = ilogit(beta.upper),
        mean_upper = ilogit(beta.lower),
        B_lower = ilogit(eta_upper),
        B_upper = ilogit(eta_lower)
    ) %>%
    arrange(time)

n1_res %>%
    ggplot(aes(time, beta)) +
        geom_ribbon(
            aes(ymin = eta_lower, ymax = eta_upper),
            fill = "orangered",
            alpha = 0.5
        ) +
        geom_ribbon(
            aes(ymin = beta.lower, ymax = beta.upper)
        ) +
        geom_path()

n1_res %>%
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
        aes(t, travel_time),
        data = ds1, size = 1
    )


n2_fit <-
    jags.model(
        "models/quickfit_linear.jags",
        data = segdata[c("b", "N", "Vmax", "L")],
        n.chains = 4,
        n.adapt = 10000
        # quiet = TRUE
    )

n2_B_samples.raw <-
    coda.samples(n2_fit,
        variable.names = "B",
        n.iter = 5000,
        thin = 5
    )
n2_B_samples <- n2_B_samples.raw %>%
    spread_draws(B[i]) %>%
    mutate(time = ds1$t[i])

n2_B_samples %>%
    sample_draws(50) %>%
    ggplot(aes(time, B)) +
    geom_point() +
    geom_point(aes(t, travel_time),
        data = ds1,
        colour = "red"
    )

n2_samples.raw <-
    coda.samples(n2_fit,
        variable.names = c("beta", "phi"),
        n.iter = 5000,
        thin = 5
    )
n2_samples <- n2_samples.raw %>%
    spread_draws(beta, phi)

n2_samples %>%
    ggplot(aes(.iteration, group = .chain)) +
    geom_path(aes(y = phi))

n2_samples %>%
    ggplot(aes(beta)) +
    geom_histogram()

n2_estimate <- n2_samples %>%
    median_qi()

# transform from beta to travel time
# tt = L / V; V = Vmax / (1 + e^-eta)
# tt = L * (1 + e^-eta) / Vmax
ilogit <- function(eta)
    segdata$L * (1 + exp(-eta)) / segdata$Vmax
n2_res <- n2_estimate %>%
    mutate(
        eta_lower = qnorm(0.1, beta.lower, phi),
        eta_upper = qnorm(0.975, beta.upper, phi),
        mean = ilogit(beta),
        mean_lower = ilogit(beta.upper),
        mean_upper = ilogit(beta.lower),
        B_lower = ilogit(eta_upper),
        B_upper = ilogit(eta_lower),
        tmin = min(dtimes),
        tmax = max(dtimes)
    )

# n1_res %>%
#     ggplot() +
#     geom_ribbon(
#         aes(ymin = eta_lower, ymax = eta_upper),
#         fill = "orangered",
#         alpha = 0.5
#     ) +
#     geom_ribbon(
#         aes(ymin = beta.lower, ymax = beta.upper)
#     ) +
#     geom_path()

n2_res %>%
    ggplot() +
    geom_rect(
        aes(
            xmin = tmin, xmax = tmax,
            ymin = B_lower, ymax = B_upper
        ),
        colour = "gray", alpha = 0.25
    ) +
    geom_rect(
        aes(
            xmin = tmin, xmax = tmax,
            ymin = mean_lower, ymax = mean_upper
        ),
        fill = "orangered", alpha = 0.5
    ) +
    geom_hline(aes(yintercept = mean), colour = "orangered") +
    geom_point(
        aes(t, travel_time),
        data = ds1, size = 1
    )


## Compare!
n1_dic <- dic.samples(n1_fit, n.iter = 5000, thin = 5)
n2_dic <- dic.samples(n2_fit, n.iter = 5000, thin = 5)

n1_dic
n2_dic
diffdic(n1_dic, n2_dic)


## compare?
n1_B_samples %>%
    mutate(b = segdata$b[t])