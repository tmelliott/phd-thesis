# test.R

dmnorm <- function(x, mu, sigma, wt)
    sapply(x, function(z) sum(wt * dnorm(z, mu, sigma)))

N <- 1000
X <- rnorm(N,50,10)
w <- dnorm(X,50,10)
w <- w/sum(w)
xx <- seq(0, 100, length = 10001)
dens <- dmnorm(xx, X, 10, w) # try 50 / N

plot(X, X*0, type = "p", xlim = c(0, 100), ylim = c(0, max(w, dens)),
    col = "blue")
lines(xx, dens, col = "red", lwd = 2)

# compare quantiles
q_theory <- qnorm(c(0.025, 0.975), 50, 15)
abline(v = q_theory, col = "black", lwd = 2)

q_pf <- quantile(X, c(0.025, 0.975))
abline(v = q_pf, col = "blue")

quantile_dens <- function(x, mu, sigma, wt)
    sapply(x, function(z) sum(wt * pnorm(z, mu, sigma)))
q_f <- function(q, x, mu, sigma, wt)
    (quantile_dens(x, mu, sigma, wt) - q)^2
q_dens <- sapply(c(0.025, 0.975),
    function(q)
        optimize(q_f, range(xx), q = q, mu = X, sigma = 10, wt = w)$minimum
)
abline(v = q_dens, col = "red")
