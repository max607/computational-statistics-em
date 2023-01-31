# Data ---------------------------------------------------------------------------------------------

vec_sample <- read.table("data.txt") %>% unlist() %>% unname()
y_bar <- mean(vec_sample)

# Functions ----------------------------------------------------------------------------------------

## Simple form -------------------------------------------------------------------------------------

density1 <- function(y, theta, log = FALSE) {
  (theta * dgamma(y, shape = 1, rate = theta, log = log) +
    dgamma(y, shape = 2, rate = theta, log = log)) / (theta + 1)
}

loglik1 <- function(theta, y_bar) {
  2 * log(theta) - log(theta + 1) - y_bar * theta
}

d_loglik1 <- function(theta, y_bar) {
  2 * theta^(-1) - (theta + 1)^(-1) - y_bar
}

dd_loglik1 <- function(theta) {
  - 2 * theta^(-2) + (theta + 1)^(-2)
}

## Complex form ------------------------------------------------------------------------------------

density2 <- function(y, theta, lambda, p, log = FALSE) {
  p * density1(y, theta, log = log) + (1 - p) * dgamma(y, shape = 1, rate = lambda, log = log)
}

# complete_loglik <- function(theta, lambda, x, y) {
#   (lamda - theta) * sum(x * y) + (2 * log(theta) - log(theta + 1) - log(lambda)) * sum(x) -
#     lambda * sum(y) + log(lambda) * length(y)
# }

lambda_hat <- function(x, y) (length(y) - sum(x)) / (sum(y) - sum(x * y))

# Plots --------------------------------------------------------------------------------------------

## Animation of theta ------------------------------------------------------------------------------

# fps <- 30
# seconds <- 20

# p <- ggplot(data.frame(x = 1)) +
#   xlim(0, 15) +
#   ylim(0, 5)

# png("/tmp/gif-part%04d.png", width = 1600, height = 1200)
# for (i in exp(seq(log(0.01), log(100), length = fps * seconds))) {
#   print(
#     p +
#       geom_function(fun = f_y, args = list(theta = i), n = 501) +
#       labs(title = paste("theta:", i))
#   )
# }
# dev.off()

# paste("ffmpeg -r", fps, "-i /tmp/gif-part%04d.png output.mp4") %>%
#   system()

## ML estimator ------------------------------------------------------------------------------------

p1 <- ggplot() +
  geom_function(fun = loglik1, args = list(y_bar = y_bar), n = 501) +
  xlim(0, 10)

p2 <- ggplot() +
  geom_function(fun = d_loglik1, args = list(y_bar = y_bar), n = 501) +
  xlim(0.1, 10)

# Estimation ---------------------------------------------------------------------------------------

## Straight optimization ---------------------------------------------------------------------------

estimation_uniroot <- function(vec_data) {
  uniroot(d_loglik1, y_bar = mean(vec_data), interval = c(0, 10),
          tol = .Machine$double.eps^0.5)$root
}

theta_hat1 <- estimation_uniroot(vec_sample)

## Newton-Raphson ----------------------------------------------------------------------------------

newton_raphson <- function(theta0 = 1, dl = d_loglik1, ddl = dd_loglik1, ...) {
  while (abs(round(step <- dl(theta0, ...) / ddl(theta0), 8)) > 0) {
    theta0 <- theta0 - step
  }
  theta0
}

theta_hat2 <- newton_raphson(y_bar = y_bar)

## Plot --------------------------------------------------------------------------------------------

p3 <- p1 +
  geom_vline(xintercept = theta_hat1) +
  geom_vline(xintercept = theta_hat2, color = "red") +
  geom_hline(yintercept = loglik1(theta_hat2, y_bar))

# microbenchmark::microbenchmark(estimation_uniroot(vec_sample),
#                                newton_raphson(y_bar = y_bar),
#                                times = 1e4)

# Parametric bootstrap -----------------------------------------------------------------------------

my_exp <- function(n, theta) -log(runif(n)) / theta

## RNG1 --------------------------------------------------------------------------------------------

my_gamma <- function(shape, theta) sum(my_exp(shape, theta))

sampler1 <- function(n, theta) {
  shape <- ifelse(runif(n, 0, theta + 1) < 1, 2, 1)
  sapply(shape, my_gamma, theta = theta)
}

## RNG2 --------------------------------------------------------------------------------------------

sampler2 <- function(n, theta) {
  vec_res <- my_exp(n, theta)
  vec_shape2 <- my_exp(sum(runif(n, 0, theta + 1) < 1), theta)
  vec_res + c(vec_shape2, rep(0, length(vec_res) - length(vec_shape2)))
}

# microbenchmark::microbenchmark(sampler1(150, theta_hat), sampler2(150, theta_hat),
#                                times = 1e4)

## Standard error ----------------------------------------------------------------------------------

set.seed(1)
boot <- replicate(1e4, {
  sample_par <- sampler2(n = length(vec_sample), theta = theta_hat2)
  newton_raphson(y_bar = mean(sample_par))
})
boot.mean <- mean(boot)
boot.se <- sqrt(mean((boot - boot.mean)^2))
boot.bias <- boot.mean - theta_hat2

## EM ----------------------------------------------------------------------------------------------

em <- function(y, theta0, lambda0, p0) {
  ll <- 0; i = 1
  ll1 <- 0; ll2 <- 1
  while(round(abs(ll2 - ll1), 6) > 0) {

    # convergence based on observed likelihood 1
    ll1 <- sum(density2(y, theta0, lambda0, p0, log = TRUE))

    # expectation
    x <- p0 * density1(y, theta0) / density2(y, theta0, lambda0, p0)

    # maximization
    lambda0 <- lambda_hat(x, y)
    theta0 <- newton_raphson(y_bar = sum(x * y) / sum(x))  # TODO: average of y, where x == 1
    p0 <- mean(x)

    # convergence based on observed likelihood 2
    ll2 <- sum(density2(y, theta0, lambda0, p0, log = TRUE))

    # monitoring
    ll[[i]] <- ll2
    i <- i + 1
  }
  list(p_hat = p0, theta_hat = theta0, lambda_hat = lambda0, ll = ll)
}

# res <- em(vec_sample, theta0 = 1, lambda0 = 1, p0 = 0.5)
# plot(res$ll, type = "l")

