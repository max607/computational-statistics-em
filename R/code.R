# Data ---------------------------------------------------------------------------------------------

vec_sample <- read.table("data.txt") %>% unlist() %>% unname()
y_bar <- mean(vec_sample)

# Functions ----------------------------------------------------------------------------------------

f_y <- function(y, theta, log = FALSE) {
  theta^2 / (theta + 1) * (1 + y) * exp(-theta * y)
}

loglik <- function(theta, y_bar) {
  2 * log(theta) - log(theta + 1) - y_bar * theta
}

d_loglik <- function(theta, y_bar) {
  2 * theta^(-1) - (theta + 1)^(-1) - y_bar
}

dd_loglik <- function(theta) {
  - 2 * theta^(-2) + (theta + 1)^(-2)
}

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
  geom_function(fun = loglik, args = list(y_bar = y_bar), n = 501) +
  xlim(0, 10)

p2 <- ggplot() +
  geom_function(fun = d_loglik, args = list(y_bar = y_bar), n = 501) +
  xlim(0.1, 10)

# Estimation ---------------------------------------------------------------------------------------

## Straight optimization ---------------------------------------------------------------------------

estimation_uniroot <- function(vec_data) {
  uniroot(d_loglik, y_bar = mean(vec_data), interval = c(0, 10),
          tol = .Machine$double.eps^0.5)$root
}

theta_hat1 <- estimation_uniroot(vec_sample)

## Newton-Raphson ----------------------------------------------------------------------------------

newton_raphson <- function(theta0 = 1, dl = d_loglik, ddl = dd_loglik, ...) {
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
  geom_hline(yintercept = loglik(theta_hat2, y_bar))

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

