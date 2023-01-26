# Data ---------------------------------------------------------------------------------------------

vec_sample <- read.table("data.txt") %>% unlist() %>% unname()

# Functions ----------------------------------------------------------------------------------------

my_df <- function(x, theta, log = FALSE) {
  theta^2 / (theta + 1) * (1 + x) * exp(-theta * x)
}

ml_estimation <- function(theta, x_bar) {
  (theta + 2) / (theta * (theta + 1)) - x_bar
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
#       geom_function(fun = my_df, args = list(theta = i), n = 501) +
#       labs(title = paste("theta:", i))
#   )
# }
# dev.off()

# paste("ffmpeg -r", fps, "-i /tmp/gif-part%04d.png output.mp4") %>%
#   system()

## ML estimator ------------------------------------------------------------------------------------

ggplot(data.table(x = 1)) +
  geom_function(fun = function(theta) (theta + 2) / (theta * (theta + 1)), n = 501) +
  geom_hline(yintercept = mean(vec_sample)) +
  xlim(0, 10)

# Estimation ---------------------------------------------------------------------------------------

## Using R functions -------------------------------------------------------------------------------

my_estimation <- function(vec_data) {  # TODO: replace with gradient acent
  uniroot(ml_estimation, x_bar = mean(vec_data), interval = c(0, 10),
          tol = .Machine$double.eps^0.5)$root
}

theta_hat <- my_estimation(vec_sample)

# Parametric bootstrap -----------------------------------------------------------------------------

## RNG1 --------------------------------------------------------------------------------------------

my_exp <- function(n, theta) -log(runif(n)) / theta

my_gamma <- function(shape, theta) sum(my_exp(shape, theta))

my_sampler <- function(n, theta) {
  shape <- ifelse(runif(n, 0, theta + 1) < 1, 2, 1)
  sapply(shape, my_gamma, theta = theta)
}

## RNG2 --------------------------------------------------------------------------------------------

my_sampler2 <- function(n, theta) {
  vec_res <- my_exp(n, theta)
  vec_shape2 <- my_exp(sum(runif(n, 0, theta + 1) < 1), theta)
  vec_res + c(vec_shape2, rep(0, length(vec_res) - length(vec_shape2)))
}

# microbenchmark::microbenchmark(my_sampler(150, theta_hat), my_sampler2(150, theta_hat),
#                                times = 1e4)

## Standard error ----------------------------------------------------------------------------------

set.seed(1)
ls_bss <- replicate(1e4, my_sampler2(n = length(vec_sample), theta = theta_hat),
                    simplify = FALSE)
vec_theta_hat <- sapply(ls_bss, my_estimation)
sd(vec_theta_hat)

