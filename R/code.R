# Likelihood ---------------------------------------------------------------------------------------

pdf_theta <- function(y, theta, log = FALSE) {
  (theta * dgamma(y, shape = 1, rate = theta, log = log) +
    dgamma(y, shape = 2, rate = theta, log = log)) / (theta + 1)
}

pdf_lambda <- function(y, lambda, log = FALSE) {
  dgamma(y, shape = 1, rate = lambda, log = log)
}

pdf_p <- function(y, p, theta, lambda, log = FALSE) {
  p * pdf_theta(y, theta, log) + (1 - p) * pdf_lambda(y, lambda, log)
}

loglik_theta <- function(theta, y_agg) {
  2 * log(theta) - log(theta + 1) - theta * y_agg
}

loglik_theta.d <- function(theta, y_agg) {
  2 * theta^(-1) - (theta + 1)^(-1) - y_agg
}

loglik_theta.dd <- function(theta) {
  - 2 * theta^(-2) + (theta + 1)^(-2)
}


# Parameter estimation -----------------------------------------------------------------------------

## Theta -------------------------------------------------------------------------------------------

newton_raphson <- function(theta0 = 1, dl = loglik_theta.d, ddl = loglik_theta.dd, ...) {
  while (abs(round(step <- dl(theta0, ...) / ddl(theta0), 8)) > 0) {
    theta0 <- theta0 - step
  }
  theta0
}

## Lambda ------------------------------------------------------------------------------------------

lambda_argmax <- function(x, y) {
  (length(y) - sum(x)) / (sum(y) - sum(x * y))
}

## Pi ----------------------------------------------------------------------------------------------

p_argmax <- function(x) {
  mean(x)
}

## E of X ------------------------------------------------------------------------------------------

x_expectation <- function(y, p, theta, lambda) {
  p * pdf_theta(y, theta) / pdf_p(y, p, theta, lambda)
}

# RNG ----------------------------------------------------------------------------------------------

my_exp <- function(n, theta) {
  -log(runif(n)) / theta
}

sample_pdf_theta <- function(n, theta) {
  y <- my_exp(n, theta)
  y_shape2 <- my_exp(sum(runif(n, 0, theta + 1) < 1), theta)
  y + c(y_shape2, rep(0, length(y) - length(y_shape2)))
}

# Parametric bootstrap -----------------------------------------------------------------------------

se_theta <- function(y, B = 1e4, theta_hat) {
  n <- length(y)
  boot <- replicate(B, newton_raphson(y_agg = mean(sample_pdf_theta(n, theta_hat))))
  sqrt(mean((boot - mean(boot))^2))
}

# EM -----------------------------------------------------------------------------------------------

em <- function(y, p0 = 0.5, theta0 = 1, lambda0 = 1) {

  # monitoring
  i = 1
  dt_params <- data.table(id = seq_len(8000), p = 0, theta = 0, lambda = 0, loglik = 0,
                          xi = vector("list", 8000))

  # convergence
  ll1 <- 0; ll2 <- 1

  while(round(abs((ll2 - ll1) / ll2), 6) > 0) {

    # convergence based on observed likelihood 1
    ll1 <- sum(pdf_p(y, p0, theta0, lambda0, log = TRUE))

    # expectation
    x <- x_expectation(y, p0, theta0, lambda0)

    # maximization
    p0 <- p_argmax(x)
    lambda0 <- lambda_argmax(x, y)
    theta0 <- newton_raphson(y_agg = sum(x * y) / sum(x))

    # convergence based on observed likelihood 2
    ll2 <- sum(pdf_p(y, p0, theta0, lambda0, log = TRUE))

    # monitoring
    dt_params[i, c("p", "theta", "lambda", "loglik", "xi") := .(p0, theta0, lambda0, ll2, list(x))]
    i <- i + 1
  }
  dt_params <- dt_params[!sapply(xi, is.null),]
  list(p_hat = p0, theta_hat = theta0, lambda_hat = lambda0, x = x, monitoring = dt_params)
}

# Application --------------------------------------------------------------------------------------

## 1 -----------------------------------------------------------------------------------------------

theta_hat <- newton_raphson(y_agg = mean(vec_sample))

## 2 -----------------------------------------------------------------------------------------------

theta_hat.se <- se_theta(vec_sample, B = 1e4, theta_hat)

## 3 -----------------------------------------------------------------------------------------------

em.run <- em(vec_sample, p0 = 0.5, theta0 = 1, lambda0 = 1)

