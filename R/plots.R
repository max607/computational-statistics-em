# Data ---------------------------------------------------------------------------------------------

# p1 <- ggplot(data.table(y = vec_sample), aes(y)) +
#   geom_histogram(color = "black", fill = "white", bins = 50) +
#   geom_vline(xintercept = y_bar) +
#   ylab("Count")

# Animation of theta -------------------------------------------------------------------------------

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

# ML estimator -------------------------------------------------------------------------------------

# p6 <- ggplot() +
#   geom_function(fun = loglik1, args = list(y_bar = y_bar), n = 501) +
#   xlim(0, 10)

# p2 <- ggplot() +
#   geom_function(fun = d_loglik1, args = list(y_bar = y_bar), n = 501) +
#   xlim(0.1, 10)

# Plot ---------------------------------------------------------------------------------------------

# p3 <- p1 +
#   geom_vline(xintercept = theta_hat1) +
#   geom_vline(xintercept = theta_hat2, color = "red") +
#   geom_hline(yintercept = loglik1(theta_hat2, y_bar))

