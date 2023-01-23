# Functions ----------------------------------------------------------------------------------------

my_df <- function(x, theta, log = FALSE) {
  theta^2 / (theta + 1) * (1 + x) * exp(-theta * x)
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


