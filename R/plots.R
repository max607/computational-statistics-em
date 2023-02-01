# Data ---------------------------------------------------------------------------------------------

p1 <- ggplot(data.table(y = vec_sample), aes(y)) +
  geom_histogram(color = "black", fill = "white", bins = 50) +
  geom_vline(xintercept = mean(vec_sample)) +
  ylab("Count")

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
#       geom_function(fun = pdf_theta, args = list(theta = i), n = 501) +
#       labs(title = paste("theta:", i))
#   )
# }
# dev.off()

# paste("ffmpeg -r", fps, "-i /tmp/gif-part%04d.png output.mp4") %>%
#   system()

# EM -----------------------------------------------------------------------------------------------

## x and y -----------------------------------------------------------------------------------------

p2 <- ggplot(data.table(x = em.run$x, y = vec_sample), aes(x, y)) +
  geom_point(alpha = 0.5)

## Convergence -------------------------------------------------------------------------------------

dt_monitoring <- em.run$monitoring

p3 <- melt(dt_monitoring, id.vars = c("id", "xi")) %>%
  ggplot(aes(id, value)) +
  geom_line() +
  facet_wrap("variable", scales = "free_y") +
  xlab("Iteration") + ylab("Value")

p4 <- dt_monitoring[, do.call(cbind, xi)] %>%
  as.data.table() %>%
  .[, id := seq_len(.N)] %>%
  melt(id.vars = "id") %>%
  .[, variable := as.numeric(factor(variable))] %>%
  ggplot(aes(variable, value, group = id)) +
  geom_line(alpha = 0.5) +
  xlab("Iteration") + ylab("x")

