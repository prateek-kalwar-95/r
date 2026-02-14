library(ggplot2)
library(animint2)

set.seed(101)
R <- 1
nsteps <- 300
nballs <- 15
sigma <- 0.05 * R

reflect <- function(x0, y0, dx, dy, R) {
  x1 <- x0 + dx
  y1 <- y0 + dy
  d <- sqrt(x1^2 + y1^2)

  if (d <= R) {
    return(c(x1, y1))
  }

  # unit normal
  n <- c(x1, y1) / d
  v <- c(dx, dy)

  # specular reflection
  v_reflect <- v - 2 * sum(v * n) * n

  c(x0 + v_reflect[1], y0 + v_reflect[2])
}

sim <- data.frame(
  step = 1,
  id   = 1:nballs,
  x    = rnorm(nballs, 0, 0.1 * R),
  y    = rnorm(nballs, 0, 0.1 * R)
)
for (i in 2:nsteps) {
  prev <- sim[sim$step == i - 1, ]
  next_step <- prev

  for (j in 1:nballs) {
    dx <- rnorm(1, 0, sigma)
    dy <- rnorm(1, 0, sigma)

    pos <- reflect(prev$x[j], prev$y[j], dx, dy, R)

    next_step$x[j] <- pos[1]
    next_step$y[j] <- pos[2]
  }

  next_step$step <- i
  sim <- rbind(sim, next_step)
}
theta <- seq(0, 2 * pi, length.out = 1000)
circle <- data.frame(
  x = R * cos(theta),
  y = R * sin(theta)
)

p <- ggplot() +
  geom_path(
    data = circle,
    aes(x, y),
    size = 1
  ) +
  geom_point(
    data = sim,
    aes(x, y, colour = factor(id)),
    showSelected = "step",
    clickSelects = "id",
    size = 6,
    alpha = 0.9
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )

viz <- animint(
  brownian = p,
  time = list(variable = "step", ms = 80)
)

animint2dir(
  viz,
  out.dir = "brownian-motion",
  open.browser = TRUE
)
