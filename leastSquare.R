library(ggplot2)
library(animint2)

set.seed(123)
n <- 20
x <- seq(1, 10, length.out = n)
y <- 2 + 1.5 * x + rnorm(n, 0, 2)

fit <- lm(y ~ x)
best_intercept <- coef(fit)[1]
best_slope <- coef(fit)[2]

slope_seq <- seq(best_slope - 2, best_slope + 2, length.out = 50)

anim_data <- do.call(rbind, lapply(seq_along(slope_seq), function(t) {
  slope <- slope_seq[t]
  intercept <- mean(y) - slope * mean(x)

  y_hat <- intercept + slope * x
  SSE <- sum((y - y_hat)^2)

  data.frame(
    x = x,
    y = y,
    y_hat = y_hat,
    slope = slope,
    intercept = intercept,
    SSE = SSE,
    frame = t,
    animation_frame = t
  )
}))

# Unique slope-level data
sse_data <- unique(anim_data[, c("slope", "intercept", "SSE", "frame")])
sse_data$animation_frame <- sse_data$frame

sse_cum <- do.call(rbind, lapply(1:50, function(t) {
  temp <- sse_data[sse_data$frame <= t, ]
  temp$animation_frame <- t
  # Calculate max RSS at this frame for dynamic y-axis effect
  temp$max_rss_at_frame <- max(temp$SSE)
  temp
}))

annotation_data <- unique(anim_data[, c("slope", "intercept", "SSE", "frame")])
annotation_data$animation_frame <- annotation_data$frame
annotation_data$label_slope <- sprintf("Slope: %.3f", annotation_data$slope)

p1 <- ggplot(anim_data, aes(x, y)) +
  geom_point(
    size = 2,
    shape = 21,
    stroke = 1.2,
    fill = "white",
    color = "black"
  ) +
  geom_abline(
    intercept = best_intercept,
    slope = best_slope,
    color = "gray80",
    size = 1.5
  ) +
  geom_line(
    aes(y = y_hat, group = animation_frame),
    showSelected = "animation_frame",
    color = "blue",
    size = 1.2
  ) +
  geom_segment(
    aes(
      x = x, y = y,
      xend = x, yend = y_hat,
      group = animation_frame
    ),
    showSelected = "animation_frame",
    color = "#FF4444", # Brighter red for better visibility
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_point(
    aes(y = y_hat, group = animation_frame),
    showSelected = "animation_frame",
    shape = 16,
    size = 2,
    color = "black"
  ) +
  scale_y_continuous(
    limits = c(-0.5, 18.5),
    breaks = seq(0, 18, 2),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(-0.5, 11),
    breaks = seq(0, 10, 2),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 14) +
  theme(aspect.ratio = 0.5) +
  labs(
    title = NULL,
    x = "X",
    y = "Y"
  )


# Calculate max RSS for manual border
max_rss <- max(sse_data$SSE)
# Define x-axis ticks manually
x_ticks <- seq(0, 50, 10)

p2 <- ggplot(sse_cum, aes(frame, SSE)) +
  # Manual Border for Main Plot Area
  annotate("rect",
    xmin = 0, xmax = 51, ymin = 0, ymax = max_rss * 1.05,
    fill = NA, color = "black", size = 0.5
  ) +

  # Manual X-axis Ticks
  annotate("segment",
    x = x_ticks, xend = x_ticks,
    y = 0, yend = -max_rss * 0.03
  ) +

  # Manual X-axis Labels
  annotate("text",
    x = x_ticks, y = -max_rss * 0.08,
    label = x_ticks, size = 15, shape = 16
  ) +
  geom_line(
    aes(key = 1),
    showSelected = "animation_frame",
    size = 1.2,
    color = "black"
  ) +
  geom_point(
    aes(key = frame),
    showSelected = "animation_frame",
    size = 2.5,
    color = "black"
  ) +
  geom_point(
    data = sse_data,
    aes(frame, SSE, key = 1),
    showSelected = "animation_frame",
    size = 5,
    shape = 16,
    color = "black"
  ) +
  # Slope Annotation
  geom_text(
    data = annotation_data,
    aes(x = 25, y = -max_rss * 0.2, label = label_slope, key = 1),
    showSelected = "animation_frame",
    hjust = 0.5,
    vjust = 0.5,
    size = 15,
    shape = 16,
    color = "black",
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(-max_rss * 0.2, max_rss * 1.05),
    breaks = pretty(c(0, max_rss), n = 4),
    labels = function(x) ifelse(x < 0, "", x),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 51),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 40, 10)
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Residual Sum of Squares (RSS)"
  )


intercept_seq <- seq(best_intercept - 3, best_intercept + 3, length.out = 50)

anim_data_int <- do.call(rbind, lapply(seq_along(intercept_seq), function(t) {
  intercept <- intercept_seq[t]
  slope <- best_slope # Fixed slope

  y_hat <- intercept + slope * x
  SSE <- sum((y - y_hat)^2)

  data.frame(
    x = x,
    y = y,
    y_hat = y_hat,
    slope = slope,
    intercept = intercept,
    SSE = SSE,
    frame = t,
    animation_frame = t
  )
}))

# Unique intercept-level data
sse_data_int <- unique(anim_data_int[, c("slope", "intercept", "SSE", "frame")])
sse_data_int$animation_frame <- sse_data_int$frame

# Cumulative RSS for intercept
sse_cum_int <- do.call(rbind, lapply(1:50, function(t) {
  temp <- sse_data_int[sse_data_int$frame <= t, ]
  temp$animation_frame <- t
  temp
}))

# Annotations for Intercept
annot_data_int <- unique(anim_data_int[, c("slope", "intercept", "SSE", "frame")])
annot_data_int$animation_frame <- annot_data_int$frame
annot_data_int$label_intercept <- sprintf("Intercept: %.3f", annot_data_int$intercept)

# Min RSS for Intercept
max_rss_int <- max(sse_data_int$SSE)
x_ticks_int <- seq(0, 50, 10)

p3 <- ggplot(anim_data_int, aes(x, y)) +
  geom_point(
    size = 2,
    shape = 21,
    stroke = 1.2,
    fill = "white",
    color = "black"
  ) +
  geom_abline(
    intercept = best_intercept,
    slope = best_slope,
    color = "gray80",
    size = 1.5
  ) +
  geom_line(
    aes(y = y_hat, group = animation_frame),
    showSelected = "animation_frame",
    color = "blue",
    size = 1.2
  ) +
  geom_segment(
    aes(
      x = x, y = y,
      xend = x, yend = y_hat,
      group = animation_frame
    ),
    showSelected = "animation_frame",
    color = "#FF4444",
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_point(
    aes(y = y_hat, group = animation_frame),
    showSelected = "animation_frame",
    shape = 16,
    size = 2,
    color = "black"
  ) +
  scale_y_continuous(
    limits = c(-0.5, 18.5),
    breaks = seq(0, 18, 2),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(-0.5, 11),
    breaks = seq(0, 10, 2),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 14) +
  labs(
    title = NULL,
    x = "X",
    y = "Y"
  )

p4 <- ggplot(sse_cum_int, aes(frame, SSE)) +
  # Manual Border
  annotate("rect",
    xmin = 0, xmax = 51, ymin = 0, ymax = max_rss_int * 1.05,
    fill = NA, color = "black", size = 0.5
  ) +
  # Manual X-axis Ticks
  annotate("segment",
    x = x_ticks_int, xend = x_ticks_int,
    y = 0, yend = -max_rss_int * 0.03
  ) +
  # Manual X-axis Labels
  annotate("text",
    x = x_ticks_int, y = -max_rss_int * 0.08,
    label = x_ticks_int, size = 15, shape = 16
  ) +
  geom_line(
    aes(key = 1),
    showSelected = "animation_frame",
    size = 1.2,
    color = "black"
  ) +
  geom_point(
    aes(key = frame),
    showSelected = "animation_frame",
    size = 2.5,
    color = "black"
  ) +
  geom_point(
    data = sse_data_int,
    aes(frame, SSE, key = 1),
    showSelected = "animation_frame",
    size = 5,
    shape = 16,
    color = "black"
  ) +
  # Intercept Annotation
  geom_text(
    data = annot_data_int,
    aes(x = 25, y = -max_rss_int * 0.2, label = label_intercept, key = 1),
    showSelected = "animation_frame",
    hjust = 0.5,
    vjust = 0.5,
    size = 15,
    shape = 16,
    color = "black",
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(-max_rss_int * 0.3, max_rss_int * 1.05),
    breaks = pretty(c(0, max_rss_int), n = 6),
    labels = function(x) ifelse(x < 0, "", x),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 51),
    expand = c(0, 0)
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 40, 10)
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Residual Sum of Squares (RSS)"
  )

viz <- animint(
  slopePlot = p1,
  slopeRSS = p2,
  interceptPlot = p3,
  interceptRSS = p4,
  time = list(variable = "animation_frame", ms = 300)
)

animint2dir(
  viz,
  out.dir = "leastSquare",
  open.browser = TRUE
)
