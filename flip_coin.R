library(ggplot2)
library(animint2)
set.seed(123)
faces <- c("Head", "Stand", "Tail")
prob <- c(0.45, 0.10, 0.45)
n_max <- 100

toss_result <- sample(faces, size = n_max, replace = TRUE, prob = prob)

blocks_freq <- do.call(rbind, lapply(1:n_max, function(i) {
  do.call(rbind, lapply(faces, function(f) {
    count_f <- sum(toss_result[1:i] == f)
    if (count_f > 0) {
      block_height <- 1 / n_max
      data.frame(
        toss = i,
        face = f,
        ymin = (0:(count_f - 1)) * block_height,
        ymax = (1:count_f) * block_height
      )
    }
  }))
}))

blocks_freq$face <- factor(blocks_freq$face, levels = faces)

face_pos <- c(1.25, 3.75, 6.25)
names(face_pos) <- faces
blocks_freq$xpos <- face_pos[as.character(blocks_freq$face)]

scatter_positions <- data.frame(
  face = toss_result,
  x = runif(n_max, 8.2, 16),
  y = runif(n_max, 0.01, 0.51)
)

scatter_positions$face <- factor(scatter_positions$face, levels = faces)

scatter_data <- do.call(rbind, lapply(1:n_max, function(i) {
  df <- scatter_positions[1:i, ]
  df$toss <- i
  df
}))

counter_data <- data.frame(
  toss = 1:n_max,
  label = paste("Number of Tosses :", 1:n_max),
  x = 11,
  y = -0.035
)


bar_labels <- do.call(rbind, lapply(1:n_max, function(i) {
  do.call(rbind, lapply(faces, function(f) {
    count_f <- sum(toss_result[1:i] == f)
    # Position: Fixed at top (0.54)
    data.frame(
      toss = i,
      face = f,
      y = 0.54,
      label = paste0(count_f, " (", sprintf("%.2f", count_f / n_max), ")")
    )
  }))
}))
bar_labels$xpos <- face_pos[as.character(bar_labels$face)]

vertical_text_data <- data.frame(
  x = 17.2,
  y = 0.25,
  label = "Flip 'coins'"
)
p <- ggplot() +
  # Left vertical divider
  annotate("segment", x = 7.5, xend = 7.5, y = 0, yend = 0.525, size = 1.2) +
  geom_text(
    data = vertical_text_data,
    aes(x = x, y = y, label = label),
    angle = 90,
    size = 15,
    fontface = "bold",
    color = "black"
  ) +

  # Manual Border for Main Plot Area (y=0 to 0.525)
  annotate("rect", xmin = 0, xmax = 16.5, ymin = 0, ymax = 0.525, fill = NA, color = "black", size = 0.5) +

  # Manual X-axis Ticks and Labels
  annotate("segment", x = face_pos, xend = face_pos, y = 0, yend = -0.015) +
  annotate("text", x = face_pos, y = -0.04, label = names(face_pos), size = 15) +
  geom_rect(
    data = blocks_freq,
    aes(
      xmin = xpos - 1.2,
      xmax = xpos + 1.2,
      ymin = ymin,
      ymax = ymax,
      fill = face
    ),
    color = "white",
    size = 0.2,
    showSelected = "toss"
  ) +
  geom_text(
    data = scatter_data,
    aes(x = x, y = y, label = face, color = face),
    showSelected = "toss",
    fontface = "bold",
    alpha = 0.8
  ) +
  scale_fill_manual(values = c(
    Head = "black",
    Stand = "red",
    Tail = "blue"
  )) +
  scale_color_manual(values = c(
    Head = "black",
    Stand = "red",
    Tail = "blue"
  )) +
  guides(fill = "none") +
  scale_y_continuous(
    limits = c(-0.1, 0.57),
    breaks = seq(0, 0.5, 0.1),
    labels = function(x) ifelse(x < 0, "", x),
    expand = c(0, 0)
  ) +
  geom_text(
    data = counter_data,
    aes(x = 12.0, y = -0.04, label = label),
    showSelected = "toss",
    size = 15,
    fontface = "bold"
  ) +
  geom_text(
    data = bar_labels,
    aes(x = xpos, y = y, label = label),
    showSelected = "toss",
    size = 15,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0, 19.5),
    breaks = NULL, # Hide defaults
    expand = c(0, 0)
  ) +
  labs(
    y = "Frequency",
    x = NULL,
    caption = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 5, r = 5, b = 10, l = 5),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  ) +
  theme_animint(width = 600)


viz <- animint(
  coin = p,
  time = list(variable = "toss", ms = 300),
  selector = list(caption = list(
    update = function(toss) {
      paste("Number of Tosses :", toss)
    }
  ))
)

animint2dir(viz, out.dir = "coin-flip", open.browser = TRUE)
