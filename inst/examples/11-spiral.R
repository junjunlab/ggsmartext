## coord_spiral(): ggplot2 plotted along an Archimedean spiral
suppressMessages(library(ggplot2))
library(ggutils)

set.seed(42)
n <- 300
d <- data.frame(
  index = seq_len(n),
  x = seq(0, 1, length.out = n),
  y = sin(seq(0, 1, length.out = n) * 8 * pi) * 0.35 + 0.5,
  set = rep(c("A", "B", "C"), length.out = n),
  group = rep(c("g1", "g2"), each = n / 2)
)

## 1. a time series on three turns
p1 <- ggplot(d, aes(x, y)) +
  geom_line(colour = "#2166ac") +
  coord_spiral() +
  labs(title = "coord_spiral(): three turns")
ggsave("spiral-01-line.png", p1, width = 7, height = 7, dpi = 150)

## 2. bars: categories run along the spiral, value is the radial height
db <- data.frame(gene = factor(paste0("G", 1:24), levels = paste0("G", 1:24)),
                 value = runif(24, 0.2, 1))
p2 <- ggplot(db, aes(gene, value)) +
  geom_col(fill = "#5B8FF9") +
  coord_spiral(end = 360 + 180, height = 0.75) +
  labs(title = "Bars along the spiral")
ggsave("spiral-02-bars.png", p2, width = 7, height = 7, dpi = 150)

## 3. scale by curve length (equal data steps -> equal physical distance)
p3 <- ggplot(d, aes(x, y)) +
  geom_line(colour = "#b2182b") +
  coord_spiral(scale_by = "curve", clockwise = TRUE) +
  labs(title = "scale_by = 'curve', clockwise = TRUE")
ggsave("spiral-03-curve.png", p3, width = 7, height = 7, dpi = 150)

## 4. facet_wrap: one spiral per panel
p4 <- ggplot(d, aes(x, y)) +
  geom_line() +
  facet_wrap(~set) +
  coord_spiral(end = 360 + 90) +
  labs(title = "facet_wrap(): one spiral per panel")
ggsave("spiral-04-wrap.png", p4, width = 10, height = 5, dpi = 150)

## 5. facet_grid: shared x / row facets
p5 <- ggplot(d, aes(x, y)) +
  geom_line() +
  facet_grid(group ~ set) +
  coord_spiral(end = 360 + 90) +
  labs(title = "facet_grid(): 2 x 3 spirals")
ggsave("spiral-05-grid.png", p5, width = 11, height = 7, dpi = 150)
