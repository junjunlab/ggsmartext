## Internal helpers shared by the guide and the algorithms.
## Nothing in this file is exported.

## Device size in cm with a safe fallback.  Used only to *estimate* how many
## label rows are needed before the real panel width is known.  Growing the
## window (or `ggsave(width = ...)`) rescales the device, so the estimate stays
## in the right ballpark.
.ggse_device_size_cm <- function() {
  size <- tryCatch(grDevices::dev.size("cm"), error = function(e) NULL)
  if (is.null(size) || length(size) != 2L || !all(is.finite(size))) {
    size <- c(14, 9)
  }
  size
}

## Rotated bounding box of a text box: an `angle` of 45 degrees inflates both
## the width and the height of the box the label occupies.
.ggse_rotate_box <- function(width, height, angle) {
  theta <- abs(angle) %% 180
  theta <- theta * pi / 180
  if (theta > pi / 2) {
    theta <- pi - theta
  }
  cos_t <- cos(theta)
  sin_t <- sin(theta)
  list(
    width  = width * cos_t + height * sin_t,
    height = width * sin_t + height * cos_t
  )
}

## Size of label boxes in cm, given the graphical parameters of `axis.text`.
## The measurement is done with `stringWidth()`/`stringHeight()` inside a
## viewport that carries the requested font, which is much cheaper than
## creating one text grob per label.
.ggse_text_box_cm <- function(labels, size = 11, colour = "black", family = "",
                              face = 1, lineheight = 1.2, angle = 0) {
  n <- length(labels)
  if (n == 0L) {
    return(list(width = numeric(), height = numeric()))
  }
  labels <- as.character(labels)
  gp <- grid::gpar(
    fontsize   = size,
    col        = colour,
    fontfamily = family,
    fontface   = face,
    lineheight = lineheight
  )
  measured <- tryCatch({
    grid::pushViewport(grid::viewport(gp = gp))
    on.exit(grid::popViewport(), add = TRUE)
    list(
      width  = grid::convertWidth(grid::stringWidth(labels), "cm", valueOnly = TRUE),
      height = grid::convertHeight(grid::stringHeight(labels), "cm", valueOnly = TRUE)
    )
  }, error = function(e) NULL)

  if (is.null(measured)) {
    ## Extremely defensive fallback: measure one grob per label.
    measured <- list(
      width  = vapply(labels, function(l) {
        grid::convertWidth(
          grid::grobWidth(grid::textGrob(l, gp = gp)), "cm", valueOnly = TRUE
        )
      }, numeric(1)),
      height = vapply(labels, function(l) {
        grid::convertHeight(
          grid::grobHeight(grid::textGrob(l, gp = gp)), "cm", valueOnly = TRUE
        )
      }, numeric(1))
    )
  }

  measured$width[!is.finite(measured$width)] <- 0
  measured$height[!is.finite(measured$height)] <- 0
  .ggse_rotate_box(measured$width, measured$height, angle)
}

## Translate a resolved `element_text` into the pieces we need for drawing.
.ggse_text_style <- function(element) {
  if (is.null(element) || ggplot2::is_theme_element(element, "blank")) {
    return(NULL)
  }
  size <- element$size %||% 11
  if (!is.numeric(size) || length(size) == 0L) size <- 11
  list(
    size       = size,
    colour     = element$colour %||% "black",
    family     = element$family %||% "",
    face       = element$face %||% 1,
    lineheight = element$lineheight %||% 1.2,
    angle      = if (is.numeric(element$angle)) element$angle else 0,
    hjust      = if (is.numeric(element$hjust)) element$hjust else NULL,
    vjust      = if (is.numeric(element$vjust)) element$vjust else NULL
  )
}

## A grob cannot report its size in cm without a device; this is a small
## wrapper around `grobHeight()`/`grobWidth()` that never errors.
.ggse_grob_size_cm <- function(grob, vertical) {
  if (is.null(grob)) return(0)
  out <- tryCatch({
    if (isTRUE(vertical)) {
      grid::convertWidth(grid::grobWidth(grob), "cm", valueOnly = TRUE)
    } else {
      grid::convertHeight(grid::grobHeight(grob), "cm", valueOnly = TRUE)
    }
  }, error = function(e) 0)
  if (!is.finite(out) || out < 0) 0 else out
}

## Rough check whether a vector is a single finite number.
.ggse_is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

## Height of one line of text, in cm.  `stringHeight()` reports the glyph
## extent (roughly 0.7 em), which is too tight to stack several rows of labels,
## so the line box -- font size times the line height -- is used instead.
.ggse_line_height_cm <- function(size, lineheight = 1.2) {
  em <- size * 0.0352778
  em * max(1, if (is.finite(lineheight)) lineheight else 1.2)
}

## Millimetres to centimetres (device independent).
.ggse_mm <- function(x) {
  grid::convertWidth(grid::unit(x, "mm"), "cm", valueOnly = TRUE)
}

## Format a number for messages.
.ggse_fmt <- function(x, digits = 2) {
  formatC(x, digits = digits, format = "f")
}

## cli inform wrapper that can be silenced with options(ggsmartext.quiet = TRUE).
## The glue expressions are evaluated in the caller's environment, so that
## messages can refer to local variables.
.ggse_inform <- function(...) {
  if (isTRUE(getOption("ggsmartext.quiet", FALSE))) {
    return(invisible(NULL))
  }
  cli::cli_inform(..., .envir = parent.frame())
}
