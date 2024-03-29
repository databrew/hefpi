#' Theme based on goolge docs
#'
#' Theme similar to the default look of charts in Google Docs.
#' @rawNamespace import(ggthemes, except = last_plot)
#' @inheritParams ggplot2::theme_grey
#' @export

theme_gdocs <- function(base_size = 12, base_family="sans") {
  
  ltgray <- "#cccccc"
  dkgray <- "#757575"
  dkgray2 <- "#5A5A5A"
  
  theme_foundation(base_size = base_size,
                   base_family = base_family) +
    theme(rect = element_rect(colour = "black", fill = "white"),
          line = element_line(colour = "black"),
          text = element_text(colour = dkgray),
          # title is aligned left, 20 point Roboto Font, plain
          plot.title = element_text(face = "plain",
                                    size = 15,
                                    hjust = 0, colour = dkgray),
          # No subtitle or captions, so treat like other text
          plot.subtitle = element_text(hjust = 0, size = 9,
                                       face = "plain", colour = dkgray),
          plot.caption = element_text(hjust = 0, size = 5,
                                      face = "plain", colour = dkgray),
          panel.background = element_rect(fill = NA, colour = NA),
          panel.border = element_rect(fill = NA, colour = NA),
          # no strips in gdocs, so make similar to axis titles
          # strip.text = element_text(hjust = 0, size = rel(1), colour = dkgray2,
          #                           face = "plain"),
          # strip.background = element_rect(colour = NA, fill = NA),
          # axis titles: Roboto 12pt, plain.
          axis.title = element_text(face = "plain", colour = dkgray2,
                                    size = rel(1)),
          # axis text: Roboto 12pt, plain
          axis.text = element_text(face = "plain", colour = dkgray2,
                                   size = 12),
          # only axis line on the x-axis. black.
          axis.line.x = element_line(colour = NA),
          axis.line.y = element_line(colour = ltgray),
          axis.ticks = element_line(colour = ltgray),
          # no axis ticks
          # axis.ticks = element_blank(),
          # grid lines on both x and y axes. light gray. no minor gridlines
          panel.grid.major = element_line(colour = ltgray),
          panel.grid.minor = element_blank(),
          # legend has no border
          legend.background = element_rect(colour = NA),
          # legend labels: Roboto 12, dark gray
          legend.text = element_text(size = rel(1),
                                     colour = dkgray),
          # no legend title - use same as legend text
          legend.title = element_text(size = rel(1),
                                      colour = dkgray2, face = "plain"),
          legend.key = element_rect(colour = NA),
          legend.position = "right",
          legend.direction = "vertical"
    )
}

#' Google Docs color palette (discrete)
#'
#' Color palettes from Google Docs.
#' This palette includes 20 colors.
#'
#' @family colour gdocs
#' @import RColorBrewer
#' @export
gdocs_pal <- function() {
  values <- colorRampPalette(brewer.pal(8, "Set2"))(60)
  f <- manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}

#' Google Docs color scales
#'
#' Color scales from Google Docs.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour gdocs
#' @rdname scale_gdocs
#' @export
scale_fill_gdocs <- function(...) {
  discrete_scale("fill", "gdocs", gdocs_pal(), ...)
}

#' @export
#' @rdname scale_gdocs
scale_colour_gdocs <- function(...) {
  discrete_scale("colour", "gdocs", gdocs_pal(), ...)
}

#' @export
#' @rdname scale_gdocs
scale_color_gdocs <- scale_colour_gdocs