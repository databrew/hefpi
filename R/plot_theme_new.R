#' Theme based on goolge docs
#'
#' Theme similar to the default look of charts in Google Docs.
#' @import ggthemes
#' @inheritParams ggplot2::theme_grey
#' @export



theme_hefpi <- function(base_size = 12, 
                        base_family="Helveticaserif",
                        plot_title_size = 15,
                        plot_title_color = "#757575",
                        sub_title_size = 10,
                        sub_title_color ="#757575" ,
                        axis_title_size = 1,
                        axis_title_color = "#757575",
                        x_axis_color ="#5A5A5A",
                        x_axis_size = 12,
                        x_axis_angle = 0,
                        x_axis_hjust = 0,
                        x_axis_vjust = 0,
                        y_axis_color ="#5A5A5A",
                        y_axis_size =12,
                        y_axis_angle=0,
                        y_axis_hjust=0,
                        y_axis_vjust=0,
                        grid_major_x = "#cccccc",
                        grid_minor_x = NA,
                        grid_major_y = "#cccccc",
                        grid_minor_y = NA,
                        x_axis_line = "#cccccc",
                        y_axis_line = "#cccccc",
                        legend_position= 'right',
                        legend_direction = 'vertical',
                        legend_text_size = 1,
                        legend_text_color = "#757575"
) {
  
  theme_foundation(base_size = base_size,
                   base_family = base_family) +
    theme(rect = element_rect(colour = "black", 
                              fill = "white"),
          line = element_line(colour = "black"),
          text = element_text(colour = "#757575"),
          # title is aligned left, 20 point Roboto Font, plain
          plot.title = element_text(face = "plain",
                                    size = plot_title_size,
                                    hjust = 0, 
                                    colour = plot_title_color),
          # No subtitle or captions, so treat like other text
          plot.subtitle = element_text(face = "plain",
                                       size = sub_title_size,
                                       hjust = 0, 
                                       colour = sub_title_color),
          plot.caption = element_text(hjust = 0, 
                                      size = 5,
                                      face = "plain", 
                                      colour = "#757575"),
          panel.background = element_rect(fill = NA, colour = NA),
          panel.border = element_rect(fill = NA, colour = NA),
          
          # axis titles
          axis.title = element_text(face = "plain", 
                                    colour = "#5A5A5A",
                                    size = rel(1)),
          # axis text: Roboto 12pt, plain
          axis.text.x = element_text(colour = x_axis_color,
                                     size =  x_axis_size,
                                     angle = x_axis_angle,
                                     hjust = x_axis_hjust,
                                     vjust = x_axis_vjust),
          axis.text.y = element_text(colour = y_axis_color,
                                     size =  y_axis_size,
                                     angle = y_axis_angle,
                                     hjust = y_axis_hjust,
                                     vjust = y_axis_vjust),
          # only axis line on the x-axis. black.
          axis.line.x = element_line(colour = x_axis_line),
          axis.line.y = element_line(colour = y_axis_line),
          # no axis ticks
          # axis.ticks = element_blank(),
          # grid lines on both x and y axes. light gray. no minor gridlines
          panel.grid.major.x = element_line(colour = grid_major_x),
          panel.grid.minor.x = element_line(colour = grid_minor_x),
          panel.grid.major.y = element_line(colour = grid_major_y),
          panel.grid.minor.y = element_line(colour = grid_minor_y),
          # legend has no border
          legend.background = element_rect(colour = NA),
          # legend labels: Roboto 12, dark gray
          legend.text = element_text(size = rel(legend_text_size),
                                     colour = legend_text_color),
          # no legend title - use same as legend text
          legend.title = element_text(size = rel(1),
                                      colour = "#5A5A5A", face = "plain"),
          legend.key = element_rect(colour = NA),
          legend.position = legend_position,
          legend.direction = legend_direction
    )
}
