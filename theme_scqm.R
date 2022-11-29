#' The SCQM theme
#'
#' This function allows you to add the scqm theme to your ggplot graphics.
#' @import ggplot2
#' @param alignment alignment of text (in [0, 1]). see [ggplot2::element_text()]
#' @export
#' @examples

theme_scqm <- function(font = "Helvetica",     # as defined by SCQM CICD
                       text_color = "#000000", # black as defined by SCQM CICD
                       grid_color = "#c9c9c9", # light grey as defined by SCQM CICD
                       alignment = 0.5) {
  
  # ============================================================================
  # the theme is based on the ggplot theme 'black white'
  ggplot2::theme_bw() +
    
  theme(
    # text style ------
    text = element_text(family = font, colour = text_color, hjust = alignment),
  
    # grid style ------
    # remove x grid
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    
    # keep y grid but change color
    panel.grid.major.y = ggplot2::element_line(color=grid_color, linetype = "solid", size = 0.2),
    panel.grid.minor.y = ggplot2::element_line(color=grid_color, linetype = "solid", size = 0.1),

    # legend style ------
    # legend.text = element_text(family = font, colour = text_color, hjust = alignment, size  = 22),
    legend.title = element_text(family = font, colour = text_color, hjust = alignment, size  = 22),
    
    # axis style ------
    axis.text =  element_text(family = font, colour = text_color, hjust = 0.5),  # I always want the axis text to be centered
    # remove ticks because with major grid, this is superfluous
    axis.ticks = ggplot2::element_blank(),
    
    # facet style ------
    # remove grey color for fact so it is see-through for pngs. Make text for strips larger
    strip.background = ggplot2::element_blank(),
    strip.text = element_text(family = font, colour = text_color, hjust = alignment, size  = 22),
    
    # plot backgrouns ------
    # remove background so it is see-through for pngs
    panel.background = ggplot2::element_blank()
    
    )
}
