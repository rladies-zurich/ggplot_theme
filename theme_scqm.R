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


#' Defining the SCQM colors
#' 
#' These colors are pre-defined in SCQM's CICD manual
#' 
#' @return a named list of HEX colors with the length of your input
#' @examples
#' scqm_color("mint", "lightmint")
scqm_color <- function(...) {
  
  # by SCQM CICD, each color is defined as dark and as light (60% hue of dark)
  scqm_colors <- c(
    
    # main palette
    `blue` = '#416896',
    `lightblue` = '#8da5c1',
    
    `grey` = '#a5a5a5',
    `lightgrey` = '#c9c9c9',
    
    `plum` = '#7b4264',
    `lightplum` = '#b08ea3',
    
    # repreg palette
    `sand` = '#c0a576',
    `lightsand` = '#d9c9ac',
    
    `mint` = '#98b8b1',
    `lightmint` = '#c2d4d0',
    
    `rose` = '#ba9a99',
    `lightrose` = '#d5c2c2'
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (scqm_colors)
  
  scqm_colors[cols]
}



#' Grouping SCQM colors into palettes
#' 
#' These palettes are pre-defined in SCQM's CICD manual
#'
#' @param palette which SCQM palette do you need
#' @return a named list of HEX colors
#' @examples
#' scqm_palette("repreg")
scqm_palette <- function(palette = "main", ...) {
  
  scqm_palettes <- list(
    `main` = scqm_color(c("blue", "grey", "plum")),
    `mainlight` = scqm_color(c("lightblue", "lightgrey", "lightplum")),
    
    `repreg` = scqm_color(c("sand", "mint", "rose")),
    `repreglight` = scqm_color(c("lightsand", "lightmint", "lightrose")),
    
    # diagnose palette
    `diagnose` = scqm_color(c("blue", "sand", "plum", "mint", "rose"))
  )
  
  # change the names for the diagnose vector
  names(scqm_palettes$diagnose) <- c("RA", "axSpA", "PsA", "PMR", "GCA")
  
  scqm_palettes[[palette]]
  
}



#' A helper function to use the SCQM palettes for custom ggplot scales
#' @inheritParams scqm_palette
#' @param direction default 1, reverse -1
#' @return 
palette_gen <- function(palette = "main", direction = 1) {
  
  function(n) {
    
    if (n > length(scqm_palette(palette)))
      warning("Not enough colors in this palette!")
    
    else {
      
      all_colors <- scqm_palette(palette)
      
      all_colors <- unname(unlist(all_colors))
      
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      
      color_list <- all_colors[1:n]
      
    }
  }
}


#' The SCQM version of [ggplot2::scale_fill_manual()]
#' @inheritParams palette_gen
scale_fill_scqm <- function(palette = "main", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "fill", "scqm",
    palette_gen(palette, direction),
    ...
  )
}

#' The SCQM version of [ggplot2::scale_colour_manual()]
#' @inheritParams palette_gen
scale_colour_scqm <- function(palette = "main", direction = 1, ...) {
  
  ggplot2::discrete_scale(
    "colour", "scqm",
    palette_gen(palette, direction),
    ...
  )
}
# alias
scale_color_scqm <- scale_colour_scqm

