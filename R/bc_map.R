#' @title Custom map of BC and surrounding area
#'
#' @import sf ggplot2 cowplot
#' @param xmin Minimum longitude.
#' @param xmax Maximum longitude.
#' @param ymin Minimum latitude.
#' @param ymax Maximum latitude.
#' @param na_inset Include an inset figure of the map position?
#'
#' @export
#'

map <- \(xmin, xmax, ymin, ymax, water_colour, na_inset) {
  data(base_map, envir=environment())
  map <- base_map +
    coord_sf(xlim = c(xmin, xmax),
             ylim = c(ymin, ymax)) +
    theme(axis.text = element_text(size = 10),
          panel.background = element_rect(fill = water_colour))

  if(na_inset == TRUE) {
    ca <- map_data("world", "Canada")
    us <- map_data("world", "USA")
    me <- map_data("world", "Mexico")

    (ins <- ggplot() +
        geom_polygon(data = us, aes(x = long, y = lat, group = group),
                     fill = "grey80", colour = "black", linewidth = 1/8) +
        geom_polygon(data = ca, aes(x = long, y = lat, group = group),
                     fill = "grey90", colour = "black", linewidth = 1/8) +
        geom_polygon(data = me, aes(x = long, y = lat, group = group),
                     fill = "grey70", colour = "black", linewidth = 1/8) +
        theme_void() +
        theme(panel.border = element_rect(colour = "black",
                                          fill = NA, linewidth = 1/4),
              panel.background = element_rect(fill = "white"))  +
        annotate("rect", fill = NA, colour = "black",
                 linewidth = 1/2,
                 xmin = xmin, xmax = xmax,
                 ymin = ymin, ymax = ymax) +
        coord_map(ylim = c(72, 20),
                  xlim = c(-57, -165)))

    ggdraw(plot = map) +
      draw_plot({
        ins
      },
      x = 0.790,
      y = 0.575,
      width = 0.2,
      height = 0.5) -> map

  }

  return(map)

}
