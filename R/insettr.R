#' @title Inset plots with ggplot2
#' @description
#' This function allows you to specify the corner the inset plot should be
#' positioned in, rather than requiring the user supply specific x and y coordinates.
#'
#'
#' @param plot1 The main ggplot object.
#' @param plot2 A ggplot object to be inset.
#' @param location The corner the inset plot is assigned to (must be one of "bl", "br", "tl", "tr").
#' @param h Height of the inset plot (as a % of the main plot size).
#' @param w Width of the inset plot (as a % of the main plot size).
#'
#' @return A ggplot object.
#' @import cowplot
#' @import ggplot2
#' @export

insettr <- \(plot1, plot2, location, h, w) {

  y <- substr(location, 1, 1)
  x <- substr(location, 2, 2)

  ggdraw(plot = plot1) +
    draw_plot(
      plot = plot2,
      x = if(x == "l") 0.06 else 0.98 ,
      y = if(y == "t") 0.98 else 0.06,
      width = w, height = h,
      vjust = if(y == "t") 1 else 0,
      hjust = if(x == "l") 0 else 1
    )

}
