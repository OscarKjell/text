
# From cowplot: https://github.com/wilkelab/cowplot/tree/master/R

#' Draw a (sub)plot.
#'
#' Places a plot somewhere onto the drawing canvas. By default, coordinates run from
#' 0 to 1, and the point (0, 0) is in the lower left corner of the canvas.
#' @param plot The plot to place. Can be a ggplot2 plot, an arbitrary grob or gtable,
#'   or a recorded base-R plot, as in [as_grob()].
#' @param x The x location of the plot. (Left side if `hjust = 0`.)
#' @param y The y location of the plot. (Bottom side if `vjust = 0`.)
#' @param hjust,vjust Horizontal and vertical justification relative to x.
#' @param halign,valign Horizontal and vertical justification of the plot inside
#'   the box.
#' @param width Width of the plot.
#' @param height Height of the plot.
#' @param scale Scales the grob relative to the rectangle defined by `x`, `y`, `width`, `height`. A setting
#'   of `scale = 1` indicates no scaling.
#' @examples
#' library(ggplot2)
#'
#' # make a plot
#' p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
#'     geom_point()
#' # draw into the top-right corner of a larger plot area
#' ggdraw() + draw_plot(p, .6, .6, .4, .4)
#' @export
draw_plot <- function(plot, x = 0, y = 0, width = 1, height = 1, scale = 1,
                      hjust = 0, vjust = 0, halign = 0.5, valign = 0.5) {
  plot <- as_grob(plot) # convert to grob if necessary
  draw_grob(
    plot, x = x, y = y, width = width, height = height,
    scale = scale, hjust = hjust, vjust = vjust,
    halign = halign, valign = valign
  )
}

#' Set up a drawing layer on top of a ggplot
#'
#' Set up a drawing layer on top of a ggplot.
#' @param plot The plot to use as a starting point. Can be a ggplot2 plot, an arbitrary
#'   grob or gtable, or a recorded base-R plot, as in [as_grob()].
#' @param xlim The x-axis limits for the drawing layer.
#' @param ylim The y-axis limits for the drawing layer.
#' @param clip Should drawing be clipped to the set limits? The default is no ("off").
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(displ, cty)) +
#'   geom_point() +
#'   theme_minimal_grid()
#' ggdraw(p) + draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
#' @export
ggdraw <- function(plot = NULL, xlim = c(0, 1), ylim = c(0, 1), clip = "off") {
  p <- ggplot() + # empty plot
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = clip) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = NULL) +
    theme_nothing() # with empty theme

  if (!is.null(plot)){
    p <- p + draw_plot(plot)
  }
  p # return ggplot drawing layer
}

draw_grob <- function(grob, x = 0, y = 0, width = 1, height = 1, scale = 1, clip = "inherit",
                      hjust = 0, vjust = 0, halign = 0.5, valign = 0.5) {
  ggplot2::layer(
    data = data.frame(x = NA),
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = GeomDrawGrob,
    inherit.aes = FALSE,
    params = list(
      grob = grob,
      xmin = x - hjust*width,
      xmax = x + (1-hjust)*width,
      ymin = y - vjust*height,
      ymax = y + (1-vjust)*height,
      scale = scale,
      clip = clip,
      halign = halign,
      valign = valign
    )
  )
}

annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})

#' @rdname draw_grob
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomCustomAnn
#' @export
GeomDrawGrob <- ggplot2::ggproto("GeomDrawGrob", ggplot2::GeomCustomAnn,
                        draw_panel = function(self, data, panel_params, coord, grob, xmin, xmax, ymin, ymax, scale = 1, clip = "inherit",
                                              halign = 0.5, valign = 0.5) {
                          if (!inherits(coord, "CoordCartesian")) {
                            stop("draw_grob only works with Cartesian coordinates",
                                 call. = FALSE)
                          }
                          corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
                          data <- coord$transform(corners, panel_params)

                          x_rng <- range(data$x, na.rm = TRUE)
                          y_rng <- range(data$y, na.rm = TRUE)

                          # set up inner and outer viewport for clipping. Unfortunately,
                          # clipping doesn't work properly most of the time, due to
                          # grid limitations
                          vp_outer <- grid::viewport(x = min(x_rng) + halign*diff(x_rng),
                                                     y = min(y_rng) + valign*diff(y_rng),
                                                     width = diff(x_rng), height = diff(y_rng),
                                                     just = c(halign, valign),
                                                     clip = clip)

                          vp_inner <- grid::viewport(x = halign, y = valign,
                                                     width = scale, height = scale,
                                                     just = c(halign, valign))

                          id <- annotation_id()
                          inner_grob <- grid::grobTree(grob, vp = vp_inner, name = paste(grob$name, id))
                          grid::grobTree(inner_grob, vp = vp_outer, name = paste("GeomDrawGrob", id))
                        }
)

#' Convert a base plot or a ggplot2 plot into a grob
#'
#' This function does its best attempt to take whatever you provide it and turn it into a grob.
#' It is primarily meant to convert ggplot plots into grobs, but it will also take any grid
#' object (grob), a recorded base R plot, a formula specifying a base R plot, a function that
#' generates a base R plot, or a trellis object.
#'
#' @param plot The plot to convert
#' @param device A function that creates an appropriate null device. See [`set_null_device()`]
#'   for details. If set to `NULL`, will use the cowplot-wide default.
#'
#' @examples
#' library(grid)
#' x <- 1:10
#' y <- (1:10)^2
#'
#' p <- ~plot(x, y)
#' grid.newpage()
#' grid.draw(as_grob(p))
#' @export
as_grob <- function(plot, device = NULL) {
  UseMethod("as_grob")
}

#' @rdname png_null_device
#' @export
pdf_null_device <- function(width, height) {
  grDevices::pdf(NULL, width = width, height = height)
  grDevices::dev.control("enable")
}

# the null device is stored in an environment
# default upon start up is pdf null device
null_dev_env <- new.env(parent = emptyenv())
null_dev_env$current <- pdf_null_device

#' @export
as_grob.ggplot <- function(plot, device = NULL) {
  # Convert ggplot plot to grob
  #
  # To be safe this works as expected, we have to do some graphics-device gymnastics.
  # We need to save and restore the current graphics device, and we also need to open
  # a null device. If we don't do this, things may go wrong, in particular in R Studio
  # or shiny, such as plots popping up in the wrong location or spurious empty plots
  # appearing in knitr. Also, depending on which null device we choose, non-standard
  # fonts may or may not work. Different null devices work best in different environments,
  # that's why the null device is configurable. (`pdf(NULL)` is the most robust but
  # can't handle all fonts, `png()` works well on OS X but creates spurious output files,
  # `Cairo(type = "raster")` works well on Windows but font-handling is broken on OS X.)

  if (is.null(device)) {
    device <- null_dev_env$current
  }

  cur_dev <- grDevices::dev.cur()   # store current device
  device(width = 6, height = 6)     # open null device
  null_dev <- grDevices::dev.cur()  # store null device

  # make sure we always clean up properly, even if something causes an error
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev) # only set cur device if not null device
  })

  ggplot2::ggplotGrob(plot)  # convert plot to grob
}
