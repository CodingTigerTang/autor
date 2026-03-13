#' Add a logo to a ggplot outside the panel area
#'
#' This function adds a PNG logo to a ggplot. It uses panel ranges to calculate
#' coordinates, allowing the logo to be placed in the margins. It automatically
#' maintains the logo's aspect ratio.
#'
#' @param p A ggplot object.
#' @param logo_path Character. The file path to a PNG image.
#' @param position Character. One of "bottom-right", "bottom-left", "top-right", or "top-left".
#' @param width_frac Numeric. The width of the logo as a fraction of the plot width (0 to 1).
#' @param offset_x Numeric. Horizontal distance from the panel edge as a fraction of plot width.
#' @param offset_y Numeric. Vertical distance from the panel edge as a fraction of plot height.
#' @param margin A \code{ggplot2::margin()} object to ensure the logo isn't cut off by the device edge.
#'
#' @return A ggplot2 object with the logo added as an annotation.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' add_logo(p, "logo.png", position = "bottom-right")
#' }
add_logo <- function(p,
                     logo_path,
                     position = c("bottom-right", "bottom-left", "top-right", "top-left"),
                     width_frac = 0.2,
                     offset_x = 0.02,
                     offset_y = 0.12,
                     margin = ggplot2::margin(1, 1, 3, 1, "lines")) {

  # 1. Setup and Validation
  position <- match.arg(position)
  if (!file.exists(logo_path)) stop("Logo file not found at: ", logo_path)
  if (!inherits(p, "ggplot")) stop("Object 'p' must be a ggplot.")

  # 2. Extract Panel Dimensions
  # We build the plot to find the internal 'data units' of the axes
  built   <- ggplot2::ggplot_build(p)
  panel   <- built$layout$panel_params[[1]]
  x_range <- panel$x.range
  y_range <- panel$y.range
  x_span  <- diff(x_range)
  y_span  <- diff(y_range)

  # 3. Handle Image Aspect Ratio
  img <- png::readPNG(logo_path)
  # height / width of the actual pixels
  img_ratio <- dim(img)[1] / dim(img)[2]

  # Calculate dimensions in data units
  logo_w <- x_span * width_frac
  # We adjust height by the ratio of the axis spans to keep it square on screen
  logo_h <- logo_w * img_ratio * (y_span / x_span)

  # 4. Placement Logic (Relative to panel edges)
  is_top   <- grepl("top", position)
  is_right <- grepl("right", position)

  # Calculate X bounds
  if (is_right) {
    xmax <- x_range[2] + (x_span * offset_x)
    xmin <- xmax - logo_w
  } else {
    xmin <- x_range[1] - (x_span * offset_x)
    xmax <- xmin + logo_w
  }

  # Calculate Y bounds
  if (is_top) {
    ymin <- y_range[2] + (y_span * offset_y)
    ymax <- ymin + logo_h
  } else {
    ymax <- y_range[1] - (y_span * offset_y)
    ymin <- ymax - logo_h
  }

  # 5. Build Grob and Combine
  logo_grob <- grid::rasterGrob(img, interpolate = TRUE)

  p +
    ggplot2::annotation_custom(
      grob = logo_grob,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
    ) +
    # 'clip = off' allows the logo to be drawn in the margin area
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = margin)
}
