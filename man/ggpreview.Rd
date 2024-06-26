% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpreview.R
\name{ggpreview}
\alias{ggpreview}
\title{Preview ggplot in Specified Dimensions}
\usage{
ggpreview(
  plot = ggplot2::last_plot(),
  width = NA,
  height = NA,
  asp = NULL,
  dpi = 300,
  device = "png",
  units = c("in", "cm", "mm", "px"),
  scale = 1,
  limitsize = TRUE,
  bg = NULL,
  ...
)
}
\arguments{
\item{plot}{Plot to save, defaults to last plot displayed.}

\item{width, height}{Plot size in units expressed by the \code{units} argument.
If not supplied, uses the size of the current graphics device.}

\item{asp}{The aspect ratio of the plot calculated as \code{width / height}. If
this is a numeric value (and not \code{NULL}) the \code{height} of the plot will be
recalculated to \code{height = width / asp}.}

\item{dpi}{Plot resolution. Also accepts a string input: "retina" (320),
"print" (300), or "screen" (72). Applies only to raster output types.}

\item{device}{Device to use. Can either be a device function
(e.g. \link{png}), or one of "eps", "ps", "tex" (pictex),
"pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only). If
\code{NULL} (default), the device is guessed based on the \code{filename} extension.}

\item{units}{One of the following units in which the \code{width} and \code{height}
arguments are expressed: \code{"in"}, \code{"cm"}, \code{"mm"} or \code{"px"}.}

\item{scale}{Multiplicative scaling factor.}

\item{limitsize}{When \code{TRUE} (the default), \code{ggsave()} will not
save images larger than 50x50 inches, to prevent the common error of
specifying dimensions in pixels.}

\item{bg}{Background colour. If \code{NULL}, uses the \code{plot.background} fill value
from the plot theme.}

\item{...}{Other arguments passed on to the graphics device function,
as specified by \code{device}.}
}
\value{
No return value, called for side effects.
}
\description{
This function previews a ggplot in its actual dimensions in order to see how
it will look when saved. It is also significantly faster than the default
preview in RStudio for ggplots created using cbbplotR. This is copied
directly from \code{nflplotR}.
}
