# Find rows of data that are selected by a brush
#
# Adapted from image-interact.R to work with xts objects.

brushedPoints <- function(df, brush,
  xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL,
  allRows = FALSE) {
  if (is.null(brush)) {
    if (allRows)
      df$selected_ <- FALSE
    else
      df <- df[0, , drop = FALSE]

    return(df)
  }

  if (is.null(brush$xmin)) {
    stop("brushedPoints requires a brush object with xmin, xmax, ymin, and ymax.")
  }

  # Which direction(s) the brush is selecting over. Direction can be 'x', 'y',
  # or 'xy'.
  use_x <- grepl("x", brush$direction)
  use_y <- grepl("y", brush$direction)

  # Try to extract vars from brush object
  xvar      <- xvar      %OR% brush$mapping$x
  yvar      <- yvar      %OR% brush$mapping$y
  panelvar1 <- panelvar1 %OR% brush$mapping$panelvar1
  panelvar2 <- panelvar2 %OR% brush$mapping$panelvar2

  # Filter out x and y values
  keep_rows <- rep(TRUE, nrow(df))
  if (use_x) {
    if (is.null(xvar))
      stop("brushedPoints: not able to automatically infer `xvar` from brush")
    # Extract data values from the data frame
    if (xts::is.xts(df)) {
      x <- asNumber(zoo::index(df))
    } else {
      x <- asNumber(x[,yvar])
    }
    keep_rows <- keep_rows & (x >= brush$xmin & x <= brush$xmax)
  }
  if (use_y) {
    if (is.null(yvar))
      stop("brushedPoints: not able to automatically infer `yvar` from brush")
    y <- asNumber(df[,yvar])
    keep_rows <- keep_rows & (y >= brush$ymin & y <= brush$ymax)
  }

  if (allRows) {
    df$selected_ <- keep_rows
    df
  } else {
    df[keep_rows, , drop = FALSE]
  }
}

# Coerce various types of variables to numbers. This works for Date, POSIXt,
# characters, and factors. Used because the mouse coords are numeric. Taken
# from image-interact.R (Shiny)
asNumber <- function(x) {
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) x <- as.integer(x)
  as.numeric(x)
}

# Taken from utils.R (Shiny)

`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}

is.POSIXct <- function(x) inherits(x, "POSIXct")
