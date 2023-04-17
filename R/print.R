#' @title Print
#' @rdname print
#' @export
print.L1 <- function(x, ...) {
  # Fixing the print of L1 class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
# Fixing the print of L2 class data
print.L2 <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.L3 <- function(x, ...) {
  # Fixing the print of L3 class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.L4 <- function(x, ...) {
  # Fixing the print of L4 class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.LSkew <- function(x, ...) {
  # Fixing the print of LSkew class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.LKur <- function(x, ...) {
  # Fixing the print of Lcv class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.Lcv <- function(x, ...) {
  # Fixing the print of Lcv class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.LCA <- function(x, ...) {
  # Fixing the print of LCA class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.LComponents <- function(x, ...) {
  # Fixing the print of LComponents class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.GEVPar <- function(x, ...) {
  # Fixing the print of GEVPar class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.GumbelPar <- function(x, ...) {
  # Fixing the print of GumbelPar class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.GenLogPar <- function(x, ...) {
  # Fixing the print of GenLogPar class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.GenParetoPar <- function(x, ...) {
  # Fixing the print of GenParetoPar class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.LCs <- function(x, ...) {
  # Fixing the print of L4 class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @rdname print
#' @export
print.OptRating <- function(x, ...) {
  # Fixing the print of OptRating class data
  attr(x, "class") <- NULL
  print.default(x, ...)
}

