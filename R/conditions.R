# Conditions that Karel can test
# These group of functions return a logical value TRUE o FALSE according to
# Karel's evaluation of her world.

#' @keywords internal
.front_is_clear <- function() {
  return(pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, pkg_env$dir_now])
}

#' @keywords internal
.front_is_blocked <- function() {
  return(!pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, pkg_env$dir_now])
}

#' @keywords internal
.left_is_clear <- function() {
  pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                     switch(pkg_env$dir_now, 2, 3, 4, 1)]
}

#' @keywords internal
.left_is_blocked <- function() {
  !pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                      switch(pkg_env$dir_now, 2, 3, 4, 1)]
}

#' @keywords internal
.right_is_clear <- function() {
  pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                     switch(pkg_env$dir_now, 4, 1, 2, 3)]
}

#' @keywords internal
.right_is_blocked <- function() {
  !pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                      switch(pkg_env$dir_now, 4, 1, 2, 3)]
}

#' @keywords internal
.beepers_present <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  return(cell %in% pkg_env$beepers_now$cell)
}

#' @keywords internal
.no_beepers_present <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  return(!cell %in% pkg_env$beepers_now$cell)
}

#' @keywords internal
.karel_has_beepers <- function() {
  return(pkg_env$beepers_bag > 0)
}

#' @keywords internal
.karel_has_no_beepers <- function() {
  return(!pkg_env$beepers_bag > 0)
}

#' @keywords internal
.facing_east <- function() {
  return(pkg_env$dir_now == 1)
}

#' @keywords internal
.facing_north <- function() {
  return(pkg_env$dir_now == 2)
}

#' @keywords internal
.facing_west <- function() {
  return(pkg_env$dir_now == 3)
}

#' @keywords internal
.facing_south <- function() {
  return(pkg_env$dir_now == 4)
}
