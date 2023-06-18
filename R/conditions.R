#' @rdname condiciones
#' @export
frente_abierto <- function() {
  return(pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, pkg_env$dir_now])
}

#' @rdname condiciones
#' @export
frente_cerrado <- function() {
  return(!pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, pkg_env$dir_now])
}

#' @rdname condiciones
#' @export
izquierda_abierto <- function() {
  pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                     switch(pkg_env$dir_now, 2, 3, 4, 1)]
}

#' @rdname condiciones
#' @export
izquierda_cerrado <- function() {
  !pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                      switch(pkg_env$dir_now, 2, 3, 4, 1)]
}

#' @rdname condiciones
#' @export
derecha_abierto <- function() {
  pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                     switch(pkg_env$dir_now, 4, 1, 2, 3)]
}

#' @rdname condiciones
#' @export
derecha_cerrado <- function() {
  !pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                      switch(pkg_env$dir_now, 4, 1, 2, 3)]
}

#' @rdname condiciones
#' @export
hay_cosos <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  return(cell %in% pkg_env$beepers_now$cell)
}

#' @rdname condiciones
#' @export
no_hay_cosos <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  return(!cell %in% pkg_env$beepers_now$cell)
}

#' @rdname condiciones
#' @export
karel_tiene_cosos <- function() {
  return(pkg_env$beepers_bag > 0)
}

#' @rdname condiciones
#' @export
karel_no_tiene_cosos <- function() {
  return(!pkg_env$beepers_bag > 0)
}

#' @rdname condiciones
#' @export
mira_al_este <- function() {
  return(pkg_env$dir_now == 1)
}

#' @rdname condiciones
#' @export
mira_al_norte <- function() {
  return(pkg_env$dir_now == 2)
}

#' @rdname condiciones
#' @export
mira_al_oeste <- function() {
  return(pkg_env$dir_now == 3)
}

#' @rdname condiciones
#' @export
mira_al_sur <- function() {
  return(pkg_env$dir_now == 4)
}
