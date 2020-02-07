#' Title
#'
#' @return
#' @export
#'
#' @examples
# frontIsClear()
frente_abierto <- function() {
  return(pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, pkg_env$dir_now])
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# leftIsClear()
izquierda_abierto <- function() {
  pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                     switch(pkg_env$dir_now, 2, 3, 4, 1)]
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# rightIsClear()
derecha_abierto <- function() {
  pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now,
                     switch(pkg_env$dir_now, 4, 1, 2, 3)]
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# beepersPresent()
hay_beepers <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  return(cell %in% pkg_env$beepers_now$cell)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# beepersInBag()
karel_tiene_beepers <- function() {
  return(pkg_env$beepers_bag > 0)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# facingEast()
mira_al_este <- function() {
  return(pkg_env$dir_now == 1)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# facingNorth()
mira_al_norte <- function() {
  return(pkg_env$dir_now == 2)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# facingWest()
mira_al_oeste <- function() {
  return(pkg_env$dir_now == 3)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# facingSouth()
mira_al_sur <- function() {
  return(pkg_env$dir_now == 4)
}
