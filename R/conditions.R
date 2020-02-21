#' Condiciones que Karel puede verificar
#'
#' Este conjunto de funciones devuelven un valor lógico \code{TRUE} o \code{FALSE} según la evaluación que Karel puede hacer de su mundo.
#'
#' @return Valor lógico TRUE o FALSE
#'
#' @details Las funciones \code{frente_abierto()}, \code{frente_cerrado()}, \code{izquierda_abierto()}, \code{izquierda_cerrado()}, \code{derecha_abierto()} y \code{derecha_cerrado()} analizan si hay paredes al frente, a la izquierda o a la derecha de Karel. Las funciones \code{hay_cosos()} y \code{no_hay_cosos()} analizan si hay \code{cosos} en la posición actual de Karel. Las funciones \code{karel_tiene_cosos()} y \code{karel_no_tiene_cosos()} analizan si Karel tiene \code{cosos} en su muchila (no visibles en la representación gráfica). Las funciones \code{mira_al_este()}, \code{mira_al_oeste()}, \code{mira_al_norte()} y \code{mira_al_sur()} analizan la dirección hacia la cual Karel está mirando.
#'
#' @examples
#' generar_mundo("world_101")
#' frente_abierto()
#' frente_cerrado()
#' izquierda_abierto()
#' izquierda_cerrado()
#' derecha_abierto()
#' derecha_cerrado()
#' hay_cosos()
#' no_hay_cosos()
#' karel_tiene_cosos()
#' karel_no_tiene_cosos()
#' mira_al_este()
#' mira_al_oeste()
#' mira_al_norte()
#' mira_al_sur()
#'
#' @seealso \code{\link{generar_mundo}}
#'
#' @name condiciones
NULL
#> NULL


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
