# This file contains functions named in Spanish which are just wrappers to the
# internal functions which are agnostic of languate. Here I also include the
# documentation in Spanish Only external functions that can be used by users
# have their documentation translated.

#' Acciones que Karel puede realizar
#'
#' \code{avanzar()}, \code{girar_izquierda()}, \code{juntar_coso()} y
#' \code{poner_coso()} son las cuatro actividades básicas que Karel sabe
#' realizar. Si se habilitan los superpoderes de Karel con
#' \code{cargar_super_karel()}, entonces también puede \code{girar_derecha()} y
#' \code{darse_vuelta()}.
#'
#' @return Estas funciones no devuelven nada, pero realizan cambios en el mundo
#'   de Karel que se ven cuando se ejecutan todas las acciones con
#'   \code{ejecutar_acciones()}.
#'
#' @examples
#' generar_mundo("mundo001")
#' avanzar()
#' juntar_coso()
#' girar_izquierda()
#' poner_coso()
#' ejecutar_acciones()
#'
#' @seealso \code{\link{cargar_super_karel}} \code{\link{generar_mundo}}
#'   \code{\link{ejecutar_acciones}}
#'
#' @name acciones
NULL
#> NULL

#' @rdname acciones
#' @export
avanzar <- function() .move(lang = "es")

#' @rdname acciones
#' @export
girar_izquierda <- function() .turn_left(lang = "es")

#' @rdname actions
#' @export
poner_coso <- function() .put_beeper(lang = "es")


#' @rdname actions
#' @export
juntar_coso <- function() .pick_beeper(lang = "es")
