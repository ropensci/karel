# This file contains functions named in Spanish which are just wrappers to the
# internal functions which are agnostic of languate. Here I also include the
# documentation in Spanish Only external functions that can be used by users
# have their documentation translated.

# ------------------------------------------------------------------------------

#' Generar el mundo de Karel
#'
#' Esta función toma un "mundo" (es decir, una lista con información acerca de
#' su tamaño, paredes, "cosos" presentes y la ubicación y dirección de Karel),
#' lo grafica y prepara todo para que Karel pueda realizar sus acciones. Siempre
#' debe ser evaluada antes de que Karel empiece a cumplir sus
#' objetivos, en especial, si en algún momento hemos cometido un error, debemos
#' comenzar de nuevo corriendo primero esta función.
#'
#' Luego de correr \code{generar_mundo()}, se ejecutan las acciones de Karel y
#' se pueden visualizar con la función \code{ejecutar_acciones()}.
#'
#' @param mundo Un carácter de largo 1 indicando el nombre de uno de los mundos
#'   que ya vienen en el paquete o un objeto de tipo lista con todos los
#'   componentes que debe tener un mundo (ver más abajo en Detalles).
#'
#' @return Dibuja el estado inicial del mundo de Karel y deja todo preparado
#'   para comenzar a registrar sus acciones.
#'
#' @export
#'
#' @examples
#' generar_mundo("mundo001")
#'
#' @seealso \code{\link{acciones}} \code{\link{ejecutar_acciones}}
#'
#' @details El argumento \code{mundo} puede consistir de un mundo creado (es
#'   decir, inventado) por cualquiera. En este caso, \code{mundo} debe ser una
#'   lista con los siguientes componentes:
#'
#'   \enumerate{
#'     \item \code{nx}: TODO
#'     \item \code{ny}:
#'     \item \code{hor_walls}:
#'     \item \code{ver_walls}:
#'     \item \code{karel_x}:
#'     \item \code{karel_y}:
#'     \item \code{karel_dir}:
#'     \item \code{beepers_x}:
#'     \item \code{beepers_y}:
#'     \item \code{beepers_n}:
#'     \item \code{beepers_bag}:
#'   }
#'
generar_mundo <- function(mundo) .generate_world(mundo, lang = "es")

# ------------------------------------------------------------------------------

#' Ejecutar acciones
#'
#' Esta función produce la animación que muestra todas las acciones realizadas
#' por Karel desde que su mundo fue generado con \code{generar_mundo}.
#'
#' @param repetir Valor lógico TRUE o FALSE que indica si la animación debe
#'   repetirse una y otra vez luego de finalizada (por defecto: TRUE).
#'
#' @return Produce una animación con \code{gganimate}.
#'
#' @examples
#' generar_mundo("mundo001")
#' avanzar()
#' juntar_coso()
#' girar_izquierda()
#' poner_coso()
#' ejecutar_acciones()
#'
#' @seealso \code{\link{generar_mundo}}
#'
#' @export
#'
ejecutar_acciones <- function(repetir = TRUE) {
  .run_actions(loop = repetir, lang = "es")
}

# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------

#' Condiciones que Karel puede verificar
#'
#' Este conjunto de funciones devuelven un valor lógico \code{TRUE} o
#' \code{FALSE} según la evaluación que Karel puede hacer de su mundo.
#'
#' @return Valor lógico TRUE o FALSE.
#'
#' @details Las funciones \code{frente_abierto()}, \code{frente_cerrado()},
#'   \code{izquierda_abierto()}, \code{izquierda_cerrado()},
#'   \code{derecha_abierto()} y \code{derecha_cerrado()} analizan si hay paredes
#'   al frente, a la izquierda o a la derecha de Karel. Las funciones
#'   \code{hay_cosos()} y \code{no_hay_cosos()} analizan si hay \code{cosos} en
#'   la posición actual de Karel. Las funciones \code{karel_tiene_cosos()} y
#'   \code{karel_no_tiene_cosos()} analizan si Karel tiene \code{cosos} en su
#'   mochila (no visibles en la representación gráfica). Las funciones
#'   \code{mira_al_este()}, \code{mira_al_oeste()}, \code{mira_al_norte()} y
#'   \code{mira_al_sur()} analizan la dirección hacia la cual Karel está
#'   mirando.
#'
#' @examples
#' generar_mundo("mundo001")
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

#' @rdname conditions
#' @export
frente_abierto <- function() .front_is_clear()

#' @rdname conditions
#' @export
frente_cerrado <- function() .front_is_blocked()

#' @rdname conditions
#' @export
izquierda_abierto <- function() .left_is_clear()

#' @rdname conditions
#' @export
izquierda_cerrado <- function() .left_is_blocked()

#' @rdname conditions
#' @export
derecha_abierto <- function() .right_is_clear()

#' @rdname conditions
#' @export
derecha_cerrado <- function() .right_is_blocked()

#' @rdname conditions
#' @export
hay_cosos <- function() .beepers_present()

#' @rdname conditions
#' @export
no_hay_cosos <- function() .no_beepers_present()

#' @rdname conditions
#' @export
karel_tiene_cosos <- function() .karel_has_beepers()

#' @rdname conditions
#' @export
karel_no_tiene_cosos <- function() .karel_has_no_beepers()

#' @rdname conditions
#' @export
mira_al_este <- function() .facing_east()

#' @rdname conditions
#' @export
mira_al_oeste <- function() .facing_west()

#' @rdname conditions
#' @export
mira_al_norte <- function() .facing_north()

#' @rdname conditions
#' @export
mira_al_sur <- function() .facing_south()

# ------------------------------------------------------------------------------


