# This file contains functions named in Spanish which are just wrappers to the
# internal functions which are agnostic of language. Here I also include the
# documentation in Spanish. Only external functions that can be used by users
# have their documentation translated.

# ------------------------------------------------------------------------------

#' Generar el mundo de Karel
#'
#' Esta función toma un "mundo" (es decir, una lista con información acerca de
#' su tamaño, paredes, "cosos" presentes y la ubicación y dirección de Karel),
#' lo grafica y prepara todo para que Karel pueda realizar sus acciones. Siempre
#' debe ser evaluada antes de que Karel empiece a cumplir sus objetivos, en
#' especial, si en algún momento hemos cometido un error, debemos comenzar de
#' nuevo corriendo primero esta función.
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
#'     \item \code{nx}: tamaño del mundo de Karel, número de celdas en el eje x
#'     \item \code{ny}: tamaño del mundo de Karel, número de celdas en el eje x
#'     \item \code{hor_walls}: un data.frame con una fila por cada pared
#'     horizontal que hay en el mundo de Karel y 3 columnas: x (coordenada del
#'     inicio de la pared en el eje x), y (coordenada del inicio de la pared en
#'     el eje y), lgth (longitud de la pared, en cantidad de celdas que abarca).
#'     Si toma el valor NULL, no hay paredes horizontales en el mundo.
#'     \item \code{ver_walls}: un data.frame con una fila por cada pared
#'     vertical que hay en el mundo de Karel y 3 columnas: x (coordenada del
#'     inicio de la pared en el eje x), y (coordenada del inicio de la pared en
#'     el eje y), lgth (longitud de la pared, en cantidad de celdas que abarca).
#'     Si toma el valor NULL, no hay paredes verticales en el mundo.
#'     \item \code{karel_x}: coordenada en el eje x para la posición inicial de
#'     Karel.
#'     \item \code{karel_y}: coordenada en el eje y para la posición inicial de
#'     Karel.
#'     \item \code{karel_dir}: dirección inicial de Karel: 1 (mira al oeste), 2
#'     (mira al norte), 3 (mira al oeste) o 4 (mira al sur).
#'     \item \code{beepers_x}: vector numérico con las coordenadas en el eje x
#'     de las celdas donde hay cosos inicialmente. El largo de los vectores
#'     beepers_x, beepers_y y beepers_n debe coincidir. Si no se desea que haya
#'     cosos en el mundo, proveer el valor NULL.
#'     \item \code{beepers_y}: vector numérico con las coordenadas en el eje y
#'     de las celdas donde hay cosos inicialmente. El largo de los vectores
#'     beepers_x, beepers_y y beepers_n debe coincidir. Si no se desea que haya
#'     cosos en el mundo, proveer el valor NULL.
#'     \item \code{beepers_n}: vector numérico con la cantidad de cosos que hay
#'     inicialmente en cada una de las posiciones determinadas por los valores
#'     de beepers_x y beepers_y. El largo de los vectores beepers_x, beepers_y y
#'     beepers_n debe coincidir. Si no se desea que haya cosos en el mundo,
#'     proveer el valor NULL.
#'     \item \code{beepers_bag}: número de cosos que Karel tienen a disposición
#'     en su muchila al inicio. Karel puede poner cosos si es que tiene cosos en
#'     su mochila. Puede tomar el valor Inf.
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
#'   repetirse una y otra vez luego de finalizada (por defecto: FALSE).
#'
#' @return Produce una animación con \code{gganimate}.
#'
#' @examples
#' generar_mundo("mundo001")
#' avanzar()
#' juntar_coso()
#' girar_izquierda()
#' poner_coso()
#' if (FALSE) ejecutar_acciones()
#'
#' @seealso \code{\link{generar_mundo}}
#'
#' @export
#'
ejecutar_acciones <- function(repetir = FALSE) {
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
#' if (FALSE) ejecutar_acciones()
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

#' @rdname acciones
#' @export
poner_coso <- function() .put_beeper(lang = "es")

#' @rdname acciones
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

#' @rdname condiciones
#' @export
frente_abierto <- function() .front_is_clear()

#' @rdname condiciones
#' @export
frente_cerrado <- function() .front_is_blocked()

#' @rdname condiciones
#' @export
izquierda_abierto <- function() .left_is_clear()

#' @rdname condiciones
#' @export
izquierda_cerrado <- function() .left_is_blocked()

#' @rdname condiciones
#' @export
derecha_abierto <- function() .right_is_clear()

#' @rdname condiciones
#' @export
derecha_cerrado <- function() .right_is_blocked()

#' @rdname condiciones
#' @export
hay_cosos <- function() .beepers_present()

#' @rdname condiciones
#' @export
no_hay_cosos <- function() .no_beepers_present()

#' @rdname condiciones
#' @export
karel_tiene_cosos <- function() .karel_has_beepers()

#' @rdname condiciones
#' @export
karel_no_tiene_cosos <- function() .karel_has_no_beepers()

#' @rdname condiciones
#' @export
mira_al_este <- function() .facing_east()

#' @rdname condiciones
#' @export
mira_al_oeste <- function() .facing_west()

#' @rdname condiciones
#' @export
mira_al_norte <- function() .facing_north()

#' @rdname condiciones
#' @export
mira_al_sur <- function() .facing_south()

# ------------------------------------------------------------------------------

#' Habilitar los superpoderes de Karel
#'
#' Luego de correr \code{cargar_super_karel()}, Karel también puede girar a la
#' derecha y darse vuelta, a través de las acciones \code{girar_derecha()} y
#' \code{darse_vuelta()}. Si no se cargan los superpoderes, estas dos funciones
#' no están disponibles.
#'
#' @return No devuelve ningún valor, pero adjuntan al Global Environment las
#'   funciones \code{girar_derecha()} y \code{darse_vuelta()}.
#'
#' @examples
#' generar_mundo("mundo001")
#' cargar_super_karel()
#' darse_vuelta()
#' girar_derecha()
#' if (FALSE) ejecutar_acciones()
#'
#' @seealso \code{\link{acciones}} \code{\link{generar_mundo}}
#'   \code{\link{ejecutar_acciones}}
#' @export
#'
cargar_super_karel <- function() .load_super_karel()

#' @rdname acciones
#' @export
girar_derecha <- function() .turn_right(lang = "es")

#' @rdname acciones
#' @export
darse_vuelta <- function() .turn_around(lang = "es")

# ------------------------------------------------------------------------------

#' Obtener el ambiente de Karel
#'
#' Esta función devuelve un ambiente (R environment) llamado pkg_env, que es
#' creado por el paquete. Se puede usar para probar el funcionamiento del
#' paquete. Es una función interna, no está pensada para ser usada por
#' estudiantes, pero se puede usar con karel:::conseguir_amb().
#'
#' @return Un ambiente de R con objetos que representan al mundo de Karel.
#'
#' @details \code{pkg_env} es un ambiente de R creado dentro del paquete para
#'   guardar y compartir entre las funciones todos los objetos relacionados con
#'   el mundo de Karel y su estado en cada momento. Dado que estas funciones que
#'   usan los estudiantes deben ser simples y no usar argumentos (como, por
#'   ejemplo, \code{avanzar()}) estas funciones modifican internamente a
#'   \code{pkg_env} para implementar cada acción.
#'
#'   Los componentes de este ambiente son:
#'   \enumerate{
#'     \item \code{nx}: tamaño del mundo de Karel, número de celdas en el eje x
#'     \item \code{ny}: tamaño del mundo de Karel, número de celdas en el eje x
#'     \item \code{hor_walls}: un data.frame con una fila por cada pared
#'     horizontal que hay en el mundo de Karel y 3 columnas: x (coordenada del
#'     inicio de la pared en el eje x), y (coordenada del inicio de la pared en
#'     el eje y), lgth (longitud de la pared, en cantidad de celdas que abarca).
#'     Si toma el valor NULL, no hay paredes horizontales en el mundo.
#'     \item \code{ver_walls}: un data.frame con una fila por cada pared
#'     vertical que hay en el mundo de Karel y 3 columnas: x (coordenada del
#'     inicio de la pared en el eje x), y (coordenada del inicio de la pared en
#'     el eje y), lgth (longitud de la pared, en cantidad de celdas que abarca).
#'     Si toma el valor NULL, no hay paredes verticales en el mundo. \item
#'     \code{open_moves}: un arreglo de dimensión nx x ny x 4 de valores
#'     TRUE/FALSE que indica si Karel puede moverse en cada dirección desde una
#'     posición determinada. Para ejemplo, si Karel está en la esquina inferior
#'     izquierda, que es la celda [1, 1], no puede ir al sur ni a la izquierda,
#'     por lo que tenemos open_moves[1, 1, 3] y open_moves[1, 1, 4] establecido
#'     en FALSO. Dependiendo de las paredes existentes podría moverse al sur o
#'     al norte, por lo que open_moves[1, 1, 1] y open_moves[1, 1, 2] puede ser
#'     VERDADERO o FALSO. Teniendo en cuenta el tamaño del mundo y la paredes,
#'     este arreglo es creado por la función interna
#'     \code{\link{generate_open_moves}}.
#'     \item \code{karel}: un data.frame con una fila para cada momento, en el
#'     que se registra cada estado de Karel a lo largo de la ejecución de sus
#'     acciones. Tiene 4 columnas: karel_x (coordenada de Karel en el eje x),
#'     karel_y (coordenada de Karel en el eje y), karel_dir (dirección a la que
#'     mira Karel, 1 este, 2 norte, 3 oeste o 4 sur), y moment (valor entero
#'     indicando cada momento).
#'     \item \code{dir_now}: dirección en la cual Karel está mirando ahora.
#'     \item \code{x_now}: coordenada en el eje x actual de Karel.
#'     \item \code{y_now}: coordenada en el eje y actual de Karel.
#'     \item \code{moment}: momento actual (valor entero).
#'     \item \code{beepers_any}: cantidad total de cosos presentes en el mundo
#'     en este momento.
#'     \item \code{beepers_bag}: número de cosos que Karel tienen a disposición
#'     en su muchila ahora. Karel puede poner cosos si es que tiene cosos en
#'     su mochila. Puede tomar el valor Inf.
#'     \item \code{beepers_now}: un data.frame con tantas filas como celdas con
#'     cosos haya en el mundo y 5 columnas: \code{x} y \code{y} para las
#'     coordenadas de la celda, \code{cell} es el número de la celda contando
#'     como celda número 1 la celda en la esquina inferior izquierda y yendo
#'     hacia arriba por fila (lo que significa que la celda número 2 sería la
#'     celda en las coordenadas x=2 e y=1), \code{n} el número de cosos en esta
#'     celda y \code{moment} el momento al cual corresponde este estado del
#'     mundo. Es creado por la función interna \code{\link{create_beepers}}.
#'     \item \code{beepers_all}: un data.frame con la misma estructura que
#'     \code{beepers_now}. Mientras que \code{beepers_now} solo tiene el estado
#'     actual de cosos, \code{beepers_all} acumula todos los estados para la
#'     animación, uniendo las filas de \code{beepers_now} y \code{beepers_all}
#'     después de cada acción.
#'     \item \code{base_plot}: gráfico inicial del mundo, con su tamaño y todas
#'     las paredes si las hay. No muestra a Karel ni a los cosos, ya que estos
#'     pueden cambiar con el tiempo. Este es el gráfico base que es utilizado
#'     más tarde para producir la animación. Este gráfico es creado por la
#'     función interna \code{\link{plot_base_world}}.
#'   }
#'
#' @examples
#' generar_mundo("mundo001")
#' if (FALSE) karel:::conseguir_amb()
#'
conseguir_amb <- function() .get_pkg_env()

# ------------------------------------------------------------------------------

#' Producir un gráfico del mundo de Karel en un momento dado
#'
#' Esta función grafica el mundo de Karel en el momento pedido. Inicialmente,
#' momento toma el valor 1 y con cada acción que Karel realiza se incrementa en
#' 1. El momento actual está guardado en \code{pkg_env$moment}. Esta función es
#' útil para revisar el código y para obtener imágenes estáticas que pueden
#' usarse al crear ejemplos y ejercicios en los materiales de estudio para los
#' estudiantes. Es una función interna, no está pensada para ser usada por
#' estudiantes, pero se puede usar con karel:::graficar_mundo_estatico().
#'
#' @param momento El momento que se desea graficar.
#'
#' @return Imprime el gráfico.
#'
#' @examples
#' if (FALSE) karel:::graficar_mundo_estatico(1)
#'
graficar_mundo_estatico <- function(momento) {
  .plot_static_world(time = momento, lang = "es")
}
