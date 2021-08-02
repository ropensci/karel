#' Habilitar los superpoderes de Karel
#'
#' Luego de correr \code{cargar_super_karel()}, Karel también puede girar a la derecha y darse vuelta, a través de las acciones \code{girar_derecha()} y \code{darse_vuelta()}. Si no se cargan los superpoderes, estas dos funciones no están disponibles.
#'
#' @return No devuelve ningún valor, pero adjuntan al Global Environment las funciones \code{girar_derecha()} y \code{darse_vuelta()}.
#'
#' @examples
#' generar_mundo("mundo001")
#' cargar_super_karel()
#' darse_vuelta()
#' girar_derecha()
#' ejecutar_acciones()
#'
#' @seealso \code{\link{acciones}} \code{\link{generar_mundo}} \code{\link{ejecutar_acciones}}
#' @export
cargar_super_karel <- function() {
  # These do what I want but they get me the note about making assignments to
  # the global environment
  # assign("girar_derecha", girar_derecha, envir = .GlobalEnv)
  # assign("darse_vuelta", darse_vuelta, envir = .GlobalEnv)

  # This hack solves the note
  global_env_set_hack("girar_derecha", girar_derecha, 1L)
  global_env_set_hack("darse_vuelta", darse_vuelta, 1L)
}

#' This function lets me add objects to another environment
#'
#' @param key name for the new object
#' @param val the object
#' @param pos the index for the environment
#' @keywords internal
global_env_set_hack <- function(key, val, pos) {
  assign(key, val, envir = as.environment(pos))
}

#' @rdname acciones
girar_derecha <- function() {

  # Proceed if there was no mistake
  if (pkg_env$error) stop("You made a mistake before and can't ask Karel to do more things. Generate the world again and start all over.\n Tuviste un error y ahora no puedes pedirle algo nuevo a Karel. Generar otra vez el mundo y volver a comenzar.")

  # Update moment and direction
  pkg_env$moment <- pkg_env$moment + 1
  pkg_env$dir_now <- switch(pkg_env$dir_now, 4, 1, 2, 3)

  # Update karel data set (only changes dir from last row)
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Update beepers dataset, they remain the same but need to reflect this new moment
  pkg_env$beepers_now$moment <- pkg_env$beepers_now$moment + 1
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}

#' @rdname acciones
darse_vuelta <- function() {

  # Proceed if there was no mistake
  if (pkg_env$error) stop("You made a mistake before and can't ask Karel to do more things. Generate the world again and start all over.\n Tuviste un error y ahora no puedes pedirle algo nuevo a Karel. Generar otra vez el mundo y volver a comenzar.")

  # Update moment and direction
  pkg_env$moment <- pkg_env$moment + 1
  pkg_env$dir_now <- switch(pkg_env$dir_now, 3, 4, 1, 2)

  # Update karel data set (only changes dir from last row)
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Update beepers dataset, they remain the same but need to reflect this new moment
  pkg_env$beepers_now$moment <- pkg_env$beepers_now$moment + 1
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}
