#' Title
#'
#'
#' @return
#'
#' @examples
girar_derecha <- function() {
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

#' Title
#'
#'
#' @return
#'
#' @examples
darse_vuelta <- function() {
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


#' Title
#'
#' Con esta queda visible en el globa env la funcion llamada funcioninterna y anda
#'
#' @return
#'
#' @export
#' @examples
cargar_super_karel <- function() {
  # These do what I want but they get me the note about making assignments to
  # the global environment
  # assign("girar_derecha", girar_derecha, envir = .GlobalEnv)
  # assign("darse_vuelta", darse_vuelta, envir = .GlobalEnv)

  # This hack solves the note
  global_env_set_hack("girar_derecha", girar_derecha, 1L)
  global_env_set_hack("darse_vuelta", darse_vuelta, 1L)
}

global_env_set_hack <- function(key, val, pos) {
  assign(key, val, envir = as.environment(pos))
}
