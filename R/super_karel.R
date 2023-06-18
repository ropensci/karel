#' Turn on Karel's superpowers
#'
#' After running \code{.load_super_karel()}, Karel can also turn right and turn
#' around with \code{.turn_right()} and \code{.turn_around()}. If these
#' superpowers aren't loaded, then these functions won't be available and Karel
#' can't use them. This is an internal function, and its called by language
#' wrappers.
#'
#' @keywords internal
#'
.load_super_karel <- function() {
  # These do what I want but they get me the note about making assignments to
  # the global environment
  # assign("girar_derecha", girar_derecha, envir = .GlobalEnv)
  # assign("darse_vuelta", darse_vuelta, envir = .GlobalEnv)

  # This hack solves the note
  global_env_set_hack("girar_derecha", girar_derecha, 1L)
  global_env_set_hack("darse_vuelta", darse_vuelta, 1L)
  global_env_set_hack("turn_right", turn_right, 1L)
  global_env_set_hack("turn_around", turn_around, 1L)
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

#' Implementation of the action of turning right
#'
#' @keywords internal
#'
.turn_right <- function(lang) {

  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }

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

#' Implementation of the action of turning around
#'
#' @keywords internal
#'
.turn_around <- function(lang) {

  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }

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
