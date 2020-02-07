#' Title
#'
#' @return
#' @export
#'
#' @examples
avanzar <- function() {
  pkg_env$moment <- pkg_env$moment + 1
  switch(pkg_env$dir_now,
         # Current direction: east
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 1])
           pkg_env$x_now <- pkg_env$x_now + 1,
         # Current direction: north
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 2])
           pkg_env$y_now <- pkg_env$y_now + 1,
         # Current direction: west
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 3])
           pkg_env$x_now <- pkg_env$x_now - 1,
         # Current direction: south
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 4])
           pkg_env$y_now <- pkg_env$y_now - 1
  )
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Even though moving doesn't change the amount of beepers, for the animation I
  # need to declare that in this new moment, beepers remain the same.
  pkg_env$beepers_now <- mutate(pkg_env$beepers_now, moment = moment + 1)
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
colocar_beeper <- function() {

  # Update moment
  pkg_env$moment <- pkg_env$moment + 1
  pkg_env$beepers_now$moment <- pkg_env$moment

  if (hay_beepers()) {
    # If there are beepers, add one more
    idx <- get_beepers_df_row()
    pkg_env$beepers_now$n[idx] <- pkg_env$beepers_now$n[idx] + 1
  } else {
    # If there arent any, add new row with one beeper to beepers dataset
    pkg_env$beepers_now <- add_row(pkg_env$beepers_now,
                                   x = pkg_env$x_now, y = pkg_env$y_now,
                                   cell = pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx,
                                   n = 1, moment = pkg_env$moment)
  }

  # Append this new state of beepers to beepers_all
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)

  # Even though putting beepers doesn't change karel position or direction I
  # need to add a row to karel dataset for the animation with this new moment
  pkg_env$karel <-
    pkg_env$karel %>%
    slice(n()) %>%
    mutate(moment = moment + 1) %>%
    bind_rows(pkg_env$karel, .)
}


get_beepers_df_row <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  cell_present <- which(pkg_env$beepers_now$cell == cell)
  return(cell_present)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
quitar_beeper <- function() {

  # We can only remove if there are no beepers there, otherwise it's an error
  if (hay_beepers()) {

    # Update moment
    pkg_env$moment <- pkg_env$moment + 1
    pkg_env$beepers_now$moment <- pkg_env$moment

    # Get cell index and see if there are any beepers already there
    idx <- get_beepers_df_row()

    # Remove beeper
    pkg_env$beepers_now$n[idx] <- pkg_env$beepers_now$n[idx] - 1

    # Append this new state of beepers to beepers_all
    pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)

    # Even though removing beepers doesn't change karel position or direction I
    # need to add a row to karel dataset for the animation with this new moment
    pkg_env$karel <-
      pkg_env$karel %>%
      slice(n()) %>%
      mutate(moment = moment + 1) %>%
      bind_rows(pkg_env$karel, .)
  } else {
    stop("No hay beepers para quitar")
  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
girar_izquierda <- function() {
  # Update moment and direction
  pkg_env$moment <- pkg_env$moment + 1
  pkg_env$dir_now <- switch(pkg_env$dir_now, 2, 3, 4, 1)

  # Update karel data set (only changes dir from last row)
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Update beepers dataset, they remain the same but need to reflect this new moment
  pkg_env$beepers_now$moment <- pkg_env$beepers_now$moment + 1
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
probar1 <- function() return(pkg_env)

#' Title
#'
#' @return
#' @export
#'
#' @examples
probar2 <- function() return(pkg_env$beepers_all)
