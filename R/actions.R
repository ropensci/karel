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
avanzar <- function() {
  # Proceed if there was no mistake
  if (pkg_env$error) stop("You made a mistake before and can't ask Karel to do more things. Generate the world again and start all over.\n Tuviste un error y ahora no puedes pedirle algo nuevo a Karel. Generar otra vez el mundo y volver a comenzar.")
  pkg_env$moment <- pkg_env$moment + 1
  switch(pkg_env$dir_now,

         # Current direction: east
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 1]) {
           pkg_env$x_now <- pkg_env$x_now + 1
         } else {
           pkg_env$error <- TRUE
           stop("Can't move east, there's a wall. Generate again the world and start all over.\nNo puede avanzar hacia el este, hay una pared. Generar otra vez el mundo y volver a comenzar.")
         },

         # Current direction: north
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 2]) {
           pkg_env$y_now <- pkg_env$y_now + 1
         } else {
           pkg_env$error <- TRUE
           stop("Can't move north, there's a wall. Generate again the world and start all over.\nNo puede avanzar hacia el norte, hay una pared. Generar otra vez el mundo y volver a comenzar.")
         },

         # Current direction: west
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 3]) {
           pkg_env$x_now <- pkg_env$x_now - 1
         } else {
           pkg_env$error <- TRUE
           stop("Can't move west, there's a wall. Generate again the world and start all over.\nNo puede avanzar hacia el oeste, hay una pared. Generar otra vez el mundo y volver a comenzar.")
         },

         # Current direction: south
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 4]) {
           pkg_env$y_now <- pkg_env$y_now - 1
         } else {
           pkg_env$error <- TRUE
           stop("Can't move south, there's a wall. Generate again the world and start all over.\nNo puede avanzar hacia el sur, hay una pared. Generar otra vez el mundo y volver a comenzar.")
         }
  )
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Even though moving doesn't change the amount of beepers, for the animation I
  # need to declare that in this new moment, beepers remain the same.
  pkg_env$beepers_now <- mutate(pkg_env$beepers_now, moment = moment + 1)
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}

#' @rdname acciones
#' @export
girar_izquierda <- function() {
  # Proceed if there was no mistake
  if (pkg_env$error) stop("You made a mistake before and can't ask Karel to do more things. Generate the world again and start all over.\n Tuviste un error y ahora no puedes pedirle algo nuevo a Karel. Generar otra vez el mundo y volver a comenzar.")
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

#' @rdname acciones
#' @export
poner_coso <- function() {

  # Proceed if there was no mistake
  if (pkg_env$error) stop("You made a mistake before and can't ask Karel to do more things. Generate the world again and start all over.\n Tuviste un error y ahora no puedes pedirle algo nuevo a Karel. Generar otra vez el mundo y volver a comenzar.")

  if (pkg_env$beepers_bag == 0) {
    pkg_env$error <- TRUE
    stop("Can't put a beeper since there aren't any left in Karel's bag. Generate again the world and start all over.\nNo puede colocar un coso ya que no le queda ninguno en la mochila. Generar otra vez el mundo y volver a comenzar.")
  } else {

    # Update bag
    pkg_env$beepers_bag <- pkg_env$beepers_bag - 1
    # Update moment
    pkg_env$moment <- pkg_env$moment + 1

    if (pkg_env$beepers_any == 0) {
      # There are no beepers in the world now, create again the beepers_now dataset
      pkg_env$beepers_now <- tibble(x = pkg_env$x_now,
                                    y = pkg_env$y_now,
                                    cell = pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx,
                                    n = 1,
                                    moment = pkg_env$moment)
    } else {
      pkg_env$beepers_now$moment <- pkg_env$moment
      # There are beepers, but I have to see if there are already here or not
      if (hay_cosos()) {
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
    }
    pkg_env$beepers_any <- pkg_env$beepers_any + 1

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
}

#' @rdname acciones
#' @export
juntar_coso <- function() {

  # Proceed if there was no mistake
  if (pkg_env$error) stop("You made a mistake before and can't ask Karel to do more things. Generate the world again and start all over.\n Tuviste un error y ahora no puedes pedirle algo nuevo a Karel. Generar otra vez el mundo y volver a comenzar.")

  # We can only remove if there are no beepers there, otherwise it's an error
  if (hay_cosos()) {

    # Update beepers count
    pkg_env$beepers_any <- pkg_env$beepers_any - 1
    pkg_env$beepers_bag <- pkg_env$beepers_bag + 1

    # Update moment
    pkg_env$moment <- pkg_env$moment + 1
    pkg_env$beepers_now$moment <- pkg_env$moment

    # Get cell index and see if there are any beepers already there
    idx <- get_beepers_df_row()

    # Remove beeper
    pkg_env$beepers_now$n[idx] <- pkg_env$beepers_now$n[idx] - 1

    # Remove row from beepers dataset it there are no beepers left, so the box disappears
    if (pkg_env$beepers_now$n[idx] == 0) {
      pkg_env$beepers_now <- dplyr::slice(pkg_env$beepers_now, -idx)
      if (nrow(pkg_env$beepers_now) == 0) {
        # This means that in this moment there were no beepers in the world
        # In order the plot to work, I will create a dataset with NA values
        pkg_env$beepers_now[1, ] <- list(NA, NA, NA, NA, pkg_env$moment)
      }
    }

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
    pkg_env$error <- TRUE
    stop("There are no beepers here to remove. Generate again the world and start all over.\nNo hay cosos para quitar. Generar otra vez el mundo y volver a comenzar.")
  }
}
