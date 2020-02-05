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

colocar_beeper <- function() {

  pkg_env$moment <- pkg_env$moment + 1
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx

  if (cell %in% pkg_env$beepers_now$cell) {
    pkg_env$beepers_now$cell <- pkg_env$beepers_now$cell + 1 # ESTA MAL MODIFICAR SOLO CELL
  } else {
    pkg_env$beepers_now <-
      pkg_env$beepers_now %>%
      add_row(x = pkg_env$x_now, y = pkg_env$y_now, cell = cell, n = 1) %>%
      mutate(moment = pkg_env$moment)
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

probar1 <- function() return(pkg_env)
probar2 <- function() return(pkg_env$beepers_all)
