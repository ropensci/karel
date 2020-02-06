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

  # Update moment
  pkg_env$moment <- pkg_env$moment + 1
  pkg_env$beepers_now$moment <- pkg_env$moment

  # Get cell index and see if there are any beepers already there
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  cell_present <- which(pkg_env$beepers_now$cell == cell)

  if (length(cell_present) > 0) {
    # If there are beepers, add one more
    pkg_env$beepers_now$n[cell_present] <- pkg_env$beepers_now$n[cell_present] + 1
  } else {
    # If there arent any, add new row with one beeper to beepers dataset
    pkg_env$beepers_now <- add_row(pkg_env$beepers_now,
                                   x = pkg_env$x_now, y = pkg_env$y_now,
                                   cell = cell, n = 1, moment = pkg_env$moment)
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

# hasta aca bien

get_beepers_df_row <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  cell_present <- which(pkg_env$beepers_now$cell == cell)
  return()
}


quitar_beeper <- function() {

  # We can only remove if there are no beepers there, otherwise it's an error
  if (beepers_presentes()) {
    # Update moment
    pkg_env$moment <- pkg_env$moment + 1
    pkg_env$beepers_now$moment <- pkg_env$moment

    # Get cell index and see if there are any beepers already there
    cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
    cell_present <- which(pkg_env$beepers_now$cell == cell)

    # Remove beeper
    pkg_env$beepers_now$n[cell_present] <- pkg_env$beepers_now$n[cell_present] + 1
  }





  if (length(cell_present) > 0) {
    # If there are beepers, add one more
    pkg_env$beepers_now$n[cell_present] <- pkg_env$beepers_now$n[cell_present] + 1
  } else {
    # If there arent any, add new row with one beeper to beepers dataset
    pkg_env$beepers_now <- add_row(pkg_env$beepers_now,
                                   x = pkg_env$x_now, y = pkg_env$y_now,
                                   cell = cell, n = 1, moment = pkg_env$moment)
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
